# ==============================================================================
# 03_lambda_star.R -- lambda*_6, the slope-activated item-level reliability
#                     proxy for the M << I regime, plus classical Guttman
#                     lambda_6 for comparison.
#
# Paper: Sec. 3.3 and App. D. This is the novel estimator, and in the original
# code it existed in three places at once --
#
#   lambda6.R                 L6_est()                      (5 return columns)
#   lambda_star_full.R        L6_est()                      (14 return columns,
#                                                            plus a driver loop)
#   metric_bootstraps.R       estimate_lambda6_components() (13 return columns,
#                                                            `r2` not `L6`)
#
# -- identical regression, three different sets of outputs under two different
# function names, and a fourth long-form variant (`feols_estimations()`) that
# fitted the same model via `split = ~item` on a pre-melted frame. There is now
# one implementation. The column naming follows the paper: L_6^*(i) per item
# (Eq. 26), aggregated to lambda*_6 (Eq. 13).
# ==============================================================================

# ------------------------------------------------------------------------------
# Identification (paper Prop. D.1)
# ------------------------------------------------------------------------------

#' Check identification of the slope-activated design
#'
#' The two-way fixed effects of Eq. (12) are identified only up to one additive
#' constant per connected component of the *activated* bipartite graph
#' `G = ({models}, {other items}, {(m, j) : S_mj = 1})` (Prop. D.1). When that
#' graph splits into several components the R-squared is still computed, but the
#' effects are only identified within components and the estimate is not
#' comparable across items.
#'
#' The original code asserted this condition in the paper and never tested it,
#' so a disconnected benchmark would have produced quietly incomparable
#' L*_6(i) values. This function makes the condition observable.
#'
#' @param scores Numeric matrix of other-item scores, models x (items - 1).
#' @return List with `n_components`, `n_edges`, `connected`, and
#'   `largest_component_share`.
check_activation_connectivity <- function(scores) {
  scores <- as.matrix(scores)
  activated <- scores > 0
  n_m <- nrow(activated)
  n_j <- ncol(activated)
  n_edges <- sum(activated)

  if (n_edges == 0L) {
    return(list(n_components = NA_integer_, n_edges = 0L,
                connected = FALSE, largest_component_share = 0))
  }

  # Union-find over (n_m + n_j) nodes: models are 1..n_m, other-items are
  # n_m + 1 .. n_m + n_j. Each activated cell unions its model with its item.
  parent <- seq_len(n_m + n_j)
  find <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]  # path halving
      x <- parent[x]
    }
    x
  }
  union <- function(a, b) {
    ra <- find(a); rb <- find(b)
    if (ra != rb) parent[ra] <<- rb
  }

  idx <- which(activated, arr.ind = TRUE)
  for (k in seq_len(nrow(idx))) union(idx[k, 1L], n_m + idx[k, 2L])

  # Only nodes that carry at least one activated edge participate.
  touched <- c(
    which(rowSums(activated) > 0L),
    n_m + which(colSums(activated) > 0L)
  )
  roots <- vapply(touched, find, integer(1))
  sizes <- table(roots)

  list(
    n_components = length(sizes),
    n_edges = n_edges,
    connected = length(sizes) == 1L,
    largest_component_share = max(sizes) / length(touched)
  )
}

# ------------------------------------------------------------------------------
# lambda*_6
# ------------------------------------------------------------------------------

#' Per-item L*_6 components via slope-activated two-way fixed effects
#'
#' For each target item `i`, builds the long-form design of Eq. (11) --- one row
#' per (model, other-item) pair, with the target item's score replicated as the
#' outcome and the other item's score as the activating regressor --- and fits
#' Eq. (12):
#'
#'   Y^(i)_mj = alpha_i + S_mj * gamma^(i)_j + S_mj * delta^(i)_m + eta^(i)_mj
#'
#' In `fixest` syntax the slope-activated fixed effects are
#' `itemx[[otheritemscore]] + model[[otheritemscore]]`, which is what makes the
#' effects contribute only where `S_mj != 0`. `L*_6(i)` is the R-squared of that
#' fit (Eq. 26).
#'
#' The estimator's reason for existing is the `M << I` regime: it never forms an
#' unrestricted `(I - 1)`-dimensional coefficient vector, so the effective
#' sample size is the number of activated edges rather than the number of
#' models, and no item has to be dropped or shrunk (App. D.3.2).
#'
#' Memory note (carried over from the original, which got this right): the long
#' design is built one target item at a time rather than once for all items,
#' which keeps peak memory at `M x (I - 1)` rows instead of `M x I x (I - 1)`.
#'
#' @param wide Wide response matrix: one `model` (or `id` / `subject`) column and
#'   one column per item. Coercible to a data.table.
#' @param n_workers Parallel workers. Defaults to one fewer than available cores.
#' @param check_connectivity Verify the Prop. D.1 identification condition per
#'   item. Adds a union-find pass per item; cheap next to the regression.
#' @param extra_r2 Return the full family of `fixest` R-squared variants
#'   (`ar2`, `wr2`, `pr2`, ...) in addition to `L6`. The pipeline uses `war2`
#'   for the pseudo-lambda6 uniqueness transform in 20_adjust_metrics.R.
#' @return data.table with one row per item: `item`, `nobs`, `L6`, `intercept`,
#'   `intercept_se`, and, if requested, connectivity and extra R-squared columns.
#' @examples
#' set.seed(1)
#' wide <- data.table::data.table(model = 1:40)
#' for (j in 1:12) wide[[paste0("item_", j)]] <- rbinom(40, 1, 0.5)
#' comps <- lambda_star_components(wide, n_workers = 1)
#' lambda_star(comps)            # benchmark-level lambda*_6
lambda_star_components <- function(wide,
                                   n_workers = NULL,
                                   check_connectivity = FALSE,
                                   extra_r2 = TRUE) {
  wide <- data.table::as.data.table(wide)

  id_cols   <- intersect(c("model", "id", "subject"), names(wide))
  if (length(id_cols) == 0L) {
    stop("`wide` must contain a 'model', 'id', or 'subject' identifier column.")
  }
  id_col    <- id_cols[1L]
  if (!identical(id_col, "model")) data.table::setnames(wide, id_col, "model")
  item_cols <- setdiff(names(wide), c("model", "id", "subject"))

  if (length(item_cols) < 3L) {
    stop("lambda*_6 needs at least 3 items; got ", length(item_cols), ".")
  }

  message(glue(
    "lambda*_6: {length(item_cols)} items x {nrow(wide)} models."
  ))

  if (is.null(n_workers)) n_workers <- max(1L, future::availableCores(omit = 1L))
  # Restore whatever plan the caller had once we are done, instead of resetting
  # to sequential unconditionally as the original three copies all did -- that
  # silently de-parallelised every later stage of the bootstrap loop.
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  br_plan(workers = n_workers)

  results <- future.apply::future_lapply(
    item_cols,
    function(target_item) {
      other_items <- setdiff(item_cols, target_item)

      # Eq. (11): replicate the target item across the other items.
      reg_data <- data.table::melt(
        wide,
        id.vars        = "model",
        measure.vars   = other_items,
        variable.name  = "itemx",
        value.name     = "otheritemscore",
        variable.factor = FALSE
      )
      reg_data[, resp := wide[reg_data, on = "model", get(target_item)]]

      # Eq. (12). Standard errors are clustered by model to address the
      # within-model dependence across j (App. D.3.3).
      fit <- try(
        fixest::feols(
          fml = resp ~ 1 | itemx[[otheritemscore]] + model[[otheritemscore]],
          data     = reg_data,
          warn     = FALSE,
          notes    = FALSE,
          verbose  = 0,
          cluster  = ~model,
          panel.id = ~ model + itemx
        ),
        silent = TRUE
      )
      if (inherits(fit, "try-error")) {
        return(data.table::data.table(item = target_item, nobs = NA_integer_,
                                      L6 = NA_real_))
      }

      r2_vals <- as.list(fixest::r2(fit, type = "all"))
      ct      <- fixest::coeftable(fit)

      out <- data.table::data.table(
        item         = target_item,
        nobs         = fit$nobs,
        L6           = r2_vals$r2,             # L*_6(i), Eq. (26)
        meanscores   = mean(fit$scores, na.rm = TRUE),
        meanobsr2    = mean(fit$scores^2, na.rm = TRUE),
        intercept    = ct["(Intercept)", "Estimate"],
        intercept_se = ct["(Intercept)", "Std. Error"]
      )
      # `set()` rather than `out[[nm]] <- ...`: the base-R form copies the whole
      # data.table and triggers data.table's shallow-copy warning once per item.
      if (extra_r2) {
        for (nm in c("ar2", "wr2", "war2", "cor2", "wapr2", "wpr2", "pr2", "apr2")) {
          data.table::set(out, j = nm,
                          value = if (is.null(r2_vals[[nm]])) NA_real_ else r2_vals[[nm]])
        }
      }
      if (check_connectivity) {
        conn <- check_activation_connectivity(as.matrix(wide[, ..other_items]))
        data.table::set(out, j = "n_components",    value = conn$n_components)
        data.table::set(out, j = "n_edges",         value = conn$n_edges)
        data.table::set(out, j = "identified",      value = conn$connected)
        data.table::set(out, j = "largest_comp_sh", value = conn$largest_component_share)
      }
      out[]
    },
    future.seed     = TRUE,
    future.packages = c("data.table", "fixest")
  )

  out <- data.table::rbindlist(results, fill = TRUE)

  if (check_connectivity && "identified" %in% names(out)) {
    n_bad <- sum(!out$identified, na.rm = TRUE)
    if (n_bad > 0L) {
      warning(glue(
        "{n_bad} of {nrow(out)} items have a disconnected activated design ",
        "(Prop. D.1); their L*_6 values are identified only within components."
      ))
    }
  }

  message("lambda*_6 estimation complete.")
  out
}

#' Benchmark-level lambda*_6 from its per-item components
#'
#' Aggregation step of Eq. (13). The paper allows either the mean or a trimmed
#' mean; the trimmed mean is the safer default for benchmarks with a handful of
#' degenerate items, so the trim fraction is exposed rather than hard-coded.
#'
#' @param components Output of `lambda_star_components()`.
#' @param trim Fraction trimmed from each tail before averaging. `0` gives the
#'   plain mean.
lambda_star <- function(components, trim = 0) {
  mean(components$L6, trim = trim, na.rm = TRUE)
}

# ------------------------------------------------------------------------------
# Classical Guttman lambda_6, for the comparison in Fig. 4
# ------------------------------------------------------------------------------

#' Classical Guttman's lambda_6
#'
#' Implements Eqs. (22)-(24): `1 - sum(sigma^2_e,i) / Var(sum_i X_i)`, with the
#' item residual variances taken from the squared multiple correlations of each
#' item on all the others.
#'
#' This is the estimator that degenerates when `M << I` -- with fewer models
#' than items the design is rank-deficient and every in-sample R-squared goes to
#' 1, driving lambda_6 to 1 regardless of the data. That failure is the entire
#' motivation for `lambda_star_components()`, so the function warns rather than
#' returning a confident-looking number.
#'
#' @param wide Wide response matrix with a `model` column.
#' @return List with `lambda6` and the per-item squared multiple correlations.
guttman_lambda6 <- function(wide) {
  X <- as.matrix(
    data.table::as.data.table(wide)[, setdiff(names(wide), c("model", "id", "subject")),
                                    with = FALSE]
  )
  n_models <- nrow(X)
  n_items  <- ncol(X)

  if (n_models <= n_items) {
    warning(glue(
      "Guttman's lambda_6 is ill-posed here: {n_models} models for {n_items} ",
      "items (M <= I), so the item-wise SMCs saturate at 1. This is the ",
      "degeneracy lambda*_6 is designed to avoid (paper Sec. 3.3)."
    ))
  }

  R <- stats::cor(X)
  # SMC_i = 1 - 1 / (R^-1)_ii, the standard identity for the squared multiple
  # correlation of item i on all the others.
  Rinv <- try(solve(R), silent = TRUE)
  if (inherits(Rinv, "try-error")) {
    # Singularity is the M <= I degeneracy made explicit, so it is reported as
    # a message rather than a second warning on top of the one above.
    message("Item correlation matrix is singular; lambda_6 is undefined.")
    return(list(lambda6 = NA_real_, smc = rep(NA_real_, n_items)))
  }
  smc <- 1 - 1 / diag(Rinv)

  item_var  <- apply(X, 2, stats::var)
  resid_var <- item_var * (1 - smc)
  total_var <- stats::var(rowSums(X))

  list(
    lambda6 = if (total_var > 0) 1 - sum(resid_var) / total_var else NA_real_,
    smc     = stats::setNames(smc, colnames(X))
  )
}
