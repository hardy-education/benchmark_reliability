# ==============================================================================
# 04_item_metrics.R -- item-level reliability and discrimination metrics.
#
# These are the candidate item-selection criteria compared against lambda*_6 in
# App. D.8: classical internal-consistency contributions (alpha-drop, corrected
# item-total correlation, Guttman lambda_6 from `psych`), the Mokken
# nonparametric scalability coefficients H_i and Z_i, the Molenaar-Sijtsma
# single-item reliability, and factor-analytic loadings.
#
# Each function takes a response matrix (long or wide) and returns one row per
# item with an `item` key, so the bootstrap loop can left_join them together in
# any order. That contract is the reason they all start with
# `as_response_matrix()`.
# ==============================================================================

# ------------------------------------------------------------------------------
# Classical internal consistency
# ------------------------------------------------------------------------------

#' Cronbach's alpha and per-item alpha-drop statistics
#'
#' Wraps `psych::alpha()` and renames its output to the names used throughout
#' the pipeline. The renaming is the point: `psych` returns `r.cor`,
#' `raw_alpha`, `std.alpha`, `G6(smc)` and `S/N`, which appear under four
#' different spellings across the original scripts.
#'
#' @param wide Response matrix.
#' @param na_replace Value substituted for missing responses. Benchmarks record
#'   an unattempted item as missing; scoring it 0 treats "not attempted" as
#'   "wrong", which is the convention the leaderboards themselves use.
#' @return One row per item: `alpha_rest` (alpha with the item dropped),
#'   `ctt_disc` (corrected item-total correlation), `g6_rest`, `snr_alpha`.
item_alpha <- function(wide, na_replace = 0) {
  wide <- as_response_matrix(wide)
  a <- psych::alpha(
    item_block(wide) |> naniar::replace_na_with(na_replace),
    check.keys = FALSE,
    warnings   = FALSE,
    use        = "pairwise.complete.obs"
  )
  dplyr::inner_join(
    tibble::as_tibble(a$item.stats, rownames = "item"),
    tibble::as_tibble(a$alpha.drop, rownames = "item"),
    by = "item"
  ) |>
    janitor::clean_names() |>
    dplyr::rename(
      r_rest         = r_cor,
      alpha_rest     = raw_alpha,
      alpha_rest_std = std_alpha,
      g6_rest        = g6_smc,
      snr_alpha      = s_n,
      valid_n        = n
    )
}

#' Bifactor / omega-hierarchical loadings per item
#'
#' Schmid-Leiman loadings from `psych::omegah()`. Reported in App. D.8 as one of
#' the factor-analytic comparators for item selection.
#'
#' @param wide Response matrix.
#' @param n_factors Number of group factors.
#' @param poly Use polychoric correlations, appropriate for binary scores.
#' @param na_replace Missing-response substitution, as in `item_alpha()`.
item_omega <- function(wide, n_factors = 3, poly = TRUE, na_replace = 0,
                       fm = "minres", rotate = "oblimin", ...) {
  wide <- as_response_matrix(wide)
  tic(glue("omega ({n_factors} factors)"))
  om <- psych::omegah(
    item_block(wide) |> naniar::replace_na_with(na_replace),
    nfactors = n_factors,
    flip     = FALSE,
    poly     = poly,
    plot     = FALSE,
    digits   = 8,
    fm       = fm,
    use      = "pairwise.complete.obs",
    rotate   = rotate,
    ...
  )
  toc()
  om
}

#' Per-item Schmid-Leiman loadings, suffixed by factor count
omega_loadings <- function(om, n_factors) {
  sl <- tibble::as_tibble(om$schmid$sl, rownames = "item") |> janitor::clean_names()
  names(sl) <- paste0(names(sl), "_", n_factors)
  dplyr::rename(sl, item = paste0("item_", n_factors))
}

#' The item correlation matrix implied by an omega fit, in long form
omega_correlations <- function(om) {
  tibble::as_tibble(om$R, rownames = "idx") |>
    tidyr::pivot_longer(-idx, names_to = "item", values_to = "rho")
}

#' Principal-component loadings per item
#'
#' @param wide Response matrix.
#' @param n_factors Number of components retained.
item_pc_loadings <- function(wide, n_factors = 3, rotate = "oblimin", ...) {
  wide <- as_response_matrix(wide)
  pa <- parameters::principal_components(
    item_block(wide),
    n        = n_factors,
    use      = "pairwise.complete.obs",
    rotation = rotate,
    ...
  )
  tibble::tibble(pa) |>
    janitor::clean_names() |>
    dplyr::rename(item = variable) |>
    tidyr::pivot_longer(-item, names_to = "metric", values_to = "val") |>
    dplyr::mutate(metric = paste0(metric, "_", n_factors)) |>
    tidyr::pivot_wider(names_from = metric, values_from = val)
}

# ------------------------------------------------------------------------------
# Distribution of an item's correlations with every other item
# ------------------------------------------------------------------------------

#' Summarise each item's inter-item correlation distribution
#'
#' An item's usefulness depends less on its mean correlation with the others
#' than on whether it correlates *negatively* with any of them, so the summary
#' keeps the tails: `cneg` and `clow` are the share of correlations below 0 and
#' below 0.1, and `c1`...`c99` are quantiles.
#'
#' @param cor_long Long correlation frame with `item` and `rho`.
item_correlation_summary <- function(cor_long) {
  probs <- c(c1 = 0.01, c5 = 0.05, c10 = 0.10, c25 = 0.25,
             c75 = 0.75, c90 = 0.90, c95 = 0.95, c99 = 0.99)
  cor_long |>
    dplyr::group_by(item) |>
    dplyr::summarise(
      cneg   = mean(rho < 0, na.rm = TRUE),
      clow   = mean(rho < 0.1, na.rm = TRUE),
      !!!purrr::imap(probs, function(p, nm) {
        rlang::quo(stats::quantile(rho, !!p, na.rm = TRUE))
      }),
      cmax   = max(rho, na.rm = TRUE),
      cmin   = min(rho, na.rm = TRUE),
      cmean  = mean(rho, na.rm = TRUE),
      cvar   = stats::var(rho, na.rm = TRUE),
      cse    = stats::sd(rho, na.rm = TRUE) / sqrt(dplyr::n()),
      cmedse = stats::mad(rho, na.rm = TRUE) / sqrt(dplyr::n()),
      cmed   = stats::median(rho, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Pearson inter-item correlations of a response matrix, in long form
item_correlations_long <- function(wide) {
  wide <- as_response_matrix(wide)
  stats::cor(item_block(wide)) |>
    tibble::as_tibble(rownames = "item") |>
    tidyr::pivot_longer(-item, names_to = "itemx", values_to = "rho") |>
    dplyr::filter(item != itemx)
}

#' Join two per-item metric tables, reconciling numeric vs prefixed item ids
join_item_metrics <- function(x, y) {
  fix <- function(d) {
    if (is.numeric(d$item)) d$item <- paste0("i", d$item)
    d
  }
  dplyr::inner_join(fix(x), fix(y), by = "item")
}

# ------------------------------------------------------------------------------
# Mokken nonparametric scalability
# ------------------------------------------------------------------------------

#' Mokken scalability coefficients H_i and Z_i per item
#'
#' Loevinger's H_i (App. D.8.2) is the observed item covariance over its maximum
#' given the marginals; Z_i is the corresponding test statistic. Both come from
#' the `mokken` package. Returns the scale-level `H` and `Z` alongside, so a
#' single row carries both the item and its context.
#'
#' @param wide Response matrix.
item_mokken_hz <- function(wide) {
  wide <- as_response_matrix(wide)
  x <- item_block(wide) |> dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  hi <- mokken::coefH(x, se = FALSE, results = FALSE)
  zi <- mokken::coefZ(x)

  dplyr::bind_cols(
    tibble::as_tibble(hi$Hi, rownames = "item") |> dplyr::rename(Hi = value),
    tibble::as_tibble(zi$Zi) |> dplyr::rename(Zi = value)
  ) |>
    dplyr::mutate(h = hi$H, z = zi$Z)
}

#' Molenaar-Sijtsma single-item reliability
#'
#' Wraps `single_mokken()` (R/04b_mokken_single.R) and returns NA for the whole
#' matrix rather than failing when the nonparametric estimate is undefined,
#' which happens when an item has no off-boundary ISRFs.
#'
#' @param wide Response matrix.
item_ms <- function(wide) {
  wide <- as_response_matrix(wide)
  x <- item_block(wide)
  # The original `MS_est()` called single_mokken() once, discarded the result,
  # and then called it again inside tryCatch -- doubling the cost of the single
  # most expensive item metric in the bootstrap loop. Called once here.
  tryCatch(
    tibble::tibble(
      MS   = collapse::replace_inf(single_mokken(as.matrix(x))),
      item = colnames(x)
    ),
    error = function(e) {
      warning(glue("Molenaar-Sijtsma estimate failed: {conditionMessage(e)}"))
      tibble::tibble(MS = NA_real_, item = colnames(x))
    }
  )
}
