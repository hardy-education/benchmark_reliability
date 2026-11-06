# Empirical reliability as items are added in a chosen order.
#
# For an item order o and every prefix length n = 2, ..., N, take the measured
# population's responses on the first n items, estimate the crossed variance
# components afresh, and report the empirical G(n) and Phi(n) of that subset
# (paper Figs. 5, 7, 8). This is *not* a D-study projection: each point is a new
# variance decomposition of the actual selected subset.
#
# The legacy code fitted lme4::lmer once per (benchmark, method, population, n)
# on a ~100-point grid of n. For a complete matrix the prefix sums of squares
# can all be read off one cumulative-sum pass:
#
#   r_m(n) = sum of model m's responses on the first n items  (row cumsums)
#   SS_model(n) = sum_m r_m(n)^2 / n - S(n)^2 / (M n)
#   SS_item(n)  = sum_{j<=n} c_j^2 / M - S(n)^2 / (M n)
#   SS_total(n) = sum_{j<=n} sum_m x_mj^2 - S(n)^2 / (M n)
#
# so every prefix of a 429 x 12,032 benchmark costs one O(M N) sweep, and the
# REML estimates follow from [.balanced_reml()]. Kendall's tau between each
# model's prefix mean and its full-benchmark mean (the empirical rank-stability
# curve) comes from the same cumulative sums.

#' Empirical reliability curve for one item order
#'
#' @param X Measured-population matrix, models x items.
#' @param order Item names in selection order. Items of `X` not in `order` are
#'   appended in random order (they were unscored in the reference population).
#' @param steps Prefix lengths to report. `NULL` = every n from 2 to N.
#' @param kendall Also compute Kendall's tau between prefix means and full
#'   means (needs `pcaPP` for speed; falls back to `stats::cor`).
#' @param engine `"auto"`: closed form for a complete matrix, `lmer` per step
#'   otherwise. `"lmer"` forces the legacy per-step fit.
#' @return `data.table(n_items, var_model, var_item, var_residual, engine,
#'   status, kendall, g_rel, g_abs, snr_rel)`. `status` is as in
#'   [variance_components()]; prefixes with no regular REML fit (zero
#'   residual, e.g. a handful of models answering every selected item
#'   identically) have NA components and NA reliability.
reliability_curve <- function(X, order, steps = NULL, kendall = TRUE, engine = c("auto", "lmer")) {
  engine <- match.arg(engine)
  .check_matrix(X)
  extra <- setdiff(colnames(X), order)
  order <- c(intersect(order, colnames(X)), if (length(extra)) sample(extra))
  Xo <- X[, order, drop = FALSE]
  M <- nrow(Xo); N <- ncol(Xo)
  steps <- sort(unique(steps %||% seq.int(2L, N)))
  steps <- steps[steps >= 2 & steps <= N]

  if (engine == "auto" && !anyNA(Xo)) {
    # Uncentred prefix totals are exact for 0/1 data, so tied models stay tied
    # (Kendall's tau-b depends on ties) and the prefix sums of squares are
    # exact integers (see .crossed_ss()): no cancellation, no slightly
    # negative residuals, and a zero residual is detected exactly.
    Ru <- matrixStats::rowCumsums(Xo)          # M x N prefix totals per model
    n <- steps
    exact <- .is_integer_valued(Xo)
    if (exact) {
      ss <- .crossed_ss(sx2 = cumsum(colSums(Xo^2))[n], sr2 = colSums(Ru^2)[n],
                        sc2 = cumsum(colSums(Xo)^2)[n], tot = colSums(Ru)[n], M = M, I = n, exact = TRUE)
      exact <- ss$exact
    }
    if (!exact) {
      # Non-integer data: centre first so the subtraction is well conditioned,
      # clamp at zero, and treat residuals below 1e-10 of the total as zero.
      mu <- mean(Xo)
      R <- Ru - rep(seq_len(N) * mu, each = M)
      Xc <- Xo - mu
      S <- colSums(R)[n]
      corr <- S^2 / (M * n)
      ssm <- colSums(R^2)[n] / n - corr
      ssi <- cumsum(colSums(Xc)^2)[n] / M - corr
      ssr <- (cumsum(colSums(Xc^2))[n] - corr) - ssm - ssi
      ss <- list(ssm = pmax(ssm, 0), ssi = pmax(ssi, 0), ssr = pmax(ssr, 0))
    }
    vc <- .balanced_reml(ss$ssm, ss$ssi, ss$ssr, M, n, tol = if (exact) 0 else 1e-10)
    out <- data.table::data.table(n_items = n, var_model = vc$var_model, var_item = vc$var_item,
                                  var_residual = vc$var_residual, engine = "anova", status = vc$status)
    if (kendall) out[, kendall := .prefix_kendall(Ru[, n, drop = FALSE], Ru[, N])]
  } else {
    out <- data.table::rbindlist(lapply(steps, function(n) variance_components(Xo[, seq_len(n), drop = FALSE], engine = "lmer")))
    if (kendall) {
      full <- rowMeans(Xo, na.rm = TRUE)
      obs <- !is.na(Xo); Xz <- Xo; Xz[!obs] <- 0
      Rs <- matrixStats::rowCumsums(Xz)[, steps, drop = FALSE]
      Cn <- matrixStats::rowCumsums(obs * 1)[, steps, drop = FALSE]
      out[, kendall := .prefix_kendall(Rs / pmax(Cn, 1), full)]
    }
  }
  out[, `:=`(
    g_rel = var_model / (var_model + var_residual / n_items),
    g_abs = var_model / (var_model + (var_item + var_residual) / n_items),
    snr_rel = n_items * var_model / var_residual
  )]
  out[]
}

# Kendall's tau between each column of `P` (prefix scores; a common scale
# factor per column does not change ranks) and the vector `full`.
.prefix_kendall <- function(P, full) {
  ktau <- if (requireNamespace("pcaPP", quietly = TRUE)) pcaPP::cor.fk else function(x, y) stats::cor(x, y, method = "kendall")
  vapply(seq_len(ncol(P)), function(j) {
    x <- P[, j]
    if (stats::var(x) == 0) return(NA_real_)
    suppressWarnings(ktau(x, full))
  }, numeric(1))
}
