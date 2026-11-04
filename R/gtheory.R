# Generalizability theory for a crossed model x item design (paper Sec. 3).
#
#   Y_mi = mu + u_m + v_i + e_mi,   Var(Y) = s2_M + s2_I + s2_R      (Eq. 1)
#
#   relative reliability   G(n)   = s2_M / (s2_M + s2_R / n)          (Eq. 2)
#   absolute reliability   Phi(n) = s2_M / (s2_M + (s2_I + s2_R) / n) (Eq. 3)
#   SNR_rel = n s2_M / s2_R,  SNR_abs = n s2_M / (s2_I + s2_R)        (Eq. 4)
#
# Estimation. With one response per cell and no missing cells the design is
# balanced and the REML estimates have an exact closed form (docs/closed_forms.md):
#
#   * The REML likelihood factors into three independent strata: models
#     (df M-1, expected mean square l_M = s2_R + I s2_M), items (df I-1,
#     l_I = s2_R + M s2_I) and residual (df (M-1)(I-1), l_R = s2_R).
#   * Maximising it subject to s2_M, s2_I >= 0, i.e. l_R <= l_M and
#     l_R <= l_I, is a convex problem in the precisions 1/l. Its solution is
#     the tree-ordered isotonic fit of the mean squares with the residual
#     stratum at the root, found by pooling the residual with the smallest
#     violating stratum and re-checking the other (.balanced_reml()).
#   * When both moment estimates are negative the answer is usually NOT
#     "both zero": pooling the residual with the smaller stratum lowers the
#     residual, and the other component can come out positive.
#
# This matches lme4::lmer(resp ~ 1 + (1|item) + (1|model)) to optimiser
# tolerance, boundary cases included (tests/testthat/test-gtheory.R), at
# O(M * I) cost, which is what makes item-by-item reliability curves cheap.
#
# The closed form needs a complete matrix. With missing cells the row and
# column effects are no longer orthogonal, the cell counts per row and column
# differ, and the balanced expected mean squares do not hold, so dropping NAs
# from the sums of squares (na.rm = TRUE) is NOT a valid repair. The `"lmer"`
# engine fits the mixed model to the observed cells instead, and `"auto"`
# switches to it whenever X has an NA.

# ---------------------------------------------------------------------------
# Variance components
# ---------------------------------------------------------------------------

# Status codes returned with every estimate.
#   interior        all three moment estimates >= 0; REML = ANOVA estimates
#   boundary_model  s2_M on the zero boundary (models pooled into residual)
#   boundary_item   s2_I on the zero boundary (items pooled into residual)
#   boundary_both   both on the boundary (all three strata pooled)
#   degenerate      residual sum of squares is zero: the REML likelihood is
#                   unbounded as s2_R -> 0, so there is no regular maximiser
#   constant        the matrix is constant; nothing to decompose
#   too_small       fewer than 2 models or 2 items
REML_STATUS <- c("interior", "boundary_model", "boundary_item", "boundary_both",
                 "degenerate", "constant", "too_small")

# Recycle arguments to one common length; each must have length 1 or that length.
.recycle <- function(...) {
  args <- list(...)
  len <- lengths(args)
  n <- max(len)
  if (any(len != 1L & len != n)) stop("Arguments must have length 1 or a common length.", call. = FALSE)
  lapply(args, rep_len, length.out = n)
}

# Sums of squares of the crossed design from its sufficient statistics:
#   sx2 = sum of x^2, sr2 = sum over models of (row total)^2,
#   sc2 = sum over items of (column total)^2, tot = grand total.
#
#   M I SS_model    = M sr2 - tot^2
#   M I SS_item     = I sc2 - tot^2
#   M I SS_residual = M I sx2 - M sr2 - I sc2 + tot^2
#
# For integer data (0/1 responses) every term is an integer, and doubles hold
# integers exactly below 2^53, so the numerators are exact. The residual then
# carries no cancellation error: SS_residual = 0 exactly when the residual is
# zero and is never slightly negative. The caller passes `exact = FALSE` for
# non-integer data, and the result is clamped at zero.
.crossed_ss <- function(sx2, sr2, sc2, tot, M, I, exact) {
  MI <- M * I
  t2 <- tot^2
  num_m <- M * sr2 - t2
  num_i <- I * sc2 - t2
  num_r <- MI * sx2 - M * sr2 - I * sc2 + t2
  if (exact && max(abs(c(MI * sx2, M * sr2, I * sc2, t2)), na.rm = TRUE) >= 2^53)
    exact <- FALSE  # too large to be exact; fall through to clamping
  list(ssm = pmax(num_m, 0) / MI, ssi = pmax(num_i, 0) / MI, ssr = pmax(num_r, 0) / MI, exact = exact)
}

# Constrained REML for the balanced two-way crossed random-effects model,
# vectorised over any number of designs (all inputs are recycled to a common
# length). Inputs are sums of squares about the grand mean.
#
# `tol` is the relative size below which a residual sum of squares counts as
# zero; use 0 when the sums of squares are exact.
#
# Returns a list of component vectors plus `status` (see REML_STATUS).
# Degenerate, constant and too-small designs get NA components.
.balanced_reml <- function(ssm, ssi, ssr, M, I, tol = 0) {
  a <- .recycle(ssm = ssm, ssi = ssi, ssr = ssr, M = M, I = I)
  ssm <- a$ssm; ssi <- a$ssi; ssr <- a$ssr; M <- a$M; I <- a$I
  n <- length(ssm)
  dfm <- M - 1; dfi <- I - 1; dfr <- (M - 1) * (I - 1)
  msm <- ssm / dfm; msi <- ssi / dfi; msr <- ssr / dfr

  # Residual stratum variance l_R. Start from the unconstrained estimate and
  # pool where an order constraint l_R <= l_M or l_R <= l_I is violated.
  vr <- msr
  pool_m <- msm < msr
  pool_i <- msi < msr
  # Both violated: pool with the smaller mean square first, then re-check the
  # other against the lowered residual. (Pooling with the larger one first
  # can never be optimal: its pooled value exceeds the smaller mean square.)
  both <- pool_m & pool_i
  first_m <- both & msm <= msi
  first_i <- both & !first_m
  pmr <- (ssm + ssr) / (dfm + dfr)
  pir <- (ssi + ssr) / (dfi + dfr)
  pall <- (ssm + ssi + ssr) / (dfm + dfi + dfr)
  keep_i <- first_m & msi > pmr           # only models pooled after all
  keep_m <- first_i & msm > pir           # only items pooled after all
  # (A tie gives a zero component either way and is labelled boundary_both.)
  pool_m <- (pool_m & !pool_i) | first_m | (first_i & !keep_m)
  pool_i <- (pool_i & !both) | first_i | (first_m & !keep_i)
  vr <- ifelse(pool_m & pool_i, pall, ifelse(pool_m, pmr, ifelse(pool_i, pir, msr)))
  vm <- ifelse(pool_m, 0, (msm - vr) / I)
  vi <- ifelse(pool_i, 0, (msi - vr) / M)

  status <- ifelse(pool_m & pool_i, "boundary_both",
                   ifelse(pool_m, "boundary_model", ifelse(pool_i, "boundary_item", "interior")))
  sst <- ssm + ssi + ssr
  bad <- rep(NA_character_, n)
  bad[which(ssr <= tol * sst)] <- "degenerate"
  bad[which(sst <= 0)] <- "constant"
  bad[which(M < 2 | I < 2 | is.na(sst))] <- "too_small"
  is_bad <- !is.na(bad)
  status[is_bad] <- bad[is_bad]
  vm[is_bad] <- NA_real_; vi[is_bad] <- NA_real_; vr[is_bad] <- NA_real_
  list(var_model = vm, var_item = vi, var_residual = vr, status = status)
}

#' Variance components of a models x items response matrix
#'
#' @param X Numeric matrix, models in rows, items in columns.
#' @param engine `"auto"` uses the exact closed form when `X` is complete and
#'   `lmer` otherwise; `"anova"` forces the closed form (errors on NA, because
#'   the balanced formulas do not apply to incomplete data); `"lmer"` always
#'   fits `lme4::lmer` (the legacy behaviour).
#' @return One-row `data.table` with `var_model`, `var_item`, `var_residual`,
#'   `n_models`, `n_items`, `engine`, and `status`:
#'
#'   * `"interior"`, `"boundary_model"`, `"boundary_item"`,
#'     `"boundary_both"`: a regular REML fit, with the named components on the
#'     zero boundary.
#'   * `"degenerate"` (zero residual), `"constant"`, `"too_small"`: no regular
#'     REML fit exists, and the components are NA.
#'
#'   For `engine = "lmer"` the status is `"lmer"` or `"lmer_singular"`
#'   (a component on the boundary).
#' @examples
#' X <- matrix(rbinom(200, 1, 0.5), 20, dimnames = list(NULL, paste0("i", 1:10)))
#' variance_components(X)
variance_components <- function(X, engine = c("auto", "anova", "lmer")) {
  engine <- match.arg(engine)
  .check_matrix(X)
  if (engine == "auto") engine <- if (anyNA(X)) "lmer" else "anova"
  M <- nrow(X); I <- ncol(X)
  if (engine == "anova") {
    if (anyNA(X)) stop("engine = 'anova' needs a complete matrix: with missing cells the balanced ",
                       "REML formulas do not apply. Use engine = 'lmer' or handle_missing().", call. = FALSE)
    vc <- .anova_components(X)
  } else {
    vc <- .lmer_components(X)
  }
  data.table::data.table(var_model = vc$var_model, var_item = vc$var_item, var_residual = vc$var_residual,
                         n_models = M, n_items = I, engine = engine, status = vc$status)
}

# Closed-form REML for a complete matrix.
.anova_components <- function(X) {
  M <- nrow(X); I <- ncol(X)
  if (.is_integer_valued(X)) {
    rs <- rowSums(X); cs <- colSums(X)
    ss <- .crossed_ss(sum(X^2), sum(rs^2), sum(cs^2), sum(rs), M, I, exact = TRUE)
  } else {
    ss <- list(exact = FALSE)
  }
  if (!ss$exact) {
    # Non-integer data: compute the residual directly rather than by
    # subtraction, so a near-zero residual is not swamped by roundoff.
    Xc <- X - mean(X)
    rm <- rowMeans(Xc); cm <- colMeans(Xc)
    ss <- list(ssm = I * sum(rm^2), ssi = M * sum(cm^2),
               ssr = sum((Xc - outer(rm, cm, "+"))^2))
  }
  .balanced_reml(ss$ssm, ss$ssi, ss$ssr, M, I, tol = if (isTRUE(ss$exact)) 0 else 1e-12)
}

.is_integer_valued <- function(X) all(X == round(X))

# Fit resp ~ 1 + (1|item) + (1|model) by REML on the observed cells. This is
# the right estimator for incomplete (unbalanced) data.
.lmer_components <- function(X) {
  if (!requireNamespace("lme4", quietly = TRUE)) stop("Package 'lme4' is required for engine = 'lmer'.", call. = FALSE)
  obs <- which(!is.na(X), arr.ind = TRUE)
  d <- data.frame(resp = X[obs], model = factor(obs[, 1]), item = factor(obs[, 2]))
  fit <- suppressMessages(lme4::lmer(resp ~ 1 + (1 | item) + (1 | model), data = d,
                                     control = lme4::lmerControl(optimizer = "bobyqa", calc.derivs = FALSE,
                                                                 optCtrl = list(maxfun = 1e5))))
  vc <- as.data.frame(lme4::VarCorr(fit))
  list(var_model = vc$vcov[vc$grp == "model"], var_item = vc$vcov[vc$grp == "item"],
       var_residual = vc$vcov[vc$grp == "Residual"],
       status = if (lme4::isSingular(fit)) "lmer_singular" else "lmer")
}

# ---------------------------------------------------------------------------
# Coefficients
# ---------------------------------------------------------------------------

#' Reliability coefficients from variance components
#'
#' Adds the relative (G, Eq. 2) and absolute (Phi, Eq. 3) coefficients and SNRs
#' (Eq. 4) at test length `n_items`, plus the expected Kendall concordance
#' between independent replications implied by G (App. C).
#'
#' @param vc Output of [variance_components()] (or any table with
#'   `var_model`, `var_item`, `var_residual`, `n_items`).
#' @param n_items Test length to evaluate at; defaults to `vc$n_items`, i.e.
#'   the empirical coefficient of the observed test.
reliability_coefficients <- function(vc, n_items = vc$n_items) {
  out <- data.table::copy(data.table::as.data.table(vc))
  # Assign outside data.table's scope: inside `[`, `n_items` would mean the column.
  out$n_eval <- rep_len(n_items, nrow(out))
  out[, `:=`(
    g_rel = var_model / (var_model + var_residual / n_eval),
    g_abs = var_model / (var_model + (var_item + var_residual) / n_eval),
    snr_rel = n_eval * var_model / var_residual,
    snr_abs = n_eval * var_model / (var_item + var_residual)
  )]
  out[, expected_tau := g_to_expected_tau(g_rel)]
  out[]
}

#' Convert a reliability coefficient to an SNR, SNR = G / (1 - G)
g_to_snr <- function(g) { g <- pmin(pmax(g, 0), 1); ifelse(g >= 1, Inf, g / (1 - g)) }

#' Convert an SNR to a reliability coefficient, G = SNR / (1 + SNR)
snr_to_g <- function(snr) { snr <- pmax(snr, 0); ifelse(is.infinite(snr), 1, snr / (1 + snr)) }

#' Expected Kendall's tau between replications implied by G (Greiner; Eq. C.6)
#'
#' Under a Gaussian copula, E[tau] = (2/pi) asin(rho) with rho = G.
g_to_expected_tau <- function(g) (2 / pi) * asin(pmin(pmax(g, -1), 1))

#' SNR implied by an observed Kendall's tau (inverse of [g_to_expected_tau()])
tau_to_snr <- function(tau) g_to_snr(sin(pi / 2 * pmin(pmax(tau, -1), 1)))

# ---------------------------------------------------------------------------
# Decision studies
# ---------------------------------------------------------------------------

#' Project reliability to other test lengths (D-study)
#'
#' Holds the variance components fixed and varies the number of comparable
#' items. This is a projection for *additional items from the same universe*;
#' it does not describe a selected subset (use [reliability_curve()] for that).
#'
#' @param vc Variance components (one or more rows; grouping columns are kept).
#' @param n_grid Integer test lengths.
dstudy_curve <- function(vc, n_grid) {
  vc <- data.table::as.data.table(vc)
  out <- vc[rep(seq_len(nrow(vc)), each = length(n_grid))]
  out[, n_items := rep(n_grid, times = nrow(vc))]
  reliability_coefficients(out, n_items = out$n_items)
}

#' Items needed to reach a target reliability (Eq. 5)
#'
#' n* = max(1, ceil(r / (1 - r) * B / s2_M)) with B = s2_R (relative) or
#' s2_I + s2_R (absolute).
#'
#' @param vc Variance components.
#' @param target Target reliability in (0, 1).
#' @param type `"relative"` or `"absolute"`.
items_needed <- function(vc, target = 0.9, type = c("relative", "absolute")) {
  type <- match.arg(type)
  B <- if (type == "relative") vc$var_residual else vc$var_item + vc$var_residual
  # The tolerance stops 0.9 / 0.1 * 20 = 180.00000000000003 rounding up to 181.
  ifelse(vc$var_model > 0, pmax(1, ceiling(target / (1 - target) * B / vc$var_model - 1e-9)), Inf)
}
