# Item-level selection criteria (paper Sec. 4.3 and App. D.8).
#
# Every function takes a complete models x items matrix X (models in rows) for
# the *reference* population and returns a named numeric vector, one value per
# item, where larger means "select earlier". Items that are constant in the
# reference population carry no information about model differences there;
# [item_metrics()] removes them before scoring and reports NA for them, which
# matches the legacy pipeline (it applied janitor::remove_constant() first).
#
# For 0/1 data, lambda-star-6, Loevinger's H_i and the attenuation-corrected
# item-rest correlation all have exact closed forms that cost O(M * I) in
# total. They reproduce fixest / mokken / psych to floating-point precision
# (tests/testthat/test-item-metrics.R), so no bootstrap over item subsets is
# needed to make the full-benchmark computation tractable. See
# docs/closed_forms.md for the derivations.

# ---------------------------------------------------------------------------
# lambda-star-6 (Eqs. 16-18)
# ---------------------------------------------------------------------------

#' lambda-star-6 item communality proxy, L*6(i), exact closed form
#'
#' The paper defines L*6(i) as the R^2 of the slope-activated two-way model
#'   Y_mj = X_mi = alpha_i + S_mj gamma_j + S_mj delta_m + eta_mj,  S_mj = X_mj,
#' over all models m and other items j != i. For binary S the model is
#' saturated on activated cells (S_mj = 1): delta_m alone fits X_mi exactly
#' there. Only the non-activated cells (model m failed item j) leave residual,
#' all fitted by the intercept. With z_mi the number of other items model m
#' failed, the OLS solution is
#'   alpha_i = sum_m z_mi X_mi / sum_m z_mi
#'   RSS_i   = (sum_m z_mi) alpha_i (1 - alpha_i)
#'   TSS_i   = (I - 1) M p_i (1 - p_i)
#'   L*6(i)  = 1 - RSS_i / TSS_i.
#' This equals `fixest::feols(resp ~ 1 | itemx[[s]] + model[[s]])`'s R^2
#' (the legacy estimator) to ~1e-13, and runs in milliseconds where the
#' regression took ~1 hour on AirBench.
#'
#' @param X Complete 0/1 matrix, models x items, no constant columns.
#' @return Named numeric vector of L*6(i).
#' @seealso [lambda_star_feols()] for the regression form (any score scale).
lambda_star <- function(X) {
  .check_matrix(X, need_complete = TRUE)
  if (!is_binary_matrix(X)) stop("The closed form needs 0/1 scores; use lambda_star_feols() for other scales.", call. = FALSE)
  M <- nrow(X); I <- ncol(X)
  Fm <- rowSums(1 - X)                 # failures per model, all items
  Z <- Fm - (1 - X)                    # z_mi: failures on items other than i
  zs <- colSums(Z)
  a <- colSums(Z * X) / zs
  rss <- ifelse(zs > 0, zs * a * (1 - a), 0)   # zs = 0: every cell activated, perfect fit
  p <- colMeans(X)
  tss <- (I - 1) * M * p * (1 - p)
  out <- ifelse(tss > 0, 1 - rss / tss, NA_real_)
  stats::setNames(out, colnames(X))
}

#' lambda-star-6 by explicit fixed-effects regression (reference implementation)
#'
#' One `fixest::feols` fit per item, as in the legacy code. Use it for
#' non-binary scores or to check [lambda_star()]. Cost grows as O(I^2 M).
#'
#' @param X Complete numeric matrix, models x items.
#' @param items Optional subset of target items.
lambda_star_feols <- function(X, items = colnames(X)) {
  if (!requireNamespace("fixest", quietly = TRUE)) stop("Package 'fixest' is required.", call. = FALSE)
  .check_matrix(X, need_complete = TRUE)
  M <- nrow(X); I <- ncol(X)
  vapply(items, function(ti) {
    others <- setdiff(colnames(X), ti)
    d <- data.frame(resp = rep(X[, ti], times = length(others)),
                    s = as.vector(X[, others, drop = FALSE]),
                    itemx = rep(others, each = M),
                    model = rep(seq_len(M), times = length(others)))
    fit <- fixest::feols(resp ~ 1 | itemx[[s]] + model[[s]], data = d, warn = FALSE, notes = FALSE)
    unname(fixest::r2(fit, "r2"))
  }, numeric(1))
}

# ---------------------------------------------------------------------------
# Classical and nonparametric criteria
# ---------------------------------------------------------------------------

#' Raw item difficulty, 1 - proportion correct
#'
#' Larger = harder, so "select earlier" means hardest first, as in the paper.
item_difficulty <- function(X) stats::setNames(1 - colMeans(X), colnames(X))

#' Loevinger's item scalability coefficient H_i (App. D.8.2), exact closed form
#'
#' H_i = sum_{j != i} Cov(X_i, X_j) / sum_{j != i} Cov_max(X_i, X_j), with
#' Cov_max = min(p_i, p_j) - p_i p_j for dichotomous items. The numerator is
#' Cov(X_i, rest score) and the denominator needs only the sorted p's, so the
#' whole vector costs O(M I + I log I) instead of the O(M I^2) pairwise
#' matrices built by `mokken::coefH()` (which it matches to ~1e-15).
#'
#' @param X Complete 0/1 matrix, no constant columns.
loevinger_hi <- function(X) {
  .check_matrix(X, need_complete = TRUE)
  N <- nrow(X); I <- ncol(X)
  p <- colMeans(X); Tm <- rowSums(X)
  cov_sum <- (colSums(X * Tm) - colSums(X^2)) / N - p * (sum(p) - p)
  o <- order(p); ps <- p[o]
  min_sum <- numeric(I)
  min_sum[o] <- c(0, cumsum(ps)[-I]) + (I - seq_len(I)) * ps   # sum_{j != i} min(p_i, p_j)
  covmax_sum <- min_sum - p * (sum(p) - p)
  stats::setNames(cov_sum / covmax_sum, colnames(X))
}

#' Attenuation-corrected item-rest correlation (App. D.8.3, Eq. 34)
#'
#' rho^2(X_i, R_i) / alpha(R_i), where R_i is the rest score and alpha(R_i)
#' Cronbach's alpha of the remaining k - 1 items. Matches
#' `psych::alpha()$item.stats$r.drop^2 / alpha.drop$raw_alpha`.
#'
#' Note: the legacy code used psych's `r.cor` (a correlation corrected with SMCs
#' on the diagonal) in place of the item-rest correlation `r.drop` that Eq. 34
#' specifies. In the M < I regime SMCs are not identified, and the two orderings
#' correlate only ~0.66 (Spearman) on GPQA. This function follows the paper.
#'
#' @param X Complete numeric matrix, no constant columns.
#' @param signed If `TRUE`, keep the sign of the item-rest correlation, so items
#'   that run against the rest of the test rank last rather than first. The
#'   default `FALSE` follows Eq. 34 (squared, sign discarded).
corrected_iic <- function(X, signed = FALSE) {
  .check_matrix(X, need_complete = TRUE)
  M <- nrow(X); k <- ncol(X)
  Tm <- rowSums(X); v <- matrixStats::colVars(X)
  cov_xt <- (colSums(X * Tm) - M * colMeans(X) * mean(Tm)) / (M - 1)
  var_rest <- stats::var(Tm) - 2 * cov_xt + v
  r_drop <- (cov_xt - v) / sqrt(v * var_rest)
  alpha_drop <- (k - 1) / (k - 2) * (1 - (sum(v) - v) / var_rest)
  out <- r_drop^2 / alpha_drop
  if (signed) out <- sign(r_drop) * out
  out[!is.finite(out) | alpha_drop <= 0] <- NA_real_
  stats::setNames(out, colnames(X))
}

# ---------------------------------------------------------------------------
# 2PL IRT discrimination (App. D.8.1)
# ---------------------------------------------------------------------------

#' 2PL discrimination parameters from a full-information mirt fit
#'
#' Settings follow the legacy code (EM with SQUAREM acceleration, 2,000
#' cycles, theta in [-9, 9]). In the M << I regime (e.g. the top 10% of
#' models) the 2PL is weakly identified; check `attr(, "converged")`.
#'
#' @param X Complete 0/1 matrix, no constant columns.
#' @param ncycles Maximum EM cycles.
#' @param ... Passed to `mirt::mirt()`.
#' @return Named vector of discriminations `a`, with attributes `converged`,
#'   `difficulty` (IRT b), and `cycles`.
irt_2pl_discrimination <- function(X, ncycles = 2000, ...) {
  if (!requireNamespace("mirt", quietly = TRUE)) stop("Package 'mirt' is required for 2PL discrimination.", call. = FALSE)
  .check_matrix(X, need_complete = TRUE)
  df <- as.data.frame(X)
  fit <- tryCatch(
    mirt::mirt(df, 1, itemtype = "2PL", verbose = FALSE, accelerate = "squarem",
               technical = list(NCYCLES = ncycles, theta_lim = c(-9, 9)), ...),
    error = function(e) e)
  if (inherits(fit, "error")) {
    return(structure(stats::setNames(rep(NA_real_, ncol(X)), colnames(X)), converged = FALSE, error = conditionMessage(fit)))
  }
  pars <- mirt::coef(fit, simplify = TRUE, IRTpars = TRUE)$items
  structure(stats::setNames(pars[colnames(X), "a"], colnames(X)),
            converged = isTRUE(mirt::extract.mirt(fit, "converged")),
            difficulty = stats::setNames(pars[colnames(X), "b"], colnames(X)),
            cycles = mirt::extract.mirt(fit, "iterations"))
}

# ---------------------------------------------------------------------------
# Registry and driver
# ---------------------------------------------------------------------------

#' The selection methods compared in the paper
#'
#' `legacy` gives the column name used for the same criterion in the
#' pre-refactor results files, so old and new outputs can be joined.
SELECTION_METHODS <- data.table::data.table(
  method = c("lambda_star", "difficulty", "irt_a", "ciic", "loevinger_hi", "random"),
  label = c("Lambda 6*", "Raw Difficulty", "2PL discrimination", "Corrected Interitem Corr.", "Loevinger's Hi", "Random"),
  legacy = c("L6", "diff", "a", "CA", "Hi", "rand")
)

#' Method sets used in the timing study
METHOD_SETS <- list(
  with_2pl = c("lambda_star", "difficulty", "irt_a", "ciic", "loevinger_hi", "random"),
  no_2pl = c("lambda_star", "difficulty", "ciic", "loevinger_hi", "random")
)

.metric_fun <- function(method) {
  switch(method,
    lambda_star = lambda_star,
    difficulty = item_difficulty,
    irt_a = irt_2pl_discrimination,
    ciic = corrected_iic,
    loevinger_hi = loevinger_hi,
    stop("Unknown method '", method, "'.", call. = FALSE))
}

#' Score every item of a reference matrix under each selection method
#'
#' @param X Reference-population matrix (complete; see [handle_missing()]).
#' @param methods Method names from [SELECTION_METHODS]. `"random"` needs no
#'   scores and is skipped here; it is handled in [selection_order()].
#' @return Long `data.table(item, method, value)` with attribute `timing`
#'   (seconds per method) and `irt_converged` when 2PL was requested.
item_metrics <- function(X, methods = setdiff(METHOD_SETS$with_2pl, "random")) {
  methods <- setdiff(methods, "random")
  const <- constant_items(X)
  Xv <- X[, !colnames(X) %in% const, drop = FALSE]
  timing <- stats::setNames(numeric(length(methods)), methods)
  extra <- list()
  out <- data.table::rbindlist(lapply(methods, function(m) {
    v <- timed(.metric_fun(m)(Xv))
    timing[m] <<- attr(v, "seconds")
    if (m == "irt_a") extra$irt_converged <<- attr(v, "converged")
    vals <- stats::setNames(rep(NA_real_, ncol(X)), colnames(X))
    vals[names(v)] <- as.numeric(v)
    data.table::data.table(item = colnames(X), method = m, value = unname(vals))
  }))
  attr(out, "timing") <- timing
  attr(out, "irt_converged") <- extra$irt_converged
  out
}

#' Item order implied by a selection method
#'
#' Highest score first. Ties are broken at random (seeded), and items with no
#' score (constant in the reference population, or a failed fit) go last in
#' random order. For `method = "random"` the whole order is a seeded
#' permutation.
#'
#' @param scores Named numeric vector of item scores (ignored for random).
#' @param items All items to order.
#' @param method Method name.
#' @param seed Integer seed.
selection_order <- function(scores, items, method, seed = 1L) {
  set.seed(seed)
  if (method == "random") return(sample(items))
  s <- scores[items]
  tiebreak <- stats::runif(length(items))
  items[order(is.na(s), -s, tiebreak)]
}
