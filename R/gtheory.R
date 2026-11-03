# Generalizability theory for a crossed model x item design (paper Sec. 3).
#
#   Y_mi = mu + u_m + v_i + e_mi,   Var(Y) = s2_M + s2_I + s2_R      (Eq. 1)
#
#   relative reliability   G(n)   = s2_M / (s2_M + s2_R / n)          (Eq. 2)
#   absolute reliability   Phi(n) = s2_M / (s2_M + (s2_I + s2_R) / n) (Eq. 3)
#   SNR_rel = n s2_M / s2_R,  SNR_abs = n s2_M / (s2_I + s2_R)        (Eq. 4)
#
# Estimation. With one response per cell and no missing cells the design is
# balanced, and the REML estimates have a closed form: the ANOVA (expected mean
# squares) estimates when they are all non-negative, and a pooled re-estimate
# when a component falls on the zero boundary. This is identical to what
# lme4::lmer(resp ~ 1 + (1|item) + (1|model)) returns (see
# tests/testthat/test-gtheory.R) but costs O(M * I) arithmetic instead of an
# iterative fit, which is what makes item-by-item reliability curves cheap.
# With missing cells, `engine = "lmer"` fits the mixed model directly.

# ---------------------------------------------------------------------------
# Variance components
# ---------------------------------------------------------------------------

# REML for the balanced two-way crossed random-effects model, vectorised over
# any number of designs. Inputs are sums of squares about the grand mean.
# Returns a list of component vectors.
.balanced_reml <- function(ssm, ssi, ssr, M, I) {
  dfm <- M - 1; dfi <- I - 1; dfr <- (M - 1) * (I - 1)
  msm <- ssm / dfm; msi <- ssi / dfi; msr <- ssr / dfr
  vm <- (msm - msr) / I
  vi <- (msi - msr) / M
  vr <- msr
  # Boundary: a component whose moment estimate is negative is set to zero and
  # its sum of squares is pooled into the residual (the REML solution of the
  # reduced model). Pooling can push the other component negative, so the
  # "both on the boundary" case is resolved last.
  # Both flags come from the unconstrained estimates (they are disjoint).
  m0 <- vm < 0 & vi >= 0
  i0 <- vi < 0 & vm >= 0
  if (any(m0, na.rm = TRUE)) {
    w <- which(m0)
    vr[w] <- (ssm[w] + ssr[w]) / (dfm[w] + dfr[w])
    vm[w] <- 0
    vi[w] <- (msi[w] - vr[w]) / M[w]
  }
  if (any(i0, na.rm = TRUE)) {
    w <- which(i0)
    vr[w] <- (ssi[w] + ssr[w]) / (dfi[w] + dfr[w])
    vi[w] <- 0
    vm[w] <- (msm[w] - vr[w]) / I[w]
  }
  both <- (vm < 0 | vi < 0) & !is.na(vm)
  if (any(both)) {
    w <- which(both)
    vr[w] <- (ssm[w] + ssi[w] + ssr[w]) / (dfm[w] + dfi[w] + dfr[w])
    vm[w] <- 0; vi[w] <- 0
  }
  list(var_model = vm, var_item = vi, var_residual = vr)
}

#' Variance components of a models x items response matrix
#'
#' @param X Numeric matrix, models in rows, items in columns.
#' @param engine `"auto"` uses the exact closed form when `X` is complete and
#'   `lmer` otherwise; `"anova"` forces the closed form (errors on NA);
#'   `"lmer"` always fits `lme4::lmer` (the legacy behaviour).
#' @return One-row `data.table` with `var_model`, `var_item`, `var_residual`,
#'   `n_models`, `n_items`, `engine`.
#' @examples
#' X <- matrix(rbinom(200, 1, 0.5), 20, dimnames = list(NULL, paste0("i", 1:10)))
#' variance_components(X)
variance_components <- function(X, engine = c("auto", "anova", "lmer")) {
  engine <- match.arg(engine)
  .check_matrix(X)
  if (engine == "auto") engine <- if (anyNA(X)) "lmer" else "anova"
  M <- nrow(X); I <- ncol(X)
  if (engine == "anova") {
    if (anyNA(X)) stop("engine = 'anova' needs a complete matrix.", call. = FALSE)
    Xc <- X - mean(X)
    ssm <- I * sum(rowMeans(Xc)^2)
    ssi <- M * sum(colMeans(Xc)^2)
    ssr <- sum(Xc^2) - ssm - ssi
    vc <- .balanced_reml(ssm, ssi, ssr, M, I)
  } else {
    vc <- .lmer_components(X)
  }
  data.table::data.table(var_model = vc$var_model, var_item = vc$var_item, var_residual = vc$var_residual,
                         n_models = M, n_items = I, engine = engine)
}

# Fit resp ~ 1 + (1|item) + (1|model) by REML on the observed cells.
.lmer_components <- function(X) {
  if (!requireNamespace("lme4", quietly = TRUE)) stop("Package 'lme4' is required for engine = 'lmer'.", call. = FALSE)
  obs <- which(!is.na(X), arr.ind = TRUE)
  d <- data.frame(resp = X[obs], model = factor(obs[, 1]), item = factor(obs[, 2]))
  fit <- suppressMessages(lme4::lmer(resp ~ 1 + (1 | item) + (1 | model), data = d,
                                     control = lme4::lmerControl(optimizer = "bobyqa", calc.derivs = FALSE,
                                                                 optCtrl = list(maxfun = 1e5))))
  vc <- as.data.frame(lme4::VarCorr(fit))
  list(var_model = vc$vcov[vc$grp == "model"], var_item = vc$vcov[vc$grp == "item"],
       var_residual = vc$vcov[vc$grp == "Residual"])
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
