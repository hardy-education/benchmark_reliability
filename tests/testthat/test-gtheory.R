# The closed-form REML must agree with lme4::lmer, including on the boundary.

lmer_vc <- function(X) {
  d <- data.frame(resp = as.vector(X), model = factor(rep(seq_len(nrow(X)), ncol(X))),
                  item = factor(rep(seq_len(ncol(X)), each = nrow(X))))
  fit <- suppressMessages(lme4::lmer(resp ~ 1 + (1 | item) + (1 | model), data = d,
                                     control = lme4::lmerControl(optimizer = "bobyqa", calc.derivs = FALSE)))
  v <- as.data.frame(lme4::VarCorr(fit))
  c(v$vcov[v$grp == "model"], v$vcov[v$grp == "item"], v$vcov[v$grp == "Residual"])
}

test_that("anova engine equals lmer REML for interior solutions", {
  skip_if_not_installed("lme4")
  X <- sim_responses(60, 40)
  vc <- variance_components(X, "anova")
  expect_equal(c(vc$var_model, vc$var_item, vc$var_residual), lmer_vc(X), tolerance = 1e-5)
})

test_that("anova engine equals lmer REML when the model variance hits zero", {
  skip_if_not_installed("lme4")
  set.seed(11)
  X <- matrix(rbinom(40 * 6, 1, rep(c(0.2, 0.4, 0.5, 0.6, 0.7, 0.9), each = 40)), 40, 6,
              dimnames = list(NULL, paste0("i", 1:6)))  # no model signal
  vc <- variance_components(X, "anova")
  expect_equal(vc$var_model, 0)
  expect_equal(c(vc$var_model, vc$var_item, vc$var_residual), lmer_vc(X), tolerance = 1e-4)
})

test_that("anova engine equals lmer REML when the item variance hits zero", {
  skip_if_not_installed("lme4")
  set.seed(12)
  X <- matrix(rbinom(30 * 8, 1, rep(plogis(rnorm(30, 0, 1.5)), 8)), 30, 8,
              dimnames = list(NULL, paste0("i", 1:8)))  # no item signal
  vc <- variance_components(X, "anova")
  expect_equal(c(vc$var_model, vc$var_item, vc$var_residual), lmer_vc(X), tolerance = 1e-4)
})

test_that("G and Phi follow Eqs. 2-3 and SNR maps round-trip", {
  vc <- data.table::data.table(var_model = 0.02, var_item = 0.03, var_residual = 0.18, n_items = 50)
  r <- reliability_coefficients(vc)
  expect_equal(r$g_rel, 0.02 / (0.02 + 0.18 / 50))
  expect_equal(r$g_abs, 0.02 / (0.02 + 0.21 / 50))
  expect_true(r$g_abs <= r$g_rel)
  expect_equal(snr_to_g(g_to_snr(r$g_rel)), r$g_rel)
  expect_equal(tau_to_snr(g_to_expected_tau(0.8)), g_to_snr(0.8))
})

test_that("items_needed inverts the D-study (Eq. 5)", {
  vc <- data.table::data.table(var_model = 0.01, var_item = 0.02, var_residual = 0.2, n_items = 10)
  n <- items_needed(vc, 0.9)
  expect_gte(reliability_coefficients(vc, n)$g_rel, 0.9 - 1e-12)
  expect_lt(reliability_coefficients(vc, n - 1)$g_rel, 0.9)
})

# ---------------------------------------------------------------------------
# Constrained REML: boundary, degenerate and missing-data cases
# ---------------------------------------------------------------------------

# Sums of squares by the textbook (residual computed directly).
direct_ss <- function(X) {
  Xc <- X - mean(X); rm <- rowMeans(Xc); cm <- colMeans(Xc)
  c(ssm = ncol(X) * sum(rm^2), ssi = nrow(X) * sum(cm^2), ssr = sum((Xc - outer(rm, cm, "+"))^2))
}

# Independent check: maximise the balanced REML likelihood numerically under
# s2_M, s2_I >= 0. -2 logL = sum_s df_s log(l_s) + SS_s / l_s, with
# l_M = s2_R + I s2_M, l_I = s2_R + M s2_I, l_R = s2_R.
reml_deviance <- function(X, p) {
  M <- nrow(X); I <- ncol(X); ss <- unname(direct_ss(X))
  df <- c(M - 1, I - 1, (M - 1) * (I - 1))
  l <- c(p[3] + I * p[1], p[3] + M * p[2], p[3]); sum(df * log(l) + ss / l)
}
brute_reml <- function(X) {
  M <- nrow(X); I <- ncol(X); ss <- unname(direct_ss(X))
  df <- c(M - 1, I - 1, (M - 1) * (I - 1))
  f <- function(p) reml_deviance(X, p)
  s <- ss / df
  starts <- list(c(max((s[1] - s[3]) / I, 0), max((s[2] - s[3]) / M, 0), s[3]), c(0, 0, sum(ss) / sum(df)),
                 c(s[1] / I, s[2] / M, s[3] / 2))
  fits <- lapply(starts, function(st) stats::optim(st, f, method = "L-BFGS-B", lower = c(0, 0, 1e-10),
                                                   control = list(factr = 1, pgtol = 0, maxit = 1e4)))
  fits[[which.min(vapply(fits, `[[`, 0, "value"))]]$par
}

# Small random 0/1 matrices put a large share of draws on the boundary,
# including the case where both moment estimates are negative.
random_designs <- function(k, seed) {
  set.seed(seed)
  lapply(seq_len(k), function(j) {
    M <- sample(3:8, 1); I <- sample(2:6, 1)
    X <- matrix(rbinom(M * I, 1, runif(1, 0.2, 0.8)), M, I, dimnames = list(NULL, paste0("i", seq_len(I))))
    X
  })
}

test_that("closed form equals numerical REML on every boundary configuration", {
  seen <- character()
  for (X in random_designs(400, 21)) {
    vc <- variance_components(X, "anova")
    if (!vc$status %in% REML_STATUS[1:4]) next
    seen <- c(seen, vc$status)
    est <- c(vc$var_model, vc$var_item, vc$var_residual); num <- brute_reml(X)
    info <- paste(vc$status, paste(dim(X), collapse = "x"))
    # The closed form is the constrained maximiser: no feasible point the
    # optimiser finds has a higher REML likelihood, and the two agree to
    # the optimiser's precision.
    expect_lte(reml_deviance(X, est), reml_deviance(X, num) + 1e-10, label = info)
    expect_equal(est, num, tolerance = 1e-3, info = info)
  }
  expect_true(all(REML_STATUS[1:4] %in% seen))
})

test_that("both moment estimates negative is not the same as both components zero", {
  # Find designs with MS_M < MS_R and MS_I < MS_R whose REML keeps one component.
  kept <- 0
  for (X in random_designs(400, 22)) {
    M <- nrow(X); I <- ncol(X)
    # Exact integer sums of squares: the textbook residual-by-subtraction can
    # turn an exact tie (all mean squares equal) into a spurious "both negative".
    e <- .crossed_ss(sum(X^2), sum(rowSums(X)^2), sum(colSums(X)^2), sum(X), M, I, exact = TRUE)
    ms <- c(e$ssm, e$ssi, e$ssr) / c(M - 1, I - 1, (M - 1) * (I - 1))
    if (!(ms[1] < ms[3] && ms[2] < ms[3])) next
    vc <- variance_components(X, "anova")
    if (vc$status == "boundary_both") next
    kept <- kept + 1
    expect_true(vc$var_model > 0 || vc$var_item > 0)
    if (requireNamespace("lme4", quietly = TRUE))
      expect_equal(c(vc$var_model, vc$var_item, vc$var_residual), unlist(.lmer_components(X)[1:3]),
                   tolerance = 1e-4, ignore_attr = TRUE)
  }
  expect_gt(kept, 5)
})

test_that("a positive model variance survives when only items and residual are pooled", {
  # MS_I < MS_M < MS_R but MS_M exceeds the item+residual pool: s2_M > 0, G > 0.
  X <- matrix(c(1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0),
              6, 5, dimnames = list(NULL, paste0("i", 1:5)))
  ss <- direct_ss(X); ms <- ss / c(5, 4, 20)
  pir <- (ss[2] + ss[3]) / 24
  skip_if_not(ms[2] < ms[1] && ms[1] < ms[3] && ms[1] >= pir, "fixture no longer in the target case")
  vc <- variance_components(X, "anova")
  expect_equal(vc$status, "boundary_item")
  expect_equal(vc$var_model, (ms[[1]] - pir[[1]]) / 5)   # (MS_M - pooled) / I
  skip_if_not_installed("lme4")
  expect_equal(c(vc$var_model, vc$var_item, vc$var_residual), unlist(.lmer_components(X)[1:3]),
               tolerance = 1e-4, ignore_attr = TRUE)
  expect_gt(reliability_coefficients(vc)$g_rel, 0)
})

test_that("a zero residual is reported as degenerate, not as a zero-variance fit", {
  X <- cbind(i1 = c(0, 1, 1, 0, 1), i2 = c(0, 1, 1, 0, 1))  # two identical items
  vc <- variance_components(X, "anova")
  expect_equal(vc$status, "degenerate")
  expect_true(is.na(vc$var_model) && is.na(vc$var_residual))
  expect_true(is.na(reliability_coefficients(vc)$g_rel))
  X <- rbind(c(0, 1, 1), c(0, 1, 1), c(0, 1, 1))           # identical models: item effects only
  colnames(X) <- c("i1", "i2", "i3")
  expect_equal(variance_components(X, "anova")$status, "degenerate")
  expect_equal(variance_components(matrix(1, 4, 3, dimnames = list(NULL, letters[1:3])), "anova")$status, "constant")
  expect_equal(variance_components(matrix(c(0, 1), 1, 2, dimnames = list(NULL, letters[1:2])), "anova")$status, "too_small")
})

test_that("integer sums of squares are exact and match the direct residual", {
  X <- sim_responses(80, 60, seed = 5)
  rs <- rowSums(X); cs <- colSums(X)
  ss <- .crossed_ss(sum(X^2), sum(rs^2), sum(cs^2), sum(rs), nrow(X), ncol(X), exact = TRUE)
  expect_true(ss$exact)
  expect_equal(unlist(ss[c("ssm", "ssi", "ssr")]), direct_ss(X), tolerance = 1e-12, ignore_attr = TRUE)
  # Integer numerators: M*I*SS is a whole number.
  expect_equal((nrow(X) * ncol(X) * ss$ssr) %% 1, 0)
  # Non-integer data with a tiny residual: no negative residual from cancellation.
  Y <- outer(rnorm(30, 0, 1e4), rep(1, 20)) + outer(rep(1, 30), rnorm(20, 0, 1e4)) + 1e-9 * rnorm(600)
  colnames(Y) <- paste0("i", 1:20)
  vc <- variance_components(Y, "anova")
  expect_true(vc$status == "degenerate" || vc$var_residual >= 0)
})

test_that(".balanced_reml recycles scalar inputs and rejects ragged ones", {
  ss <- lapply(2:6, function(n) direct_ss(sim_responses(20, 12, seed = n)[, 1:n]))
  ssm <- sapply(ss, `[[`, "ssm"); ssi <- sapply(ss, `[[`, "ssi"); ssr <- sapply(ss, `[[`, "ssr")
  vec <- .balanced_reml(ssm, ssi, ssr, 20, 2:6)          # scalar M, vector I
  one <- lapply(1:5, function(j) .balanced_reml(ssm[j], ssi[j], ssr[j], 20, j + 1))
  expect_false(anyNA(vec$var_model))
  expect_equal(vec$var_model, sapply(one, `[[`, "var_model"))
  expect_equal(vec$status, sapply(one, `[[`, "status"))
  expect_error(.balanced_reml(ssm, ssi, ssr, c(20, 20), 2:6), "common length")
})

test_that("missing cells go to lmer; dropping NAs from the balanced formulas is not REML", {
  skip_if_not_installed("lme4")
  X <- sim_responses(40, 25, seed = 6)
  set.seed(6); X[sample(length(X), 60)] <- NA
  expect_error(variance_components(X, "anova"), "complete matrix")
  vc <- variance_components(X)
  expect_equal(vc$engine, "lmer")
  # Naive na.rm version of the balanced formulas.
  M <- nrow(X); I <- ncol(X); Xc <- X - mean(X, na.rm = TRUE)
  ssm <- I * sum(rowMeans(Xc, na.rm = TRUE)^2); ssi <- M * sum(colMeans(Xc, na.rm = TRUE)^2)
  ssr <- sum(Xc^2, na.rm = TRUE) - ssm - ssi
  naive <- .balanced_reml(ssm, ssi, ssr, M, I)
  expect_false(isTRUE(all.equal(c(naive$var_model, naive$var_item, naive$var_residual),
                                c(vc$var_model, vc$var_item, vc$var_residual), tolerance = 1e-4)))
})
