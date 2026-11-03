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
