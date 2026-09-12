# Tests for the reliability <-> SNR <-> rank-concordance identities of
# Sec. 3.1 and App. C. These are the paper's central claims, so they are
# checked as identities rather than against stored numbers.

test_that("G and SNR are inverse transforms (Eq. 5)", {
  g <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
  expect_equal(snr_to_g(g_to_snr(g)), g)
  snr <- c(0, 0.5, 1, 3, 9, 99)
  expect_equal(g_to_snr(snr_to_g(snr)), snr)
})

test_that("known reliability/SNR pairs are correct", {
  # G = 0.5 <-> SNR = 1; G = 0.9 <-> SNR = 9 (Eq. 5).
  expect_equal(g_to_snr(0.5), 1)
  expect_equal(g_to_snr(0.9), 9)
  expect_equal(snr_to_g(9), 0.9)
})

test_that("boundary reliabilities do not produce NaN", {
  expect_equal(g_to_snr(1), Inf)
  expect_equal(snr_to_g(Inf), 1)
  expect_equal(g_to_snr(0), 0)
  expect_equal(g_to_error_tolerance(1), 0)
  expect_equal(g_to_error_tolerance(0), Inf)
})

test_that("Greiner's identity and its inverse round-trip (Eqs. 17, 19)", {
  snr <- c(0.1, 0.5, 1, 3, 9, 49)
  expect_equal(tau_to_snr(snr_to_expected_tau(snr)), snr)
})

test_that("expected tau matches Greiner's identity at reference points", {
  # rho = 0   -> tau = 0
  # rho = 1   -> tau = 1        (2/pi * arcsin(1) = 2/pi * pi/2)
  # rho = 0.5 -> tau = 1/3      (2/pi * arcsin(1/2) = 2/pi * pi/6)
  expect_equal(g_to_expected_tau(0), 0)
  expect_equal(g_to_expected_tau(1), 1)
  expect_equal(g_to_expected_tau(0.5), 1 / 3)
})

test_that("expected tau is monotone increasing in SNR", {
  snr <- 10^seq(-2, 3, length.out = 50)
  taus <- snr_to_expected_tau(snr)
  expect_true(all(diff(taus) > 0))
  expect_true(all(taus >= 0 & taus <= 1))
})

test_that("squared tau is the square of expected tau (Eq. 18)", {
  snr <- c(0.25, 1, 4, 16)
  expect_equal(snr_to_expected_tau2(snr), snr_to_expected_tau(snr)^2)
  # Closed form: (4/pi^2) * arcsin^2(SNR/(1+SNR))
  expect_equal(snr_to_expected_tau2(snr),
               (4 / pi^2) * asin(snr / (1 + snr))^2)
})

test_that("relative reliability ignores item variance, absolute does not", {
  # Paper Eq. (2): the item-effect term is common to all models and cannot
  # reorder them, so G_rel must not depend on it while dependability does.
  a <- gstudy_coefficients(var_model = 1, var_item = 0.5, var_residual = 1)
  b <- gstudy_coefficients(var_model = 1, var_item = 5.0, var_residual = 1)
  expect_equal(a$m_rel, b$m_rel)
  expect_gt(a$m_dep, b$m_dep)
  expect_equal(a$m_rel, 0.5)
})

test_that("D-study reliability rises with items and follows the 1/I law (Eq. 3)", {
  comp <- gstudy_coefficients(var_model = 0.01, var_item = 0.05,
                              var_residual = 0.20)
  proj <- dstudy_curve(comp, c(1, 10, 100, 1000))
  expect_true(all(diff(proj$m_rel_D) > 0))
  # Direct check of sigma_M^2 / (sigma_M^2 + sigma_res^2 / I).
  expect_equal(proj$m_rel_D, 0.01 / (0.01 + 0.20 / c(1, 10, 100, 1000)))
  # Reliability approaches 1 as items accumulate.
  expect_gt(dstudy_project(comp, n_items = 1e6)$m_rel_D, 0.99)
})

test_that("D-study reliability never exceeds 1 or drops below 0", {
  comp <- gstudy_coefficients(0.001, 0.5, 0.5)
  proj <- dstudy_curve(comp, c(1, 5, 50, 500, 5000))
  expect_true(all(proj$m_rel_D >= 0 & proj$m_rel_D <= 1))
  expect_true(all(proj$m_dep_D >= 0 & proj$m_dep_D <= 1))
})

test_that("the expected tau carried by a D-study matches the mapping", {
  comp <- gstudy_coefficients(0.02, 0.1, 0.3)
  proj <- dstudy_project(comp, n_items = 50)
  expect_equal(proj$expected_tau, g_to_expected_tau(proj$m_rel_D))
})

test_that("a failed G-study returns the full column set", {
  ok  <- gstudy_coefficients(1, 1, 1, benchmark = "x")
  bad <- gstudy_na_row("x")
  expect_identical(names(ok), names(bad))
  expect_true(all(is.na(bad$m_rel)))
})

test_that("fit_gstudy recovers known variance components", {
  # Simulate Eq. (1) directly, then check the fitted components are close.
  set.seed(42)
  n_models <- 120; n_items <- 60
  u <- rnorm(n_models, sd = sqrt(0.05))   # sigma^2_M = 0.05
  v <- rnorm(n_items,  sd = sqrt(0.20))   # sigma^2_I = 0.20
  dat <- expand.grid(model = seq_len(n_models), item = seq_len(n_items)) |>
    dplyr::mutate(
      benchmark = "sim",
      model = as.character(model), item = as.character(item),
      resp = 0.5 + u[as.integer(model)] + v[as.integer(item)] +
        rnorm(dplyr::n(), sd = sqrt(0.10))   # sigma^2_e = 0.10
    )
  fit <- fit_gstudy(dat)
  expect_equal(fit$model,    0.05, tolerance = 0.35)
  expect_equal(fit$item,     0.20, tolerance = 0.35)
  expect_equal(fit$Residual, 0.10, tolerance = 0.10)
  # And the reliability implied by those components.
  expect_equal(fit$m_rel, 0.05 / (0.05 + 0.10), tolerance = 0.15)
})

test_that("the relative _scale coefficients are identical to the _D ones", {
  # Dividing numerator and denominator of the relative coefficient by the
  # number of models cancels, so m_rel_scale == m_rel_D and i_rel_scale ==
  # i_rel_D by construction, for any design size. The original code carried
  # both pairs as if they differed. The dependability pair genuinely does
  # differ, and that is checked below.
  comp <- rbind(
    gstudy_coefficients(0.01, 0.05, 0.20, "b1"),
    gstudy_coefficients(0.30, 0.02, 0.90, "b2")
  )
  for (nm in c(1L, 7L, 250L)) {
    for (nmod in c(1L, 13L, 400L)) {
      proj <- dstudy_project(comp, n_items = nm, n_models = nmod)
      expect_equal(proj$m_rel_scale, proj$m_rel_D,
                   info = glue("n_items = {nm}, n_models = {nmod}"))
      expect_equal(proj$i_rel_scale, proj$i_rel_D,
                   info = glue("n_items = {nm}, n_models = {nmod}"))
    }
  }
})

test_that("the dependability _scale coefficients do depend on model count", {
  comp <- gstudy_coefficients(0.05, 0.10, 0.30, "b")
  one  <- dstudy_project(comp, n_items = 50, n_models = 1)
  many <- dstudy_project(comp, n_items = 50, n_models = 100)
  # More models means the item term is charged relatively more heavily against
  # the averaged signal, so dependability falls.
  expect_gt(one$m_dep_scale, many$m_dep_scale)
  expect_equal(one$m_dep_scale, one$m_dep_D)
})

test_that("i_dep_D does not depend on the item count", {
  # Items are the object of measurement and are not averaged over, so the
  # absolute error variance is sigma_M^2/n_models + sigma_e^2/n_models, with
  # no 1/I term. Compare i_rel_D, which likewise carries no n_items divisor.
  comp <- gstudy_coefficients(0.0039, 0.0254, 0.1951, "b")
  vals <- vapply(c(1L, 10L, 1000L),
                 function(ni) dstudy_project(comp, n_items = ni,
                                             n_models = 4)$i_dep_D,
                 numeric(1))
  expect_equal(vals, rep(vals[1], 3))
  expect_equal(vals[1], 0.0254 / (0.0254 + 0.0039 / 4 + 0.1951 / 4))
})
