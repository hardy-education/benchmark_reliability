# The one-pass curve must equal a fresh variance decomposition at every prefix.

test_that("reliability_curve equals per-prefix variance_components", {
  X <- sim_responses(50, 30)
  ord <- sample(colnames(X))
  cv <- reliability_curve(X, ord)
  brute <- data.table::rbindlist(lapply(2:ncol(X), function(n) variance_components(X[, ord[1:n], drop = FALSE], "anova")))
  expect_equal(cv$var_model, brute$var_model, tolerance = 1e-10)
  expect_equal(cv$var_item, brute$var_item, tolerance = 1e-10)
  expect_equal(cv$var_residual, brute$var_residual, tolerance = 1e-10)
})

test_that("reliability_curve Kendall tau equals stats::cor on prefix means", {
  X <- sim_responses(40, 20, seed = 2)
  ord <- colnames(X)
  cv <- reliability_curve(X, ord, steps = c(5, 10, 20))
  ref <- sapply(c(5, 10, 20), function(n) cor(rowMeans(X[, ord[1:n]]), rowMeans(X), method = "kendall"))
  expect_equal(cv$kendall, ref, tolerance = 1e-10)
})

test_that("reliability_curve falls back to lmer with missing cells", {
  skip_if_not_installed("lme4")
  X <- sim_responses(30, 12, seed = 3)
  X[1, 2] <- NA
  cv <- reliability_curve(X, colnames(X), steps = c(4, 12), kendall = FALSE)
  expect_true(all(cv$engine == "lmer"))
  expect_true(all(cv$g_rel >= 0 & cv$g_rel <= 1))
})

test_that("handle_missing auto drops the cheaper of items or models", {
  X <- sim_responses(30, 12)
  X[1:5, 1] <- NA                     # one item, five models -> drop the item
  expect_equal(attr(handle_missing(X), "dropped")$items, colnames(X)[1])
  X <- sim_responses(30, 12)
  X[1, 1:6] <- NA                     # one model, six items -> drop the model
  expect_equal(attr(handle_missing(X), "dropped")$models, rownames(X)[1])
})

test_that("populations have the documented sizes", {
  sc <- data.table::data.table(benchmark = "b", model = paste0("m", 1:41), score = seq(0, 1, length.out = 41))
  expect_length(population_models(sc, "b", "top_frac:0.1"), 4)
  expect_length(population_models(sc, "b", "top_n:10"), 10)
  expect_length(population_models(sc, "b", "top_half"), 20)
  expect_true("m41" %in% population_models(sc, "b", "top_frac:0.1"))
})
