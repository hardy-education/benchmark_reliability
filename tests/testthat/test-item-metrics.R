# The closed forms must reproduce the packages the legacy code called.

test_that("lambda_star closed form equals the fixest regression (legacy estimator)", {
  skip_if_not_installed("fixest")
  X <- sim_responses(50, 30)
  expect_equal(lambda_star(X)[1:8], lambda_star_feols(X, colnames(X)[1:8]), tolerance = 1e-8)
})

test_that("lambda_star closed form also holds when M < I", {
  skip_if_not_installed("fixest")
  X <- sim_responses(12, 60, seed = 3)
  expect_equal(lambda_star(X)[1:5], lambda_star_feols(X, colnames(X)[1:5]), tolerance = 1e-8)
})

test_that("lambda_star rejects non-binary input", {
  X <- sim_responses(); X[1, 1] <- 0.5
  expect_error(lambda_star(X), "0/1")
})

test_that("loevinger_hi equals mokken::coefH()$Hi", {
  skip_if_not_installed("mokken")
  X <- sim_responses(80, 25, seed = 4)
  expect_equal(unname(loevinger_hi(X)), as.numeric(mokken::coefH(X, se = FALSE, results = FALSE)$Hi), tolerance = 1e-10)
})

test_that("loevinger_hi handles tied item proportions", {
  skip_if_not_installed("mokken")
  X <- sim_responses(40, 20, seed = 5)
  X[, 2] <- X[, 1][sample(nrow(X))]   # same p as item 1
  expect_equal(unname(loevinger_hi(X)), as.numeric(mokken::coefH(X, se = FALSE, results = FALSE)$Hi), tolerance = 1e-10)
})

test_that("corrected_iic equals psych r.drop^2 / alpha.drop (Eq. 34)", {
  skip_if_not_installed("psych")
  X <- sim_responses(70, 20, seed = 6)
  a <- suppressWarnings(suppressMessages(psych::alpha(as.data.frame(X), check.keys = FALSE, warnings = FALSE)))
  ref <- a$item.stats$r.drop^2 / a$alpha.drop$raw_alpha
  expect_equal(unname(corrected_iic(X)), ref, tolerance = 1e-10)
})

test_that("item_metrics reports NA for items constant in the reference population", {
  X <- sim_responses(30, 15)
  X[, 3] <- 1
  im <- item_metrics(X, c("lambda_star", "difficulty", "ciic", "loevinger_hi"))
  expect_true(all(is.na(im[item == colnames(X)[3]]$value)))
  expect_false(anyNA(im[item != colnames(X)[3]]$value))
})

test_that("selection_order puts the highest score first and unscored items last", {
  s <- c(a = 0.1, b = 0.9, c = NA, d = 0.5)
  o <- selection_order(s, names(s), "lambda_star")
  expect_equal(o[1:3], c("b", "d", "a"))
  expect_equal(o[4], "c")
  expect_setequal(selection_order(NULL, names(s), "random", seed = 3), names(s))
})
