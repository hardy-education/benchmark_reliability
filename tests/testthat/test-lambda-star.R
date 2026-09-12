# Tests for lambda*_6 (Sec. 3.3, App. D) and classical Guttman lambda_6.

make_binary_matrix <- function(n_models, n_items, seed = 1, shared = 0) {
  set.seed(seed)
  # `shared` mixes in a common model ability, so a test can control how much
  # shared signal the items carry.
  ability <- stats::rnorm(n_models)
  wide <- data.table::data.table(model = as.character(seq_len(n_models)))
  for (j in seq_len(n_items)) {
    lin <- shared * ability + stats::rnorm(n_models) * sqrt(max(0, 1 - shared^2))
    wide[[paste0("item_", j)]] <- as.integer(lin > stats::qnorm(runif(1, 0.25, 0.75)))
  }
  wide
}

test_that("lambda*_6 returns one row per item with an R-squared in [0, 1]", {
  wide <- make_binary_matrix(40, 12, seed = 2)
  comps <- lambda_star_components(wide, n_workers = 1L)
  expect_equal(nrow(comps), 12L)
  expect_setequal(comps$item, paste0("item_", 1:12))
  expect_true(all(comps$L6 >= 0 & comps$L6 <= 1, na.rm = TRUE))
})

test_that("lambda*_6 works in the M << I regime that breaks classical lambda_6", {
  # 15 models, 60 items: this is the regime the estimator exists for.
  wide <- make_binary_matrix(15, 60, seed = 3, shared = 0.6)
  comps <- lambda_star_components(wide, n_workers = 1L)
  expect_equal(nrow(comps), 60L)
  expect_false(any(is.na(comps$L6)))
  # It must not saturate at 1 the way an unrestricted regression would.
  expect_lt(lambda_star(comps), 0.999)
})

test_that("classical lambda_6 warns when M <= I", {
  wide <- make_binary_matrix(10, 30, seed = 4)
  expect_warning(guttman_lambda6(wide), "ill-posed")
})

test_that("the effective sample size is activated edges, not models", {
  # Eq. (11) replicates the target across the other items, so nobs is
  # M x (I - 1) -- far larger than M, which is the point of App. D.3.2.
  wide <- make_binary_matrix(20, 10, seed = 5)
  comps <- lambda_star_components(wide, n_workers = 1L)
  expect_equal(unique(comps$nobs), 20L * 9L)
})

test_that("lambda*_6 rises with more shared signal among items", {
  weak   <- lambda_star_components(make_binary_matrix(60, 15, 6, shared = 0.0),
                                   n_workers = 1L)
  strong <- lambda_star_components(make_binary_matrix(60, 15, 6, shared = 0.9),
                                   n_workers = 1L)
  expect_gt(lambda_star(strong), lambda_star(weak))
})

test_that("aggregation honours the trim argument (Eq. 13)", {
  comps <- data.table::data.table(item = as.character(1:10),
                                  L6 = c(rep(0.5, 9), 100))
  expect_equal(lambda_star(comps, trim = 0), mean(c(rep(0.5, 9), 100)))
  expect_equal(lambda_star(comps, trim = 0.2), 0.5)
})

test_that("missing L6 values are dropped rather than propagated", {
  comps <- data.table::data.table(item = c("a", "b"), L6 = c(0.4, NA_real_))
  expect_equal(lambda_star(comps), 0.4)
})

test_that("an identifier column other than 'model' is accepted", {
  wide <- make_binary_matrix(30, 8, seed = 7)
  data.table::setnames(wide, "model", "subject")
  comps <- lambda_star_components(wide, n_workers = 1L)
  expect_equal(nrow(comps), 8L)
})

test_that("too few items is an error, not a silent NA", {
  wide <- make_binary_matrix(20, 2, seed = 8)
  expect_error(lambda_star_components(wide, n_workers = 1L), "at least 3 items")
})

test_that("a matrix with no identifier column is an error", {
  wide <- data.table::data.table(a = c(0, 1), b = c(1, 0), c = c(1, 1))
  expect_error(lambda_star_components(wide), "identifier column")
})

# --- Identification, Prop. D.1 ------------------------------------------------

test_that("a fully activated design is connected", {
  scores <- matrix(1, nrow = 6, ncol = 5)
  conn <- check_activation_connectivity(scores)
  expect_true(conn$connected)
  expect_equal(conn$n_components, 1L)
  expect_equal(conn$n_edges, 30L)
})

test_that("two disjoint skill blocks give two components", {
  # Models 1-3 solve only items 1-2; models 4-6 solve only items 3-4. The
  # activated bipartite graph splits, so the fixed effects are identified only
  # up to one constant per component (Prop. D.1).
  scores <- matrix(0, nrow = 6, ncol = 4)
  scores[1:3, 1:2] <- 1
  scores[4:6, 3:4] <- 1
  conn <- check_activation_connectivity(scores)
  expect_false(conn$connected)
  expect_equal(conn$n_components, 2L)
  expect_equal(conn$largest_component_share, 0.5)
})

test_that("a single shared item reconnects the design", {
  scores <- matrix(0, nrow = 6, ncol = 4)
  scores[1:3, 1:2] <- 1
  scores[4:6, 3:4] <- 1
  scores[1, 3] <- 1   # one bridging edge
  conn <- check_activation_connectivity(scores)
  expect_true(conn$connected)
  expect_equal(conn$n_components, 1L)
})

test_that("an all-zero design has no activated edges", {
  conn <- check_activation_connectivity(matrix(0, 4, 4))
  expect_equal(conn$n_edges, 0L)
  expect_false(conn$connected)
})

test_that("connectivity checking warns on a disconnected benchmark", {
  # Items 1-4 are solved only by models 1-10, items 5-8 only by models 11-20,
  # so every target item sees a disconnected design over the others.
  wide <- data.table::data.table(model = as.character(1:20))
  for (j in 1:4) wide[[paste0("item_", j)]] <- c(rep(1L, 10), rep(0L, 10))
  for (j in 5:8) wide[[paste0("item_", j)]] <- c(rep(0L, 10), rep(1L, 10))
  expect_warning(
    lambda_star_components(wide, n_workers = 1L, check_connectivity = TRUE),
    "Prop. D.1"
  )
})
