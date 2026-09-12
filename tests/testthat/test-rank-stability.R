# Tests for item selection and the empirical rank-concordance curves
# (Sec. 3.3, Sec. 4).

test_that("reverse-coded criteria are flipped and renamed", {
  metrics <- tibble::tibble(
    benchmark = "b", item = c("i1", "i2"),
    L6 = c(0.2, 0.8),        # higher is better: untouched
    cneg = c(0.1, 0.9)       # lower is better: flipped to 1 - x, suffixed _rv
  )
  long <- prepare_item_criteria(metrics, metrics = c("L6", "cneg"))

  l6 <- dplyr::filter(long, metric == "L6")
  expect_equal(l6$est, c(0.2, 0.8))

  rv <- dplyr::filter(long, metric == "cneg_rv")
  expect_equal(nrow(rv), 2L)
  expect_equal(rv$est, c(0.9, 0.1))
  expect_false("cneg" %in% long$metric)
})

test_that("infit and outfit are taken in absolute value", {
  # Both tails of infit indicate misfit, so |x| comes first and the reverse
  # coding is applied to that.
  metrics <- tibble::tibble(benchmark = "b", item = c("i1", "i2"),
                            infit = c(-2, 0.5))
  long <- prepare_item_criteria(metrics, metrics = "infit")
  expect_equal(long$metric, c("infit_rv", "infit_rv"))
  expect_equal(long$est, c(1 - 2, 1 - 0.5))
})

test_that("random baselines are reproducible and independent", {
  m <- tibble::tibble(benchmark = "b", item = as.character(1:50))
  a <- add_random_baselines(m, n_draws = 3, seed = 99)
  b <- add_random_baselines(m, n_draws = 3, seed = 99)
  expect_equal(a$rand, b$rand)
  expect_equal(a$rand3, b$rand3)
  # Independent draws, so not equal to each other.
  expect_false(isTRUE(all.equal(a$rand, a$rand2)))
  expect_true(all(a$rand >= 0 & a$rand <= 1))
})

test_that("top-item selection respects the budget and takes the best items", {
  crit <- tibble::tibble(
    benchmark = "b", item = as.character(1:10), metric = "L6",
    est = seq(0.1, 1.0, by = 0.1)
  )
  sel <- select_top_items(crit, n_items = 3)
  expect_equal(nrow(sel), 3L)
  expect_setequal(sel$item, c("8", "9", "10"))
})

test_that("a benchmark with fewer items than the budget keeps all its items", {
  # This is why some reliability curves stop earlier than others (Fig. 2).
  crit <- tibble::tibble(
    benchmark = c(rep("small", 3), rep("big", 20)),
    item = c(paste0("s", 1:3), paste0("b", 1:20)),
    metric = "L6", est = runif(23)
  )
  sel <- select_top_items(crit, n_items = 10)
  expect_equal(sum(sel$benchmark == "small"), 3L)
  expect_equal(sum(sel$benchmark == "big"), 10L)
})

test_that("selection is applied independently per benchmark and criterion", {
  crit <- tidyr::expand_grid(
    benchmark = c("b1", "b2"), metric = c("L6", "rand"),
    item = as.character(1:8)
  ) |> dplyr::mutate(est = runif(dplyr::n()))
  sel <- select_top_items(crit, n_items = 4)
  expect_equal(nrow(sel |> dplyr::distinct(benchmark, metric)), 4L)
  expect_true(all(dplyr::count(sel, benchmark, metric)$n == 4L))
})

test_that("random baselines pool into one series for plotting", {
  df <- tibble::tibble(metric = c("rand", "rand2", "rand7", "L6"))
  expect_equal(pool_random_metrics(df)$metric,
               c("rand", "rand", "rand", "L6"))
})

test_that("cumulative_kendall recovers tau = 1 when the subset ranks like the whole", {
  # Deterministic construction: every item is a perfect copy of the model's
  # ability ordering, so any subset reproduces the full ranking exactly.
  n_models <- 12; n_items <- 6
  ability <- seq_len(n_models)
  responses <- tidyr::expand_grid(model = as.character(ability),
                                  item = paste0("i", seq_len(n_items))) |>
    dplyr::mutate(
      benchmark = "b",
      resp = as.numeric(model),
      samp = "test",
      average = as.numeric(model)
    )
  crit <- tibble::tibble(benchmark = "b", item = paste0("i", seq_len(n_items)),
                         metric = "fixed", estimate = seq_len(n_items))

  br_plan(strategy = "sequential")
  out <- cumulative_kendall(responses, crit, max_items = n_items)

  expect_equal(nrow(out), n_items)
  expect_true(all(abs(out$kcor - 1) < 1e-9))
})

test_that("cumulative_kendall returns near-zero tau for unrelated items", {
  set.seed(11)
  n_models <- 60; n_items <- 40
  responses <- tidyr::expand_grid(model = as.character(seq_len(n_models)),
                                  item = paste0("i", seq_len(n_items))) |>
    dplyr::mutate(benchmark = "b",
                  resp = stats::rbinom(dplyr::n(), 1, 0.5),
                  samp = "test")
  # Reference ranking drawn independently of the responses.
  refs <- tibble::tibble(model = as.character(seq_len(n_models)),
                         average = stats::rnorm(n_models))
  responses <- dplyr::left_join(responses, refs, by = "model")

  crit <- tibble::tibble(benchmark = "b", item = paste0("i", seq_len(n_items)),
                         metric = "rand", estimate = runif(n_items))

  br_plan(strategy = "sequential")
  out <- cumulative_kendall(responses, crit, max_items = n_items)
  expect_lt(abs(mean(out$kcor, na.rm = TRUE)), 0.2)
})

test_that("cumulative_kendall requires its input columns", {
  bad <- tibble::tibble(benchmark = "b", item = "i1", model = "m1", resp = 1)
  crit <- tibble::tibble(benchmark = "b", item = "i1", metric = "m", estimate = 1)
  expect_error(cumulative_kendall(bad, crit), "missing")
})

test_that("the curve is truncated at max_items", {
  n_items <- 20
  responses <- tidyr::expand_grid(model = as.character(1:10),
                                  item = paste0("i", seq_len(n_items))) |>
    dplyr::mutate(benchmark = "b", resp = as.numeric(model),
                  samp = "test", average = as.numeric(model))
  crit <- tibble::tibble(benchmark = "b", item = paste0("i", seq_len(n_items)),
                         metric = "m", estimate = seq_len(n_items))
  br_plan(strategy = "sequential")
  out <- cumulative_kendall(responses, crit, max_items = 5)
  expect_equal(max(out$n_items), 5L)
})
