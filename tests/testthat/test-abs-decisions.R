test_that("absolute SEM exceeds relative SEM unless item variance is zero", {
  expect_gt(sem_absolute(0.5, 1, 10), sem_relative(1, 10))
  expect_equal(sem_absolute(0, 1, 10), sem_relative(1, 10))
})

test_that("both SEMs follow the 1/sqrt(I) law", {
  expect_equal(sem_relative(1, 100), sem_relative(1, 1) / 10)
  expect_equal(sem_absolute(0.5, 1, 100), sem_absolute(0.5, 1, 1) / 10)
})

test_that("the SEM ratio is free of the item count", {
  a <- sem_absolute(0.5, 1, 7) / sem_relative(1, 7)
  b <- sem_absolute(0.5, 1, 700) / sem_relative(1, 700)
  expect_equal(a, b)
})

test_that("score intervals are clamped and flagged at the boundary", {
  iv <- score_interval(0.05, 0.1)
  expect_equal(iv$lower, 0)
  expect_true(iv$clamped)
  expect_false(score_interval(0.5, 0.01)$clamped)
})