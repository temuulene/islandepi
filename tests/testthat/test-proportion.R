# Worked examples from Newcombe (1998), Table II: methods 3 (Wilson score
# without continuity correction) and 5 (Clopper-Pearson), to four decimals.
newcombe <- data.frame(
  x = c(81, 15, 0, 1),
  n = c(263, 148, 20, 29),
  wilson_lower = c(0.2553, 0.0624, 0, 0.0061),
  wilson_upper = c(0.3662, 0.1605, 0.1611, 0.1718),
  exact_lower = c(0.2527, 0.0578, 0, 0.0009),
  exact_upper = c(0.3676, 0.1617, 0.1684, 0.1776)
)

test_that("the Wilson interval matches Newcombe's worked examples", {
  out <- islh_proportion(newcombe$x, newcombe$n)
  expect_equal(round(out$lower, 4), newcombe$wilson_lower)
  expect_equal(round(out$upper, 4), newcombe$wilson_upper)
  expect_equal(out$method, rep("wilson", 4))
})

test_that("the exact interval matches Newcombe's worked examples", {
  out <- islh_proportion(newcombe$x, newcombe$n, method = "exact")
  expect_equal(round(out$lower, 4), newcombe$exact_lower)
  expect_equal(round(out$upper, 4), newcombe$exact_upper)
})

test_that("the exact interval agrees with binom.test", {
  for (i in seq_len(nrow(newcombe))) {
    reference <- stats::binom.test(newcombe$x[i], newcombe$n[i])$conf.int
    out <- islh_proportion(newcombe$x[i], newcombe$n[i], method = "exact")
    expect_equal(c(out$lower, out$upper), as.numeric(reference))
  }
})

test_that("limits are exactly 0 and 1 at the edges", {
  for (method in c("wilson", "exact")) {
    out <- islh_proportion(c(0, 20), 20, method = method)
    expect_identical(out$lower[1], 0)
    expect_identical(out$upper[2], 1)
    expect_true(all(out$lower >= 0 & out$upper <= 1))
  }
})

test_that("per scales the estimate and both limits", {
  unit <- islh_proportion(15, 148)
  percent <- islh_proportion(15, 148, per = 100)
  expect_equal(percent$proportion, unit$proportion * 100)
  expect_equal(percent$lower, unit$lower * 100)
  expect_equal(percent$upper, unit$upper * 100)
  expect_equal(percent$per, 100)
  expect_equal(percent$conf, 0.95)
})

test_that("x and n recycle from length 1 and missing x stays missing", {
  out <- islh_proportion(c(1, NA, 3), 10)
  expect_equal(out$n, c(10, 10, 10))
  expect_true(is.na(out$proportion[2]))
  expect_true(is.na(out$lower[2]))
  expect_error(islh_proportion(1:3, c(10, 10)), "same length")
})

test_that("impossible proportions are refused", {
  expect_error(islh_proportion(5, 4), "larger than", class = "islh_error")
  expect_error(islh_proportion(0, 0), "positive", class = "islh_error")
  expect_error(islh_proportion(1, NA_real_), "missing", class = "islh_error")
  expect_error(islh_proportion(0.5, 10), "whole", class = "islh_error")
  expect_error(islh_proportion(1, 10, conf = 95), "between 0 and 1")
  expect_error(islh_proportion(1, 10, method = "wald"))
})
