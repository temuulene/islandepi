test_that("the crude rate ratio matches poisson.test()", {
  cases <- c(22, 205, 0, 3)
  population <- c(80091, 133395, 5000, 12000)
  reference <- c(14, 160, 4, 9)
  reference_population <- c(80091, 133395, 5000, 15000)

  out <- islh_rate_ratio(cases, population, reference, reference_population)
  for (i in seq_along(cases)) {
    test <- stats::poisson.test(
      c(cases[i], reference[i]),
      c(population[i], reference_population[i])
    )
    expect_equal(out$ratio[i], unname(test$estimate))
    expect_equal(c(out$lower[i], out$upper[i]), as.numeric(test$conf.int))
  }
  expect_equal(out$method, rep("exact", 4))
})

test_that("the review's two worked comparisons are reproduced", {
  # Comox Valley 22 vs 14, Nanaimo 205 vs 160, equal populations.
  out <- islh_rate_ratio(c(22, 205), 1, c(14, 160), 1)
  expect_equal(round(out$ratio, 2), c(1.57, 1.28))
  expect_equal(round(out$lower, 2), c(0.77, 1.04))
  expect_equal(round(out$upper, 2), c(3.32, 1.59))
})

test_that("a reference with no cases gives no ratio", {
  out <- islh_rate_ratio(3, 1000, 0, 1000)
  expect_true(is.na(out$ratio))
  expect_true(is.na(out$lower))
  expect_true(is.na(out$upper))
})

test_that("the crude rate ratio checks its inputs", {
  expect_error(islh_rate_ratio(1:3, 1, 1:2, 1), "common length")
  expect_error(islh_rate_ratio(-1, 1, 1, 1), class = "islh_error")
  expect_error(islh_rate_ratio(1, 0, 1, 1), class = "islh_error")
  expect_error(islh_rate_ratio(1, 1, 1, 1, conf = 95), "between 0 and 1")
})

one_stratum <- function(cases, population) {
  islh_dsr(cases, population, std_population = 1)
}

test_that("with one stratum the standardized ratio is the exact one", {
  pairs <- list(c(22, 14), c(205, 160), c(0, 4), c(3, 1))
  for (pair in pairs) {
    x <- one_stratum(pair[1], 50000)
    reference <- one_stratum(pair[2], 70000)
    out <- islh_dsr_ratio(x, reference)
    exact <- islh_rate_ratio(pair[1], 50000, pair[2], 70000)
    expect_equal(out$ratio, exact$ratio)
    expect_equal(out$lower, exact$lower)
    expect_equal(out$upper, exact$upper)
  }
})

dsr_pair <- function() {
  standard <- c(30000, 30000, 25000, 15000)
  list(
    x = islh_dsr(c(2, 9, 14, 30), c(20000, 25000, 22000, 9000), standard),
    reference = islh_dsr(
      c(6, 25, 31, 60),
      c(80000, 90000, 70000, 30000),
      standard
    )
  )
}

test_that("swapping the rates inverts the interval", {
  rates <- dsr_pair()
  forward <- islh_dsr_ratio(rates$x, rates$reference)
  backward <- islh_dsr_ratio(rates$reference, rates$x)
  expect_equal(forward$ratio, 1 / backward$ratio)
  expect_equal(forward$lower, 1 / backward$upper)
  expect_equal(forward$upper, 1 / backward$lower)
  expect_lt(forward$lower, forward$ratio)
  expect_gt(forward$upper, forward$ratio)
})

test_that("against a near-certain reference it is Fay and Feuer's interval", {
  standard <- c(30000, 30000, 25000, 15000)
  x <- islh_dsr(c(2, 9, 14, 30), c(20000, 25000, 22000, 9000), standard)
  # A reference so large its rate is known almost exactly.
  big <- islh_dsr(
    c(2, 9, 14, 30) * 1e5,
    c(20000, 25000, 22000, 9000) * 1e5,
    standard
  )
  out <- islh_dsr_ratio(x, big)
  expect_equal(out$lower, x$lower / big$rate, tolerance = 1e-3)
  expect_equal(out$upper, x$upper / big$rate, tolerance = 1e-3)
})

test_that("a wider confidence level gives a wider interval", {
  rates <- dsr_pair()
  narrow <- islh_dsr_ratio(rates$x, rates$reference, conf = 0.9)
  wide <- islh_dsr_ratio(rates$x, rates$reference, conf = 0.99)
  expect_lt(wide$lower, narrow$lower)
  expect_gt(wide$upper, narrow$upper)
})

test_that("standardized ratios refuse rates they cannot compare", {
  rates <- dsr_pair()
  other <- islh_dsr(
    c(2, 9, 14, 30),
    c(20000, 25000, 22000, 9000),
    c(10000, 20000, 30000, 40000)
  )
  expect_error(
    islh_dsr_ratio(rates$x, other),
    "different standard weights",
    class = "islh_error"
  )

  named <- function(id) {
    islh_dsr(c(2, 9), c(2000, 900), c(3, 1), standard_id = id)
  }
  expect_error(islh_dsr_ratio(named("A"), named("B")), "different standards")

  expect_error(islh_dsr_ratio(data.frame(rate = 1), rates$x), "one rate")

  suppressed <- rates$x
  attr(suppressed, "islh_detail_removed") <- TRUE
  expect_error(islh_dsr_ratio(suppressed, rates$x), "islh_suppress_table")
})

test_that("a zero reference rate gives no ratio", {
  x <- one_stratum(3, 1000)
  zero <- one_stratum(0, 1000)
  expect_message(out <- islh_dsr_ratio(x, zero), "reference rate is zero")
  expect_true(is.na(out$ratio))
  expect_true(is.na(out$upper))
})
