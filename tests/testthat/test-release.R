# Every attribute and column attribute whose name starts with "islh_".
package_attributes <- function(x) {
  on_frame <- grep("^islh_", names(attributes(x)), value = TRUE)
  on_columns <- unlist(lapply(x, function(column) {
    grep("^islh_", names(attributes(column)), value = TRUE)
  }))
  c(on_frame, on_columns)
}

suppressed_dsr <- function() {
  rate <- islh_dsr(
    cases = c(1, 2),
    population = c(1000, 2000),
    std_population = c(1000, 2000),
    strata = c("young", "old")
  )
  islh_suppress_table(
    rate,
    "cases",
    threshold = 5,
    linked = list(cases = c("rate", "lower", "upper"))
  )
}

test_that("suppression removes the stratum detail behind a standardized rate", {
  hidden <- suppressed_dsr()

  expect_true(is.na(hidden$cases))
  expect_true(is.na(hidden$rate))
  expect_null(attr(hidden, "islh_dsr_strata"))
  expect_error(
    islh_dsr_detail(hidden),
    "has been through",
    class = "islh_error"
  )
})

test_that("suppression removes the detail even when nothing is hidden", {
  # A total of 50 can rest on a stratum of 1. The stratum counts are a finer
  # table that the rule never looked at.
  rate <- islh_dsr(
    cases = c(1, 49),
    population = c(1000, 2000),
    std_population = c(1000, 2000)
  )
  shown <- islh_suppress_table(rate, "cases", threshold = 5)

  expect_equal(shown$cases, 50)
  expect_null(attr(shown, "islh_dsr_strata"))
  expect_error(islh_dsr_detail(shown), class = "islh_error")
})

test_that("joined standardized rates lose their keyed detail too", {
  standard <- data.frame(age = c("0-64", "65+"), population = c(80, 20))
  rate <- islh_dsr_joined(
    data.frame(age = c("0-64", "65+"), cases = c(1, 2)),
    data.frame(age = c("0-64", "65+"), population = c(1000, 2000)),
    standard,
    by = "age",
    standard_id = "example"
  )
  hidden <- islh_suppress_table(rate, "cases", threshold = 5)

  expect_null(attr(hidden, "islh_dsr_strata"))
  expect_null(attr(hidden, "islh_strata"))
})

test_that("a shared copy carries no package attributes after serialization", {
  hidden <- suppressed_dsr()
  shared <- islh_release_copy(hidden, c("cases", "rate", "lower", "upper"))
  restored <- unserialize(serialize(shared, NULL))

  expect_identical(class(restored), "data.frame")
  expect_identical(names(restored), c("cases", "rate", "lower", "upper"))
  expect_length(package_attributes(restored), 0L)
  expect_true(is.na(restored$cases))
  expect_error(islh_dsr_detail(restored), class = "islh_error")
  expect_error(islh_suppression_audit(restored), class = "islh_error")

  # The withheld total is 3, from strata of 1 and 2. None of those values
  # survives anywhere in the shared object.
  values <- unlist(restored, use.names = FALSE)
  expect_false(any(c(1, 2, 3) %in% values))
})

test_that("the working object keeps its audit for reviewers", {
  hidden <- suppressed_dsr()
  islh_release_copy(hidden, "cases")

  expect_equal(islh_suppression_audit(hidden)$reason[1], "small")
})

test_that("a release copy keeps column types and drops other attributes", {
  data <- data.frame(
    day = as.Date("2026-01-05") + 0:1,
    group = factor(c("a", "b")),
    count = c(4, 9)
  )
  attr(data$count, "islh_note") <- "internal"
  attr(data$count, "label") <- "Count"
  attr(data, "islh_interval") <- "day"
  rownames(data) <- c("x", "y")

  shared <- islh_release_copy(data, c("group", "day", "count"))

  expect_identical(names(shared), c("group", "day", "count"))
  expect_s3_class(shared$day, "Date")
  expect_identical(levels(shared$group), c("a", "b"))
  expect_null(attributes(shared$count))
  expect_identical(rownames(shared), c("1", "2"))
  expect_length(package_attributes(shared), 0L)
})

test_that("a release copy needs approved columns and refuses list columns", {
  hidden <- suppressed_dsr()

  expect_error(islh_release_copy(hidden), "must name the columns to release")
  expect_error(islh_release_copy(hidden, c("rate", "rate")), "more than once")
  expect_error(islh_release_copy(hidden, "strata"), "list column")
  expect_error(islh_release_copy(list(a = 1), "a"), "must be a data frame")
})
