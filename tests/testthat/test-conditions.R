# Errors raised inside internal helpers must name the exported function the
# user called, not the helper, and carry the package condition class.

expect_islh_error_from <- function(expr, fn) {
  cnd <- rlang::catch_cnd(expr, classes = "error")
  expect_s3_class(cnd, "islh_error")
  expect_equal(rlang::call_name(cnd$call), fn)
}

test_that("validator errors name the exported function", {
  expect_islh_error_from(islh_suppress(1:5), "islh_suppress")
  expect_islh_error_from(
    islh_suppress(c(-1, 3), threshold = 5),
    "islh_suppress"
  )
  expect_islh_error_from(islh_crude_rate(-1, 100), "islh_crude_rate")
  expect_islh_error_from(islh_round_base(1:3, base = 0), "islh_round_base")
  expect_islh_error_from(
    islh_suppress_table(data.frame(n = 1:3), cols = TRUE, threshold = 5),
    "islh_suppress_table"
  )
})

test_that("surveillance helper errors name the exported function", {
  events <- data.frame(
    day = as.Date("2026-01-01") + 0:2,
    n = c(1, 2.5, 3)
  )
  expect_islh_error_from(
    islh_count_events(events, day, count = n),
    "islh_count_events"
  )
  expect_islh_error_from(
    islh_count_events(events, day, interval = "fortnight"),
    "islh_count_events"
  )
  expect_islh_error_from(
    islh_check_events(events, id = n, date = day, timezone = "Mars/Olympus"),
    "islh_check_events"
  )
})

test_that("errors raised directly by exported functions keep their call", {
  expect_islh_error_from(islh_age_group("40"), "islh_age_group")
  expect_islh_error_from(
    islh_age_group(1, breaks = "decades"),
    "islh_age_group"
  )
})

test_that("warnings and messages carry package classes", {
  expect_warning(islh_age_group(-1), class = "islh_warning")
  expect_message(
    islh_count_events(
      data.frame(day = as.Date("2026-07-01") + 0:13),
      day,
      interval = "week"
    ),
    class = "islh_message"
  )
})

test_that("new helper errors name the exported function", {
  counts <- data.frame(code = "011", cases = 1)
  expect_islh_error_from(
    islh_join_denominator(
      counts,
      data.frame(code = 11, population = 1),
      "code"
    ),
    "islh_join_denominator"
  )
  expect_islh_error_from(
    islh_compare_outputs(counts, counts, by = character()),
    "islh_compare_outputs"
  )
  expect_islh_error_from(
    islh_check_coverage(
      data.frame(day = as.Date("2026-01-01")),
      day,
      from = "2026-01-01",
      to = "2026-01-01",
      timezone = "Mars/Olympus"
    ),
    "islh_check_coverage"
  )
  expect_islh_error_from(islh_proportion(-1, 10), "islh_proportion")
  expect_islh_error_from(
    islh_check_events(
      data.frame(id = 1, day = as.Date("2026-01-01")),
      id,
      day,
      allowed = list(missing = 1)
    ),
    "islh_check_events"
  )
})

test_that("helpers in the reporting calendar functions name the exported function", {
  events <- data.frame(
    onset = as.Date("2026-01-01") + 0:2,
    report = as.Date("2026-01-02") + 0:2
  )
  expect_islh_error_from(
    islh_reporting_calendar("2026-01-01", timezone = "Mars/Olympus"),
    "islh_reporting_calendar"
  )
  expect_islh_error_from(
    islh_reference_periods("2026-01-26", exclude = data.frame(start = 1)),
    "islh_reference_periods"
  )
  expect_islh_error_from(
    islh_reporting_delay(events, onset, report, max_delay = 0),
    "islh_reporting_delay"
  )
  expect_islh_error_from(
    islh_reporting_completeness(
      events,
      onset,
      report,
      as_of = 1,
      max_delay = 2,
      maturity = 2
    ),
    "islh_reporting_completeness"
  )
  expect_islh_error_from(
    islh_suppress_table(
      data.frame(n = 1:3, r = 1),
      "n",
      threshold = 1,
      linked = list(n = "missing")
    ),
    "islh_suppress_table"
  )
  expect_islh_error_from(
    islh_compare_periods(
      data.frame(week = as.Date("2026-01-05"), n = 1),
      week,
      n,
      interval = "fortnight"
    ),
    "islh_compare_periods"
  )
  expect_islh_error_from(
    islh_standard_population("canada_2011", c(0, 18)),
    "islh_standard_population"
  )
})
