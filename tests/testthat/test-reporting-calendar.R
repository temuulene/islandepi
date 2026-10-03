test_that("week years follow ISO and CDC rules at the year boundary", {
  cal <- islh_reporting_calendar(c(
    "2025-12-28",
    "2025-12-29",
    "2026-01-03",
    "2026-01-04",
    "2027-01-03"
  ))
  expect_equal(cal$iso_year, c(2025, 2026, 2026, 2026, 2026))
  expect_equal(cal$iso_week, c(52, 1, 1, 1, 53))
  expect_equal(cal$epi_year, c(2025, 2025, 2025, 2026, 2027))
  expect_equal(cal$epi_week, c(53, 53, 53, 1, 1))
  expect_equal(cal$isoweek_start[2], as.Date("2025-12-29"))
  expect_equal(cal$epiweek_start[1], as.Date("2025-12-28"))
})

test_that("week starts join to islh_count_events periods", {
  events <- data.frame(day = as.Date("2026-01-05") + 0:13)
  weekly <- islh_count_events(
    events,
    day,
    interval = "week",
    week_start = 1,
    from = "2026-01-05",
    to = "2026-01-18"
  )
  cal <- islh_reporting_calendar(events$day, week_start = 1)
  expect_setequal(unique(cal$week_start_date), weekly$period_start)
})

test_that("fiscal years and quarters run April to March by default", {
  cal <- islh_reporting_calendar(c("2026-03-31", "2026-04-01", "2026-10-15"))
  expect_equal(cal$fiscal_year, c("2025/26", "2026/27", "2026/27"))
  expect_equal(cal$fiscal_quarter, c(4, 1, 3))
  calendar_year <- islh_reporting_calendar("2026-03-31", fiscal_start_month = 1)
  expect_equal(calendar_year$fiscal_year, "2026")
  expect_equal(calendar_year$fiscal_quarter, 1)
})

test_that("seasons are only added on request", {
  expect_false("season" %in% names(islh_reporting_calendar("2026-01-01")))
  cal <- islh_reporting_calendar(
    c("2025-08-31", "2025-09-01", "2025-09-08"),
    season_start_month = 9
  )
  expect_equal(cal$season, c("2024-25", "2025-26", "2025-26"))
  expect_equal(cal$season_week[2:3], c(1, 2))
  expect_error(islh_reporting_calendar("2026-01-01", season_start_month = 13))
  expect_error(islh_reporting_calendar("2026-13-01"), "invalid")
})

test_that("reference periods match the week in each earlier year", {
  ref <- islh_reference_periods("2026-01-26", years_back = 5, window = 2)
  expect_length(ref, 25)
  expect_true(all(weekdays(ref) == "Monday"))
  expect_true(all(ref < as.Date("2026-01-26")))
  # Each centre is the Monday nearest the same date a year earlier.
  centres <- ref[seq(3, 25, by = 5)]
  expect_equal(
    centres,
    as.Date(c(
      "2021-01-25",
      "2022-01-24",
      "2023-01-23",
      "2024-01-29",
      "2025-01-27"
    ))
  )
})

test_that("epidemiological weeks keep their week number", {
  ref <- islh_reference_periods(
    "2026-01-04",
    years_back = 3,
    window = 0,
    interval = "epiweek"
  )
  expect_equal(lubridate::epiweek(ref), c(1, 1, 1))
  # Week 53 maps to week 52 in a year without one.
  back <- islh_reference_periods(
    "2025-12-28",
    years_back = 1,
    window = 0,
    interval = "epiweek"
  )
  expect_equal(lubridate::epiweek(back), 52)
})

test_that("exclusions remove overlapping periods", {
  ref <- islh_reference_periods(
    "2026-01-26",
    years_back = 5,
    window = 2,
    exclude = data.frame(from = "2020-09-01", to = "2021-08-31")
  )
  expect_length(ref, 20)
  expect_false(any(format(ref, "%Y") == "2021"))
  one_week <- islh_reference_periods(
    "2026-01-26",
    years_back = 1,
    window = 2,
    exclude = as.Date("2025-01-29")
  )
  expect_false(as.Date("2025-01-27") %in% one_week)
  expect_length(one_week, 4)
  expect_error(
    islh_reference_periods(
      "2026-01-26",
      years_back = 1,
      window = 0,
      exclude = as.Date("2025-01-28")
    ),
    "No reference periods"
  )
})

test_that("reference periods validate their inputs", {
  expect_error(islh_reference_periods("2026-01-27"), "start of a week")
  expect_error(
    islh_reference_periods("2026-01-26", years_back = 0),
    "at least 1"
  )
  expect_error(islh_reference_periods("2026-01-26", window = -1), "at least 0")
  monthly <- islh_reference_periods("2026-02-01", 2, 1, interval = "month")
  expect_equal(format(monthly, "%m"), rep(c("01", "02", "03"), 2))
})
