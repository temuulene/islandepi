test_that("the simulated outbreak has the documented shape", {
  expect_s3_class(islh_outbreak, "data.frame")
  expect_equal(nrow(islh_outbreak), 790L)
  expect_named(
    islh_outbreak,
    c(
      "case_id",
      "case_type",
      "sex",
      "age",
      "date_onset",
      "date_reported",
      "date_admission",
      "outcome",
      "date_outcome",
      "hsda",
      "lha_code",
      "lha_name"
    )
  )
  expect_false(anyDuplicated(islh_outbreak$case_id) > 0)
  expect_setequal(
    unique(islh_outbreak$hsda),
    c(
      "South Vancouver Island",
      "Central Vancouver Island",
      "North Vancouver Island"
    )
  )
  expect_true(all(grepl("^4[1-3][1-6]$", islh_outbreak$lha_code)))
})

test_that("the simulated outbreak keeps its dates in order", {
  x <- islh_outbreak
  expect_s3_class(x$date_onset, "Date")
  # Whole days: a line list records calendar dates, not fractions of one.
  expect_equal(as.numeric(x$date_onset), floor(as.numeric(x$date_onset)))
  expect_true(all(x$date_reported >= x$date_onset))
  expect_true(all(x$date_admission >= x$date_onset, na.rm = TRUE))
  expect_true(all(x$date_outcome >= x$date_onset, na.rm = TRUE))
})

test_that("the simulated outbreak passes the event checks", {
  issues <- islh_check_events(
    islh_outbreak,
    id = case_id,
    date = date_onset,
    required = c(hsda, case_type),
    max_date = "2026-03-08"
  )
  expect_equal(nrow(issues), 0L)
})

test_that("the seasonal counts are complete weekly counts with metadata", {
  x <- islh_seasons
  expect_equal(nrow(x), 1095L)
  expect_equal(attr(x, "islh_interval"), "week")
  expect_true(all(weekdays(x$period_start) == "Monday"))
  expect_equal(nrow(x), 3 * length(unique(x$period_start)))
  expect_false(any(x$partial_period))
  season <- islh_reporting_calendar(x$period_start, season_start_month = 9)
  totals <- tapply(x$count, season$season, sum)
  # 2020-21 is the quiet season the examples exclude.
  expect_equal(names(which.min(totals)), "2020-21")
})

test_that("the encounters hold repeat visitors and resent records", {
  x <- islh_encounters
  expect_named(x, c("encounter_id", "person_id", "site", "encounter_date"))
  expect_equal(sum(duplicated(x$encounter_id)), 3L)
  expect_true(any(duplicated(x$person_id)))
  lost <- x$site == "Site C" &
    x$encounter_date %in% as.Date(c("2026-08-12", "2026-08-13"))
  expect_false(any(lost))
})

test_that("the feed log separates missing files from nil reports", {
  log <- islh_feed_log
  coverage <- suppressMessages(islh_check_coverage(
    log,
    date = date,
    by = site,
    expected = c("Site A", "Site B", "Site C"),
    from = "2026-08-03",
    to = "2026-08-30"
  ))
  gaps <- coverage[!coverage$received, ]
  expect_equal(gaps$site, c("Site C", "Site C"))
  expect_equal(gaps$period_start, as.Date(c("2026-08-12", "2026-08-13")))
  expect_true(any(log$site == "Site B" & log$records == 0))
  expect_equal(sum(log$records), nrow(islh_encounters))
})
