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

test_that("the 2025-26 season is the outbreak counted by week of onset", {
  current <- islh_seasons[islh_seasons$period_start >= as.Date("2025-09-01"), ]
  expect_equal(sum(current$count), nrow(islh_outbreak))

  onset_week <- islh_outbreak$date_onset -
    (as.integer(format(islh_outbreak$date_onset, "%u")) - 1L)
  from_line_list <- as.data.frame(
    table(hsda = islh_outbreak$hsda, period_start = onset_week),
    stringsAsFactors = FALSE
  )
  from_line_list$period_start <- as.Date(from_line_list$period_start)
  matched <- merge(current, from_line_list, by = c("hsda", "period_start"))
  expect_equal(matched$count, matched$Freq)
  expect_equal(sum(current$count[current$count > 0]), sum(matched$Freq))
})

test_that("the LHA population covers every area, sex and age group", {
  x <- islh_lha_population
  expect_equal(nrow(x), 14L * 2L * 18L)
  expect_setequal(unique(x$geography_code), unique(islh_outbreak$lha_code))
  expect_equal(unique(x$year), 2025L)
  expect_setequal(unique(x$sex), c("F", "M"))
  expect_true(is.ordered(x$age_group))
  expect_equal(nlevels(x$age_group), 18L)
  expect_false(anyDuplicated(x[c("geography_code", "sex", "age_group")]) > 0)

  # Totals match the 2025 populations used to place the outbreak's cases.
  totals <- tapply(x$population, x$geography_name, sum)
  expect_equal(unname(totals[["Greater Victoria"]]), 258999)
  expect_equal(sum(x$population), 927705)
})
