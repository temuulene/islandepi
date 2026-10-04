# Whole workflows, not single validators: coverage, partial periods and source
# periods have to survive the step from one function to the next.

two_site_week <- function() {
  events <- data.frame(site = "A", day = as.Date("2026-01-05") + 0:6)
  daily <- islh_count_events(
    events,
    day,
    by = site,
    groups = c("A", "B"),
    from = "2026-01-05",
    to = "2026-01-11"
  )
  coverage <- suppressMessages(islh_check_coverage(
    events,
    day,
    by = site,
    expected = c("A", "B"),
    from = "2026-01-05",
    to = "2026-01-11"
  ))
  list(daily = daily, coverage = coverage)
}

# Feed coverage --------------------------------------------------------------

test_that("a site that sent nothing stays in the snapshot as unknown", {
  x <- two_site_week()
  out <- suppressMessages(islh_surveillance_snapshot(
    x$daily,
    period_start,
    count,
    by = site,
    periods = 7,
    interval = "day",
    include_total = TRUE,
    coverage = x$coverage
  ))

  expect_equal(out$site, c("A", "B", "All"))
  expect_equal(out$total, c(7, NA, NA))
  expect_equal(out$complete, c(TRUE, FALSE, FALSE))
})

test_that("filled zeros for an unreceived feed become unknown", {
  # islh_count_events() filled B with zeros because B was in `groups`. The
  # coverage check says B sent nothing, so those zeros are not real.
  x <- two_site_week()
  expect_true(all(x$daily$count[x$daily$site == "B"] == 0))

  out <- suppressMessages(islh_surveillance_snapshot(
    x$daily,
    period_start,
    count,
    by = site,
    periods = 7,
    interval = "day",
    coverage = x$coverage
  ))
  b <- out[out$site == "B", format(as.Date("2026-01-05") + 0:6)]
  expect_true(all(is.na(unlist(b))))
})

test_that("removing unreceived periods before the snapshot also works", {
  x <- two_site_week()
  known <- merge(
    x$daily,
    x$coverage[x$coverage$received, c("site", "period_start")]
  )
  expect_false("B" %in% known$site)

  expect_message(
    out <- islh_surveillance_snapshot(
      known,
      period_start,
      count,
      by = site,
      periods = 7,
      interval = "day",
      source_interval = "day",
      include_total = TRUE,
      coverage = x$coverage
    ),
    "unknown"
  )
  expect_equal(out$site, c("A", "B", "All"))
  expect_equal(out$complete, c(TRUE, FALSE, FALSE))
  expect_true(is.na(out$total[out$site == "All"]))
})

test_that("with no feed at all, every group is reported as unknown", {
  log <- data.frame(site = character(), day = as.Date(character()))
  coverage <- suppressMessages(islh_check_coverage(
    log,
    day,
    by = site,
    expected = c("A", "B"),
    from = "2026-01-05",
    to = "2026-01-11"
  ))
  counts <- data.frame(
    site = character(),
    day = as.Date(character()),
    n = numeric()
  )

  out <- suppressMessages(islh_surveillance_snapshot(
    counts,
    day,
    n,
    by = site,
    periods = 7,
    interval = "day",
    include_total = TRUE,
    coverage = coverage
  ))
  expect_equal(out$site, c("A", "B", "All"))
  expect_true(all(is.na(out$total)))
  expect_false(any(out$complete))
})

test_that("without coverage, empty counts still stop with a pointer", {
  counts <- data.frame(day = as.Date(character()), n = numeric())
  expect_error(
    islh_surveillance_snapshot(counts, day, n, source_interval = "day"),
    "coverage",
    class = "islh_error"
  )
})

test_that("coverage defaults the snapshot window to its own end", {
  # The last two days have no events and no feed. Ending at the latest event
  # would quietly move the window back two days.
  events <- data.frame(site = "A", day = as.Date("2026-01-05") + 0:4)
  daily <- islh_count_events(
    events,
    day,
    by = site,
    from = "2026-01-05",
    to = "2026-01-09"
  )
  coverage <- suppressMessages(islh_check_coverage(
    events,
    day,
    by = site,
    expected = "A",
    from = "2026-01-05",
    to = "2026-01-11"
  ))
  out <- suppressMessages(islh_surveillance_snapshot(
    daily,
    period_start,
    count,
    by = site,
    periods = 7,
    interval = "day",
    missing_periods = "missing",
    coverage = coverage
  ))
  expect_true("2026-01-11" %in% names(out))
  expect_false(out$complete)
})

test_that("coverage must list every group and span the window", {
  x <- two_site_week()
  extra <- x$daily
  extra$site[1] <- "C"
  expect_error(
    suppressMessages(islh_surveillance_snapshot(
      extra,
      period_start,
      count,
      by = site,
      periods = 7,
      interval = "day",
      coverage = x$coverage
    )),
    "does not list",
    class = "islh_error"
  )
  expect_error(
    suppressMessages(islh_surveillance_snapshot(
      x$daily,
      period_start,
      count,
      by = site,
      end = "2026-01-12",
      periods = 7,
      interval = "day",
      missing_periods = "missing",
      coverage = x$coverage
    )),
    "does not span",
    class = "islh_error"
  )
  expect_error(
    islh_surveillance_snapshot(
      x$daily,
      period_start,
      count,
      by = site,
      periods = 7,
      interval = "day",
      coverage = data.frame(site = "A")
    ),
    "islh_check_coverage",
    class = "islh_error"
  )
})

# Partial periods ------------------------------------------------------------

partial_weeks <- function() {
  events <- data.frame(day = as.Date("2026-01-05") + 0:9)
  islh_count_events(
    events,
    day,
    interval = "week",
    from = "2026-01-05",
    to = "2026-01-14",
    include_partial = TRUE
  )
}

test_that("a partial current week is not compared as a whole week", {
  weekly <- partial_weeks()
  expect_equal(weekly$count, c(7, 3))
  expect_equal(weekly$partial_period, c(FALSE, TRUE))

  expect_error(
    islh_compare_periods(
      weekly,
      period_start,
      count,
      current = "2026-01-12",
      comparison = "previous_period"
    ),
    "partial",
    class = "islh_error"
  )
})

test_that("a partial reference week is refused too", {
  weekly <- partial_weeks()
  weekly$partial_period <- c(TRUE, FALSE)
  expect_error(
    islh_compare_periods(
      weekly,
      period_start,
      count,
      current = "2026-01-12",
      comparison = "previous_period"
    ),
    "partial",
    class = "islh_error"
  )
})

test_that("an unknown partial flag is treated as partial", {
  weekly <- partial_weeks()
  weekly$partial_period <- c(FALSE, NA)
  expect_error(
    islh_compare_periods(
      weekly,
      period_start,
      count,
      current = "2026-01-12",
      comparison = "previous_period"
    ),
    "partial",
    class = "islh_error"
  )
})

test_that("subsetting keeps the partial flag and the guard", {
  weekly <- partial_weeks()
  subset <- weekly[weekly$count >= 0, ]
  expect_error(
    islh_compare_periods(
      subset,
      period_start,
      count,
      interval = "week",
      current = "2026-01-12",
      comparison = "previous_period"
    ),
    "partial",
    class = "islh_error"
  )
})

test_that("partial periods outside the compared windows do not matter", {
  weekly <- partial_weeks()
  out <- islh_compare_periods(
    weekly[1, ],
    period_start,
    count,
    current = "2026-01-05",
    comparison = "previous_period"
  )
  expect_true(is.na(out$reference))
})

test_that("Farrington refuses a partial week in the assessment or history", {
  skip_if_not_installed("surveillance")
  north <- islh_seasons[islh_seasons$hsda == "North Vancouver Island", ]

  assessed <- north
  assessed$partial_period <- assessed$period_start == as.Date("2026-01-19")
  expect_error(
    islh_farrington(assessed, period_start, count, from = "2026-01-19"),
    "partial",
    class = "islh_error"
  )

  history <- north
  history$partial_period <- history$period_start == as.Date("2023-01-16")
  expect_error(
    islh_farrington(history, period_start, count, from = "2026-01-19"),
    "partial",
    class = "islh_error"
  )
})

# Source periods -------------------------------------------------------------

test_that("the same sparse dates follow the declared source period", {
  sparse <- data.frame(
    day = as.Date(c("2026-01-05", "2026-01-12")),
    n = c(1, 1)
  )

  # Two counts a week apart: weekly totals, or daily counts with the days in
  # between missing? The snapshot will not guess.
  expect_error(
    islh_surveillance_snapshot(
      sparse,
      day,
      n,
      interval = "week",
      periods = 2,
      end = "2026-01-12",
      missing_periods = "missing"
    ),
    "source_interval",
    class = "islh_error"
  )

  as_weeks <- islh_surveillance_snapshot(
    sparse,
    day,
    n,
    interval = "week",
    periods = 2,
    end = "2026-01-12",
    missing_periods = "missing",
    source_interval = "week"
  )
  expect_equal(as_weeks$total, 2)
  expect_true(as_weeks$complete)

  as_days <- islh_surveillance_snapshot(
    sparse,
    day,
    n,
    interval = "week",
    periods = 2,
    end = "2026-01-12",
    missing_periods = "missing",
    source_interval = "day"
  )
  expect_true(is.na(as_days$total))
  expect_false(as_days$complete)
})

test_that("consecutive days are read as daily counts without a declaration", {
  daily <- data.frame(day = as.Date("2026-01-05") + 0:6, n = 1)
  out <- islh_surveillance_snapshot(daily, day, n, periods = 7)
  expect_equal(out$total, 7)
})

test_that("a declared source period must agree with recorded metadata", {
  daily <- islh_count_events(
    data.frame(day = as.Date("2026-01-05") + 0:6),
    day,
    from = "2026-01-05",
    to = "2026-01-11"
  )
  expect_error(
    islh_surveillance_snapshot(
      daily,
      period_start,
      count,
      periods = 7,
      source_interval = "week"
    ),
    "does not match",
    class = "islh_error"
  )
})
