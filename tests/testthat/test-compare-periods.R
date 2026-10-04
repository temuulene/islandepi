weekly_counts <- function() {
  data.frame(
    site = rep(c("A", "B"), each = 60),
    week = rep(seq(as.Date("2025-01-06"), by = "week", length.out = 60), 2),
    n = c(seq_len(60), rep(0, 60))
  )
}

test_that("previous period and previous year pick the right windows", {
  out <- islh_compare_periods(
    weekly_counts(),
    week,
    n,
    by = site,
    current = "2026-01-05",
    interval = "week"
  )
  a <- out[out$site == "A", ]
  expect_equal(a$reference_from, as.Date(c("2025-12-29", "2025-01-06")))
  expect_equal(a$current, c(53, 53))
  expect_equal(a$reference, c(52, 1))
  expect_equal(a$difference, c(1, 52))
  expect_equal(a$ratio, c(53 / 52, 53))
  expect_equal(a$percent_change, (c(53 / 52, 53) - 1) * 100)
})

test_that("zero references give a difference but no ratio", {
  out <- islh_compare_periods(
    weekly_counts(),
    week,
    n,
    by = site,
    current = "2026-01-05",
    interval = "week"
  )
  b <- out[out$site == "B", ]
  expect_equal(b$difference, c(0, 0))
  expect_true(all(is.na(b$ratio)))
  expect_true(all(is.na(b$percent_change)))
})

test_that("season to date sums both windows", {
  out <- islh_compare_periods(
    weekly_counts(),
    week,
    n,
    by = site,
    current = "2026-01-05",
    comparison = "season_to_date",
    season_start = "2025-12-01",
    interval = "week"
  )
  a <- out[out$site == "A", ]
  expect_equal(a$current_periods, 6)
  expect_equal(a$current, sum(48:53))
  expect_equal(a$reference_from, as.Date("2024-12-02"))
  expect_true(is.na(a$reference))
})

test_that("a missing period makes the comparison missing, with a message", {
  gappy <- weekly_counts()[-52, ]
  expect_message(
    out <- islh_compare_periods(
      gappy,
      week,
      n,
      by = site,
      current = "2026-01-05",
      comparison = "previous_period",
      interval = "week"
    ),
    "missing period",
    class = "islh_message"
  )
  expect_true(is.na(out$reference[out$site == "A"]))
  expect_equal(out$reference[out$site == "B"], 0)
})

test_that("compare periods uses count metadata and warns on distinct-ID sums", {
  events <- data.frame(
    id = c(1, 1, 2),
    day = as.Date(c("2026-01-05", "2026-01-12", "2026-01-12"))
  )
  weekly <- islh_count_events(
    events,
    day,
    id = id,
    interval = "week",
    from = "2026-01-05",
    to = "2026-01-18"
  )
  expect_warning(
    out <- islh_compare_periods(
      weekly,
      period_start,
      count,
      current = "2026-01-12",
      comparison = c("previous_period", "season_to_date"),
      season_start = "2026-01-05"
    ),
    class = "islh_warning_id_sum"
  )
  expect_equal(out$current, c(2, 3))
})

test_that("compare periods validates its inputs", {
  x <- weekly_counts()
  expect_error(
    islh_compare_periods(
      x,
      week,
      n,
      by = site,
      interval = "week",
      comparison = "season_to_date"
    ),
    "season_start"
  )
  expect_error(
    islh_compare_periods(
      x,
      week,
      n,
      by = site,
      interval = "week",
      current = "2026-01-06"
    ),
    "start of a week"
  )
  expect_error(islh_compare_periods(x, week, n), "more than one row")
  expect_error(
    islh_compare_periods(
      x,
      week,
      n,
      by = site,
      interval = "week",
      comparison = "last_week"
    ),
    "one or more"
  )
  names(x)[1] <- "current"
  expect_error(islh_compare_periods(x, week, n, by = current), "reserved")
})
