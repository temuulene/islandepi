line_list <- function() {
  data.frame(
    site = c("A", "A", "A", "A", "B", "B"),
    onset = as.Date("2026-01-01") + c(0, 0, 1, 10, 0, 2),
    report = as.Date("2026-01-01") + c(1, 3, 2, 11, 5, 4)
  )
}

test_that("delays are summarized per group", {
  out <- islh_reporting_delay(line_list(), onset, report, by = site)
  expect_equal(out$n, c(4, 2))
  expect_equal(out$mean_delay, c(1.5, 3.5))
  expect_equal(out$delay_p50, c(1, 3.5))
  expect_equal(out$longest_delay, c(3, 5))
  expect_named(
    out,
    c("site", "n", "mean_delay", "delay_p50", "delay_p90", "longest_delay")
  )
})

test_that("the cutoff and max_delay leave out records with a message", {
  expect_message(
    out <- islh_reporting_delay(
      line_list(),
      onset,
      report,
      as_of = "2026-01-05",
      max_delay = 3
    ),
    "reported after",
    class = "islh_message"
  )
  # Reports on 6 and 12 January are late, and the 3 January onset is too
  # recent, leaving three records.
  expect_equal(out$n, 3)
  expect_message(
    islh_reporting_delay(
      line_list(),
      onset,
      report,
      as_of = "2026-01-05",
      max_delay = 3
    ),
    "too recent"
  )
})

test_that("unusable and negative delays are left out, not counted", {
  x <- line_list()
  x$report[1] <- NA
  x$report[2] <- x$onset[2] - 1
  expect_message(
    out <- islh_reporting_delay(x, onset, report),
    "before the event"
  )
  expect_equal(out$n, 4)
})

test_that("delay summaries validate inputs", {
  expect_error(
    islh_reporting_delay(line_list(), onset, report, probs = 2),
    "probabilities"
  )
  expect_error(
    islh_reporting_delay(line_list(), onset, report, max_delay = 0),
    "at least 1"
  )
  names_clash <- line_list()
  names(names_clash)[1] <- "n"
  expect_error(
    islh_reporting_delay(names_clash, onset, report, by = n),
    "reserved"
  )
})

test_that("completeness uses only events old enough to learn from", {
  # Delays of 0, 1 or 2 days, equally common, for onsets up to 10 January.
  old <- data.frame(
    onset = rep(as.Date("2026-01-01") + 0:9, each = 3),
    delay = rep(0:2, 10)
  )
  # Recent events, all reported the same day: they must not shorten the
  # learned delays.
  recent <- data.frame(onset = as.Date("2026-01-11") + 0:2, delay = 0)
  x <- rbind(old, recent)
  x$report <- x$onset + x$delay

  out <- islh_reporting_completeness(
    x,
    onset,
    report,
    as_of = "2026-01-13",
    max_delay = 3
  )
  expect_equal(
    out$onset_date,
    as.Date(c("2026-01-11", "2026-01-12", "2026-01-13"))
  )
  expect_equal(out$days_since_onset, c(2, 1, 0))
  expect_equal(out$expected_complete, c(1, 2 / 3, 1 / 3))
  expect_equal(out$reported, c(1, 1, 1))
  expect_equal(unique(out$delay_records), 30)
})

test_that("records reported after as_of are not counted as reported", {
  x <- data.frame(
    onset = as.Date("2026-01-01") + c(0:9, 9),
    report = as.Date("2026-01-01") + c(1:10, 15)
  )
  out <- suppressMessages(islh_reporting_completeness(
    x,
    onset,
    report,
    as_of = "2026-01-11",
    max_delay = 2
  ))
  expect_equal(out$reported[out$onset_date == as.Date("2026-01-10")], 1)
})

test_that("completeness can pool groups or learn per group", {
  x <- data.frame(
    site = rep(c("A", "B"), each = 10),
    onset = rep(as.Date("2026-01-01") + 0:9, 2),
    delay = c(rep(0, 10), rep(1, 10))
  )
  x$report <- x$onset + x$delay
  per_group <- suppressMessages(islh_reporting_completeness(
    x,
    onset,
    report,
    as_of = "2026-01-10",
    max_delay = 2,
    by = site
  ))
  same_day <- per_group[per_group$days_since_onset == 0, ]
  expect_equal(same_day$expected_complete, c(1, 0))
  pooled <- suppressMessages(islh_reporting_completeness(
    x,
    onset,
    report,
    as_of = "2026-01-10",
    max_delay = 2,
    by = site,
    pool = TRUE
  ))
  expect_equal(
    pooled$expected_complete[pooled$days_since_onset == 0],
    c(0.5, 0.5)
  )
})

test_that("completeness requires a cutoff and something to learn from", {
  x <- line_list()
  expect_error(islh_reporting_completeness(x, onset, report), "as_of")
  expect_error(
    islh_reporting_completeness(
      x,
      onset,
      report,
      as_of = "2026-01-03",
      max_delay = 5
    ),
    "old enough"
  )
})
