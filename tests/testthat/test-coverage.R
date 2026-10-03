submissions <- function() {
  data.frame(
    site = c(rep(c("A", "B"), each = 7), rep("C", 5)),
    date = as.Date("2026-08-01") + c(0:6, 0:6, 0, 1, 4, 5, 6)
  )
}

test_that("missing feeds are marked, not treated as zero", {
  expect_message(
    out <- islh_check_coverage(
      submissions(),
      date = date,
      by = site,
      expected = c("A", "B", "C"),
      from = "2026-08-01",
      to = "2026-08-07"
    ),
    "2 of 21",
    class = "islh_message"
  )
  expect_equal(nrow(out), 21)
  gaps <- out[!out$received, ]
  expect_equal(gaps$site, c("C", "C"))
  expect_equal(gaps$period_start, as.Date(c("2026-08-03", "2026-08-04")))
  expect_true(all(gaps$records == 0L))
  expect_true(all(out$expected))
})

test_that("a site that never reported still appears", {
  out <- suppressMessages(islh_check_coverage(
    submissions(),
    date = date,
    by = site,
    expected = c("A", "B", "C", "D"),
    from = "2026-08-01",
    to = "2026-08-07"
  ))
  expect_equal(sum(out$site == "D"), 7)
  expect_false(any(out$received[out$site == "D"]))
})

test_that("unexpected groups are kept and named", {
  log <- rbind(
    submissions(),
    data.frame(site = "Z", date = as.Date("2026-08-02"))
  )
  expect_message(
    out <- islh_check_coverage(
      log,
      date = date,
      by = site,
      expected = c("A", "B", "C"),
      from = "2026-08-01",
      to = "2026-08-07"
    ),
    "Z"
  )
  extra <- out[!out$expected, ]
  expect_equal(extra$site, "Z")
  expect_true(extra$received)
})

test_that("full coverage is quiet and counts repeat submissions", {
  log <- data.frame(
    date = as.Date("2026-08-01") + c(0, 0, 1:6)
  )
  expect_no_message(
    out <- islh_check_coverage(
      log,
      date,
      from = "2026-08-01",
      to = "2026-08-07"
    )
  )
  expect_equal(out$records, c(2L, rep(1L, 6)))
  expect_true(all(out$received))
})

test_that("weekly coverage uses period boundaries", {
  log <- data.frame(
    site = c("A", "A", "B"),
    date = as.Date(c("2026-08-03", "2026-08-12", "2026-08-05"))
  )
  out <- suppressMessages(islh_check_coverage(
    log,
    date,
    by = site,
    expected = c("A", "B"),
    from = "2026-08-03",
    to = "2026-08-16",
    interval = "week",
    week_start = 1
  ))
  expect_equal(nrow(out), 4)
  expect_equal(out$received, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(out$period_end[1], as.Date("2026-08-09"))
  expect_error(
    islh_check_coverage(
      log,
      date,
      by = site,
      expected = c("A", "B"),
      from = "2026-08-01",
      to = "2026-08-16",
      interval = "week"
    ),
    "boundaries"
  )
})

test_that("coverage refuses to guess the roster or the window", {
  expect_error(
    islh_check_coverage(
      submissions(),
      date,
      by = site,
      from = "2026-08-01",
      to = "2026-08-07"
    ),
    "expected",
    class = "islh_error"
  )
  expect_error(
    islh_check_coverage(submissions(), date, by = site, expected = "A"),
    "from",
    class = "islh_error"
  )
  log <- submissions()
  names(log)[1] <- "received"
  expect_error(
    islh_check_coverage(
      log,
      date,
      by = received,
      expected = "A",
      from = "2026-08-01",
      to = "2026-08-07"
    ),
    "reserved"
  )
})

test_that("received periods can gate a snapshot", {
  events <- data.frame(
    site = c("A", "C", "C"),
    date = as.Date(c("2026-08-02", "2026-08-01", "2026-08-06"))
  )
  daily <- islh_count_events(
    events,
    date = date,
    by = site,
    from = "2026-08-01",
    to = "2026-08-07",
    groups = c("A", "B", "C")
  )
  coverage <- suppressMessages(islh_check_coverage(
    submissions(),
    date,
    by = site,
    expected = c("A", "B", "C"),
    from = "2026-08-01",
    to = "2026-08-07"
  ))
  known <- merge(daily, coverage[coverage$received, c("site", "period_start")])
  snapshot <- islh_surveillance_snapshot(
    known,
    date = period_start,
    value = count,
    by = site,
    periods = 7,
    interval = "day",
    missing_periods = "missing"
  )
  expect_equal(snapshot$complete, c(TRUE, TRUE, FALSE))
  expect_true(is.na(snapshot$total[3]))
})
