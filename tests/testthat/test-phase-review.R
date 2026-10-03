# Regression cases from the PHASE package review.
raw_population <- function() {
  x <- data.frame(
    Region = 411,
    Region.Name = "Example",
    Region.Type = "Local Health Area",
    Year = 2025,
    Type = "Estimate",
    Gender = c("F", "M", "T"),
    check.names = FALSE
  )
  x[["0"]] <- c(100, 120, 220)
  x
}

test_that("population selections retain the requested denominator only", {
  for (sex in list("F", "T", c("F", "M"))) {
    out <- .islh_tidy_bc_population(
      raw_population(),
      "lha",
      2025,
      sex,
      "five_year",
      NULL
    )
    expect_setequal(unique(out$sex), sex)
    expect_equal(sum(out$population), if (identical(sex, "F")) 100 else 220)
  }
  x <- raw_population()
  expect_error(
    .islh_tidy_bc_population(rbind(x, x), "lha", 2025, "F", "five_year", NULL),
    "duplicate|unique",
    ignore.case = TRUE
  )
  expect_error(
    .islh_tidy_bc_population(x, "lha", c(2024, 2025), "F", "five_year", NULL),
    "missing",
    ignore.case = TRUE
  )
  x[["90+"]] <- 100
  expect_error(
    .islh_tidy_bc_population(x, "lha", 2025, "F", c(0, 90, 95), NULL),
    "split|open",
    ignore.case = TRUE
  )
})

test_that("timestamps use the reporting calendar", {
  stamp <- as.POSIXct("2026-07-05 23:30:00", tz = "America/Vancouver")
  expect_equal(.islh_surv_as_date(stamp, "date"), as.Date("2026-07-05"))
  expect_equal(
    .islh_surv_as_date(stamp, "date", timezone = "UTC"),
    as.Date("2026-07-06")
  )
  expect_error(.islh_surv_as_date(stamp, "date", timezone = "bad"), "timezone")
})

test_that("partial first and last periods are excluded or flagged", {
  x <- data.frame(
    day = seq(as.Date("2026-07-01"), as.Date("2026-07-14"), "day")
  )
  expect_message(
    out <- islh_count_events(x, day, interval = "week"),
    "Left out 7 events in 2 partial periods",
    class = "islh_message"
  )
  expect_equal(out$period_start, as.Date("2026-07-06"))
  expect_equal(out$count, 7)
  partial <- islh_count_events(
    x,
    day,
    interval = "week",
    include_partial = TRUE
  )
  expect_equal(partial$partial_period, c(TRUE, FALSE, TRUE))
  expect_error(
    islh_surveillance_baseline(
      partial,
      period_start,
      count,
      method = "range",
      minimum_periods = 1
    ),
    "partial"
  )
})

test_that("missing coverage and incompatible week anchors cannot look complete", {
  x <- data.frame(day = as.Date("2026-07-01") + 0:6)
  daily <- islh_count_events(x, day)
  expect_error(
    islh_surveillance_snapshot(
      daily[1:3, ],
      period_start,
      count,
      end = "2026-07-07"
    ),
    "missing"
  )
  incomplete <- islh_surveillance_snapshot(
    daily[1:3, ],
    period_start,
    count,
    end = "2026-07-07",
    missing_periods = "missing"
  )
  expect_false(incomplete$complete)
  expect_true(is.na(incomplete$total))
  weekly <- islh_count_events(
    data.frame(day = as.Date("2026-06-29") + 0:13),
    day,
    interval = "week"
  )
  expect_error(
    islh_surveillance_snapshot(
      weekly,
      period_start,
      count,
      interval = "epiweek",
      periods = 2
    ),
    "aligned"
  )
  expect_error(
    islh_count_events(
      weekly,
      period_start,
      count = count,
      interval = "epiweek"
    ),
    "aligned"
  )
  expect_error(
    islh_surveillance_baseline(daily[-3, ], period_start, count),
    "missing"
  )
  expect_error(
    islh_count_events(
      transform(x, site = rep(c("A", "B"), length.out = 7)),
      day,
      by = site,
      groups = "A"
    ),
    "absent"
  )
})

test_that("zero history does not flag zero current activity", {
  x <- data.frame(day = as.Date("2026-06-01") + 0:3, n = 0)
  b <- islh_surveillance_baseline(x, day, n, interval = "day")
  z <- islh_surveillance_snapshot(x[1, ], day, n, periods = 1, baseline = b)
  expect_false(z$exceeds_reference)
  expect_error(
    islh_surveillance_baseline(
      x[1, ],
      day,
      n,
      interval = "day",
      minimum_periods = 1
    ),
    "two"
  )
})

test_that("keyed standardization is invariant to input order", {
  e <- data.frame(age = c("young", "old"), cases = c(4, 20))
  p <- data.frame(age = c("old", "young"), population = c(1000, 5000))
  w <- data.frame(age = c("young", "old"), population = c(6000, 4000))
  out <- islh_dsr_joined(e, p, w, by = "age", standard_id = "Synthetic 2026")
  expect_equal(out$rate, (0.6 * 4 / 5000 + 0.4 * 20 / 1000) * 1e5)
  expect_equal(out$standard_id, "Synthetic 2026")
  expect_error(
    islh_dsr_joined(e, p[1, ], w, by = "age", standard_id = "test"),
    "1 stratum in `events` is missing from `population`"
  )
  expect_error(
    islh_dsr_joined(e, rbind(p, p), w, by = "age", standard_id = "test"),
    "`population` has more than one row"
  )
})

test_that("keyed standardization names the table and column at fault", {
  e <- data.frame(age = c("young", "old"), cases = c(4, 20))
  p <- data.frame(age = c("old", "young"), pop = c(1000, 5000))
  w <- data.frame(age = c("young", "old", "oldest"), population = c(6, 4, 1))
  expect_error(
    islh_dsr_joined(e, p, w[1:2, ], by = "age", standard_id = "test"),
    "`population` has no column named"
  )
  expect_error(
    islh_dsr_joined(
      e,
      p,
      w,
      by = "age",
      denominator = "pop",
      standard_id = "test"
    ),
    "1 stratum in `standard` is not in `events`"
  )
  expect_error(
    islh_dsr_joined(e, p, w, by = "age", cases = "age", standard_id = "t"),
    "`cases` must not be a stratum key"
  )
})

test_that("complementary suppression uses distinct tuple keys and positive cells", {
  out <- islh_suppress_table(
    data.frame(n = c(1, 0, 10)),
    "n",
    5,
    complementary = TRUE
  )
  expect_equal(out$n, c(NA_real_, 0, NA_real_))
  expect_error(
    islh_suppress_table(data.frame(n = c(1, 0)), "n", 5, complementary = TRUE),
    "positive complementary"
  )
  x <- data.frame(
    a = c("A | B", "A | B", "A", "A"),
    b = c("C", "C", "B | C", "B | C"),
    n = c(1, 10, 2, 20)
  )
  out <- islh_suppress_table(x, "n", 5, complementary = TRUE, by = c("a", "b"))
  expect_true(all(is.na(out$n)))
})

test_that("production population coverage validates a full source age grid", {
  raw <- raw_population()
  for (age in 1:89) {
    raw[[as.character(age)]] <- 0
  }
  raw[["90+"]] <- 0
  out <- .islh_tidy_bc_population(
    raw,
    "lha",
    2025,
    "F",
    "five_year",
    NULL,
    validate_age_coverage = TRUE
  )
  expect_equal(sum(out$population), 100)
  expect_error(
    .islh_tidy_bc_population(
      raw[, names(raw) != "1"],
      "lha",
      2025,
      "F",
      "five_year",
      NULL,
      validate_age_coverage = TRUE
    ),
    "coverage"
  )
  projection <- raw
  projection$Type <- "Projection"
  expect_error(
    .islh_tidy_bc_population(
      rbind(raw, projection),
      "lha",
      2025,
      "F",
      "five_year",
      NULL
    ),
    "overlap"
  )
  chosen <- .islh_tidy_bc_population(
    rbind(raw, projection),
    "lha",
    2025,
    "F",
    "five_year",
    NULL,
    estimate_type = "Estimate"
  )
  expect_equal(sum(chosen$population), 100)
})
