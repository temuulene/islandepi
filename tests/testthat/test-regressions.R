# Regression tests for defects found after the v0.1.0 release. Each test
# reproduces the original failure.

# Counts and proportions -----------------------------------------------------

test_that("Wilson limits survive integer counts above 92,681", {
  out <- islh_proportion(50000L, 100000L)
  expect_false(anyNA(c(out$lower, out$upper)))
  expect_equal(out$lower, 0.4969011, tolerance = 1e-6)
  expect_equal(out$upper, 0.5030989, tolerance = 1e-6)
})

test_that("counts within floating-point error are snapped to whole numbers", {
  expect_true(is.na(islh_suppress(5 + 1e-15, threshold = 5)))
  expect_identical(islh_suppress(1e-15, threshold = 5), 0)
  expect_identical(islh_round_base(10, base = 5 + 1e-10), 10)
  expect_identical(.islh_check_counts(c(a = 2L)), c(a = 2))
})

test_that("zero-length inputs give zero-row results", {
  rate <- islh_crude_rate(numeric(0), numeric(0))
  expect_equal(nrow(rate), 0L)
  expect_named(
    rate,
    c("cases", "population", "rate", "lower", "upper", "method", "conf", "per")
  )
  expect_equal(nrow(islh_proportion(numeric(0), numeric(0))), 0L)
  expect_error(
    islh_crude_rate(numeric(0), -5),
    "negative",
    class = "islh_error"
  )
})

# Suppression ----------------------------------------------------------------

test_that("small cells that add up to a small or unique value get a partner", {
  # Two cells adding to 2 must both be 1.
  ones <- islh_suppress_table(
    data.frame(cases = c(1, 1, 40)),
    "cases",
    threshold = 5,
    complementary = TRUE
  )
  expect_true(all(is.na(ones$cases)))

  # Two cells adding to 5 disclose a group of 5.
  small_sum <- islh_suppress_table(
    data.frame(cases = c(3, 2, 42, 60)),
    "cases",
    threshold = 5,
    complementary = TRUE
  )
  expect_equal(small_sum$cases, c(NA, NA, NA, 60))

  # Six ones add to 6, above the threshold, but only one split gives 6.
  six <- islh_suppress_table(
    data.frame(cases = c(rep(1, 6), 40)),
    "cases",
    threshold = 5,
    complementary = TRUE
  )
  expect_true(is.na(six$cases[7]))

  # Two cells adding to 8 under "< 5" can only be 4 and 4.
  top <- islh_suppress_table(
    data.frame(cases = c(4, 4, 40)),
    "cases",
    threshold = 5,
    inclusive = FALSE,
    complementary = TRUE
  )
  expect_true(is.na(top$cases[3]))
  audit <- islh_suppression_audit(top)
  expect_equal(audit$reason, c("small", "small", "complementary"))

  # 3 and 4 add to 7, which several splits give, so nothing more is hidden.
  safe <- islh_suppress_table(
    data.frame(cases = c(3, 4, 40)),
    "cases",
    threshold = 5,
    complementary = TRUE
  )
  expect_equal(safe$cases, c(NA, NA, 40))
})

test_that("complementary suppression needs one label for every hidden cell", {
  counts <- data.frame(cases = c(1, 5, 40))
  expect_error(
    islh_suppress_table(
      counts,
      "cases",
      threshold = 5,
      inclusive = FALSE,
      label = "<5",
      complementary = TRUE
    ),
    "must not state a numeric bound",
    class = "islh_error"
  )
  expect_error(
    islh_suppress_table(
      counts,
      "cases",
      threshold = 5,
      label = "*",
      complementary = TRUE,
      complementary_label = "Suppressed"
    ),
    "must be the same",
    class = "islh_error"
  )
  # A neutral label is used for every hidden cell without repeating it.
  starred <- islh_suppress_table(
    counts,
    "cases",
    threshold = 5,
    inclusive = FALSE,
    label = "*",
    complementary = TRUE
  )
  expect_equal(starred$cases, c("*", "*", "40"))
  expect_error(
    islh_suppress_table(
      counts,
      "cases",
      threshold = 5,
      label = "Suppressed",
      complementary = TRUE,
      complementary_label = NULL
    ),
    "must be the same",
    class = "islh_error"
  )
  # Without complementary suppression a bound label is still allowed.
  out <- islh_suppress_table(
    counts,
    "cases",
    threshold = 5,
    inclusive = FALSE,
    label = "<5"
  )
  expect_equal(out$cases, c("<5", "5", "40"))
})

test_that("a nested list in linked is refused rather than ignored", {
  data <- data.frame(cases = c(2, 50), rate = c(1, 2), lower = c(0.5, 1))
  expect_error(
    islh_suppress_table(
      data,
      "cases",
      threshold = 5,
      linked = list(cases = list(c("rate", "lower")))
    ),
    "column names for each count",
    class = "islh_error"
  )
})

test_that("tables with repeated or empty column names are refused", {
  data <- data.frame(cases = c(100, 200), cases = c(2, 3), check.names = FALSE)
  expect_error(
    islh_suppress_table(data, cols = 2, threshold = 5),
    "unique, non-empty column names",
    class = "islh_error"
  )
  expect_error(
    islh_release_copy(data, cols = 2),
    "unique, non-empty column names",
    class = "islh_error"
  )
})

test_that("a grouped tibble needs by for complementary suppression", {
  data <- dplyr::group_by(
    dplyr::tibble(area = c("X", "X", "Y", "Y"), cases = c(2, 30, 40, 50)),
    area
  )
  expect_error(
    islh_suppress_table(data, "cases", threshold = 5, complementary = TRUE),
    "grouped",
    class = "islh_error"
  )
  out <- islh_suppress_table(
    data,
    "cases",
    threshold = 5,
    complementary = TRUE,
    by = "area"
  )
  expect_equal(out$cases, c(NA, NA, 40, 50))
})

test_that("large counts are written out in full", {
  expect_equal(
    islh_suppress(c(2, 100000, NA), threshold = 5, label = "Suppressed"),
    c("Suppressed", "100000", NA)
  )
  out <- islh_suppress_table(
    data.frame(cases = c(2, 300000), rate = c(1, 250000)),
    "cases",
    threshold = 5,
    linked = list(cases = "rate"),
    linked_label = "Suppressed"
  )
  expect_equal(out$rate, c("Suppressed", "250000"))
})

test_that("suppressing in two calls keeps both records", {
  data <- data.frame(a = c(2, 30), b = c(40, 3))
  once <- islh_suppress_table(data, "a", threshold = 5)
  twice <- islh_suppress_table(once, "b", threshold = 5)
  audit <- islh_suppression_audit(twice)
  expect_equal(audit$column, c("a", "b"))
  expect_equal(audit$row, c(1L, 2L))
})

test_that("labels with braces are shown, not evaluated", {
  out <- islh_suppress_table(
    data.frame(cases = c(1, 10, 20)),
    "cases",
    threshold = 5,
    label = "{x}",
    complementary = TRUE,
    complementary_label = "{x}"
  )
  expect_equal(out$cases, c("{x}", "{x}", "20"))
})

# Release copies -------------------------------------------------------------

test_that("release copies refuse columns they would corrupt", {
  data <- data.frame(area = c("a", "b"))
  data$m <- matrix(1:4, nrow = 2)
  expect_error(
    islh_release_copy(data, c("area", "m")),
    "cannot be copied safely",
    class = "islh_error"
  )

  series <- data.frame(area = c("a", "b"))
  series$t <- stats::ts(1:2)
  expect_error(
    islh_release_copy(series, c("area", "t")),
    "cannot be copied safely",
    class = "islh_error"
  )
})

test_that("release copies convert POSIXlt and ignore sticky sf geometry", {
  data <- data.frame(area = c("a", "b"))
  data$time <- as.POSIXlt(c("2026-01-01", "2026-01-02"), tz = "UTC")
  out <- islh_release_copy(data, c("area", "time"))
  expect_s3_class(out$time, "POSIXct")
  expect_equal(format(out$time), c("2026-01-01", "2026-01-02"))

  skip_if_not_installed("sf")
  points <- sf::st_as_sf(
    data.frame(lha = c("a", "b"), cases = 1:2, x = 1:2, y = 1:2),
    coords = c("x", "y")
  )
  shared <- islh_release_copy(points, c("lha", "cases"))
  expect_named(shared, c("lha", "cases"))
  expect_identical(class(shared), "data.frame")
})

# Keys and comparisons -------------------------------------------------------

test_that("ordered and plain factors match as labels", {
  a <- data.frame(g = factor(c("a", "b"), ordered = TRUE))
  b <- data.frame(g = factor(c("b", "a")))
  expect_equal(.islh_key_match(a, b, "g"), c(2L, 1L))
  c <- data.frame(g = factor("c", levels = c("a", "c"), ordered = TRUE))
  expect_equal(.islh_key_match(a, c, "g"), c(NA_integer_, NA_integer_))
})

test_that("rows only in y keep their key labels", {
  x <- data.frame(area = c("South", "Central"), n = 1:2)
  y <- data.frame(area = factor(c("South", "North")), n = 1:2)
  out <- islh_compare_outputs(x, y, by = "area")
  expect_equal(out$area[out$status == "only_in_y"], "North")

  out <- islh_compare_outputs(
    transform(x, area = factor(area)),
    transform(y, area = as.character(area)),
    by = "area"
  )
  expect_equal(out$area[out$status == "only_in_y"], "North")
})

# Standardized rates ---------------------------------------------------------

test_that("DSR ratios check weights stratum by stratum and the per scale", {
  a <- islh_dsr(
    c(10, 20, 30),
    c(1000, 2000, 3000),
    std_population = c(2, 5, 3),
    strata = c("a", "b", "c")
  )
  permuted <- islh_dsr(
    c(10, 20, 30),
    c(1000, 2000, 3000),
    std_population = c(5, 3, 2),
    strata = c("a", "b", "c")
  )
  expect_error(islh_dsr_ratio(a, permuted), "different standard weights")

  per_1000 <- islh_dsr(
    c(10, 20, 30),
    c(1000, 2000, 3000),
    std_population = c(2, 5, 3),
    strata = c("a", "b", "c"),
    per = 1000
  )
  expect_error(islh_dsr_ratio(a, per_1000), "different `per` scales")

  # Strata in a different order still match by label.
  reordered <- islh_dsr(
    c(30, 10, 20),
    c(3000, 1000, 2000),
    std_population = c(3, 2, 5),
    strata = c("c", "a", "b")
  )
  out <- islh_dsr_ratio(a, reordered)
  expect_equal(out$ratio, 1)
  expect_equal(out$per, 1e5)
})

test_that("a DSR ratio names a standard only when both rates do", {
  a <- islh_dsr(c(10, 20), c(1000, 2000), std_population = c(1, 1))
  b <- islh_dsr(
    c(10, 20),
    c(1000, 2000),
    std_population = c(1, 1),
    standard_id = "Canada 2011"
  )
  expect_true(is.na(islh_dsr_ratio(a, b)$standard_id))
  expect_equal(islh_dsr_ratio(b, b)$standard_id, "Canada 2011")
})

test_that("joined DSRs refuse reserved key names and label their strata", {
  events <- data.frame(weight = c("a", "b"), cases = c(1, 2))
  population <- data.frame(weight = c("a", "b"), population = c(1000, 2000))
  standard <- data.frame(weight = c("a", "b"), w = c(0.5, 0.5))
  expect_error(
    islh_dsr_joined(
      events,
      population,
      standard,
      by = "weight",
      weight = "w",
      standard_id = "x"
    ),
    "reserved",
    class = "islh_error"
  )

  events <- data.frame(age = c("0-64", "65+"), cases = c(1, 2))
  population <- data.frame(
    age = factor(c("65+", "0-64"), ordered = TRUE),
    population = c(2000, 1000)
  )
  standard <- data.frame(age = c("0-64", "65+"), population = c(1, 1))
  out <- islh_dsr_joined(
    events,
    population,
    standard,
    by = "age",
    standard_id = "x"
  )
  expect_equal(out$strata[[1]], c("0-64", "65+"))
  expect_equal(islh_dsr_detail(out)$population, c(1000, 2000))
})

# Surveillance ---------------------------------------------------------------

test_that("an unexpected group's unreported days are unknown, not zero", {
  days <- as.Date("2026-07-01") + 0:6
  feed <- data.frame(site = c(rep("A", 7), "X"), day = c(days, days[1]))
  coverage <- suppressMessages(islh_check_coverage(
    feed,
    day,
    by = site,
    expected = "A",
    from = days[1],
    to = days[7]
  ))
  counts <- islh_count_events(
    data.frame(site = c("A", "X"), day = days[1]),
    day,
    by = site,
    groups = c("A", "X"),
    from = days[1],
    to = days[7]
  )
  out <- suppressMessages(islh_surveillance_snapshot(
    counts,
    period_start,
    count,
    by = site,
    end = days[7],
    periods = 7,
    coverage = coverage,
    include_total = TRUE
  ))
  expect_equal(out$complete, c(TRUE, FALSE, FALSE))
  expect_true(is.na(out$total[out$site == "X"]))
})

test_that("preaggregated counts without metadata are gated", {
  weekly <- data.frame(
    period_start = as.Date(c("2026-07-06", "2026-07-13")),
    count = c(7, 14)
  )
  expect_error(
    islh_count_events(weekly, period_start, count = count, interval = "day"),
    "source_interval",
    class = "islh_error"
  )
  partial <- data.frame(
    period_start = as.Date(c("2026-06-29", "2026-07-06", "2026-07-13")),
    count = c(2, 7, 5),
    partial_period = c(TRUE, FALSE, TRUE)
  )
  expect_error(
    islh_count_events(
      partial,
      period_start,
      count = count,
      interval = "week",
      source_interval = "week"
    ),
    "partial",
    class = "islh_error"
  )
})

test_that("a named group with no preaggregated rows is missing, not zero", {
  daily <- islh_count_events(
    data.frame(site = "A", day = as.Date("2026-07-06") + 0:6),
    day,
    by = site
  )
  expect_error(
    islh_count_events(
      daily,
      period_start,
      count = count,
      by = site,
      groups = c("A", "B"),
      interval = "week"
    ),
    "missing",
    class = "islh_error"
  )
})

test_that("site counts roll up to areas after a site-level check", {
  sites <- islh_count_events(
    data.frame(
      hsda = "North",
      site = c("a", "b", "a"),
      day = as.Date(c("2026-07-06", "2026-07-06", "2026-07-13"))
    ),
    day,
    by = c(hsda, site),
    interval = "week",
    from = as.Date("2026-07-06"),
    to = as.Date("2026-07-19")
  )
  out <- islh_count_events(
    sites,
    period_start,
    count = count,
    by = hsda,
    interval = "week"
  )
  expect_equal(out$count, c(2, 1))

  # A site missing a week is still caught after rolling up.
  dropped <- sites$site == "b" & sites$period_start == as.Date("2026-07-13")
  gap <- sites[!dropped, ]
  attributes(gap)[c("islh_interval", "islh_by", "islh_periods")] <-
    attributes(sites)[c("islh_interval", "islh_by", "islh_periods")]
  expect_error(
    islh_count_events(
      gap,
      period_start,
      count = count,
      by = hsda,
      interval = "week"
    ),
    "missing",
    class = "islh_error"
  )
})

test_that("a coverage gap stays unknown when site counts roll up", {
  days <- as.Date("2026-07-01") + 0:6
  events <- data.frame(
    hsda = "North",
    site = rep(c("a", "b"), each = 6),
    day = rep(days[1:6], 2)
  )
  sites <- islh_count_events(
    events,
    day,
    by = c(hsda, site),
    from = days[1],
    to = days[6]
  )
  coverage <- suppressMessages(islh_check_coverage(
    data.frame(hsda = "North", day = days[1:6]),
    day,
    by = hsda,
    expected = "North",
    from = days[1],
    to = days[7]
  ))
  for (policy in c("error", "missing")) {
    out <- suppressMessages(islh_surveillance_snapshot(
      sites,
      period_start,
      count,
      by = hsda,
      end = days[7],
      periods = 7,
      coverage = coverage,
      missing_periods = policy
    ))
    expect_false(out$complete)
    expect_true(is.na(out[["2026-07-07"]]))
    expect_equal(out[["2026-07-01"]], 2)
  }
})

test_that("a seven-day daily snapshot fits a weekly baseline on any weekday", {
  withr::local_seed(1)
  events <- data.frame(
    day = sample(
      seq(as.Date("2026-01-05"), as.Date("2026-07-12"), "day"),
      2000,
      replace = TRUE
    )
  )
  daily <- suppressMessages(islh_count_events(
    events,
    day,
    from = as.Date("2026-01-05"),
    to = as.Date("2026-07-08")
  ))
  weekly <- suppressMessages(islh_count_events(
    events,
    day,
    interval = "week",
    from = as.Date("2026-01-05"),
    to = as.Date("2026-06-28")
  ))
  baseline <- islh_surveillance_baseline(weekly, period_start, count)
  out <- islh_surveillance_snapshot(
    daily,
    period_start,
    count,
    periods = 7,
    baseline = baseline
  )
  expect_true("exceeds_reference" %in% names(out))
})

test_that("seasons follow whole weeks", {
  out <- islh_reporting_calendar(
    as.Date(c("2026-08-30", "2026-08-31", "2026-09-01", "2026-09-07")),
    season_start_month = 9
  )
  expect_equal(out$season, c("2025-26", "2026-27", "2026-27", "2026-27"))
  expect_equal(out$season_week, c(52L, 1L, 1L, 2L))

  january <- islh_reporting_calendar(
    as.Date(c("2025-12-28", "2025-12-29")),
    season_start_month = 1
  )
  expect_equal(january$season, c("2025", "2026"))
  expect_equal(january$season_week, c(52L, 1L))

  empty <- islh_reporting_calendar(as.Date(character()), season_start_month = 9)
  expect_equal(nrow(empty), 0L)
})

test_that("each date-order rule is checked, and each bad date reported once", {
  data <- data.frame(
    id = 1:2,
    date_onset = as.Date(c("2026-01-01", "2026-01-01")),
    date_admission = c("2026-01-05", "bad"),
    date_outcome = as.Date(c("2026-01-03", "2026-01-10"))
  )
  issues <- islh_check_events(
    data,
    id = id,
    date = date_onset,
    date_order = c(
      date_outcome = "date_onset",
      date_outcome = "date_admission"
    )
  )
  expect_equal(sum(issues$.issue == "date_out_of_order"), 1L)
  expect_equal(sum(issues$.issue == "invalid_date"), 1L)

  expect_error(
    islh_check_events(
      data,
      id = id,
      date = date_onset,
      date_order = c(date_outcome = "date_onset", date_outcome = "date_onset")
    ),
    "repeats",
    class = "islh_error"
  )
})

test_that("period comparisons refuse multi-period rows", {
  data <- data.frame(period_start = as.Date("2026-07-01") + 0:20, count = 1)
  attr(data, "islh_interval") <- "day"
  attr(data, "islh_periods") <- 7L
  expect_error(
    islh_compare_periods(
      data,
      period_start,
      count,
      comparison = "previous_period"
    ),
    "7 periods per row",
    class = "islh_error"
  )
})

test_that("an empty group roster still gives period_start", {
  grid <- .islh_surv_grid(
    data.frame(site = character()),
    as.Date("2026-01-01"),
    "site"
  )
  expect_named(grid, c("site", "period_start"))
})

test_that("infinite whole-number settings are refused", {
  data <- data.frame(day = as.Date("2026-01-01") + 0:6, n = 1)
  expect_error(
    islh_surveillance_snapshot(data, day, n, periods = Inf),
    "positive whole number",
    class = "islh_error"
  )
})

test_that("delay quantile columns are named without padding", {
  data <- data.frame(
    onset = as.Date("2026-01-01") + 0:9,
    report = as.Date("2026-01-03") + 0:9
  )
  out <- suppressMessages(
    islh_reporting_delay(data, onset, report, probs = c(0.5, 0.975))
  )
  expect_true(all(c("delay_p50", "delay_p97.5") %in% names(out)))
})

test_that("a report dated before onset does not set as_of", {
  data <- data.frame(
    onset = as.Date(c("2026-01-01", "2026-01-02", "2027-06-01")),
    report = as.Date(c("2026-01-03", "2026-01-04", "2027-01-01"))
  )
  out <- suppressMessages(
    islh_reporting_delay(data, onset, report, max_delay = 3)
  )
  # The cutoff is 2026-01-04, so the 2 January onset is too recent.
  expect_equal(out$n, 1L)
})

test_that("Farrington refuses an empty table and blanks set-aside weeks", {
  skip_if_not_installed("surveillance")
  expect_error(
    islh_farrington(
      data.frame(week = as.Date(character()), n = numeric()),
      week,
      n,
      from = "2026-01-05"
    ),
    "at least one row",
    class = "islh_error"
  )

  north <- islh_seasons[islh_seasons$hsda == "North Vancouver Island", ]
  recent <- north$period_start >= as.Date("2025-12-29") &
    north$period_start <= as.Date("2026-01-19")
  north$count[recent] <- 0
  out <- islh_farrington(north, period_start, count, from = "2026-01-19")
  expect_equal(out$status, "low_count_rule")
  expect_true(is.na(out$expected))
  expect_true(is.na(out$upper_limit))
  expect_true(is.na(out$pvalue))
})

# Messages -------------------------------------------------------------------

test_that("dynamic text in errors is shown, not evaluated", {
  expect_error(
    .islh_with_bcdata_errors(stop("HTTP 502: {\"error\": \"timeout\"}")),
    "{\"error\": \"timeout\"}",
    fixed = TRUE,
    class = "islh_error"
  )
  expect_error(
    islh_count_events(
      data.frame(site = "{oops}", day = as.Date("2026-07-06")),
      day,
      by = site,
      groups = "A"
    ),
    "{oops}",
    fixed = TRUE,
    class = "islh_error"
  )
})
