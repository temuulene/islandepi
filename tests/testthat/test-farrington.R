test_that("a planted spike raises an alarm and quiet weeks do not", {
  skip_if_not_installed("surveillance")
  north <- islh_seasons[islh_seasons$hsda == "North Vancouver Island", ]
  spike <- north$period_start == as.Date("2026-01-12")
  north$count[spike] <- north$count[spike] + 200
  out <- islh_farrington(
    north,
    period_start,
    count,
    from = "2026-01-05",
    to = "2026-01-19"
  )
  expect_named(
    out,
    c(
      "period_start",
      "observed",
      "expected",
      "upper_limit",
      "alarm",
      "pvalue",
      "status"
    )
  )
  expect_equal(
    out$period_start,
    as.Date(c("2026-01-05", "2026-01-12", "2026-01-19"))
  )
  expect_true(out$alarm[2])
  expect_gt(out$observed[2], out$upper_limit[2])
  expect_false(out$alarm[1])
})

test_that("groups are modelled separately and labelled", {
  skip_if_not_installed("surveillance")
  out <- islh_farrington(
    islh_seasons,
    period_start,
    count,
    by = hsda,
    from = "2026-01-05"
  )
  expect_equal(nrow(out), 3)
  expect_setequal(out$hsda, unique(islh_seasons$hsda))
  expect_true(all(out$upper_limit >= 0))
})

test_that("Farrington refuses tables it cannot model", {
  skip_if_not_installed("surveillance")
  expect_error(
    islh_farrington(
      islh_seasons,
      period_start,
      count,
      by = hsda,
      from = "2021-01-04"
    ),
    "far enough"
  )
  gappy <- islh_seasons[-10, ]
  expect_error(
    islh_farrington(gappy, period_start, count, by = hsda, from = "2026-01-05"),
    "missing weeks"
  )
  expect_error(
    islh_farrington(
      islh_seasons,
      period_start,
      count,
      by = hsda,
      from = "2026-01-06"
    ),
    "week start"
  )
  expect_error(
    islh_farrington(islh_seasons, period_start, count, by = hsda),
    "from"
  )
  expect_error(
    islh_farrington(
      islh_seasons,
      period_start,
      count,
      by = hsda,
      from = "2026-01-05",
      alpha = 5
    ),
    "alpha"
  )
})

test_that("a week set aside by the low-count rule says so", {
  skip_if_not_installed("surveillance")
  north <- islh_seasons[islh_seasons$hsda == "North Vancouver Island", ]
  quiet <- north
  recent <- quiet$period_start >= as.Date("2025-12-29") &
    quiet$period_start <= as.Date("2026-01-19")
  quiet$count[recent] <- 0

  out <- islh_farrington(quiet, period_start, count, from = "2026-01-19")
  expect_equal(out$status, "low_count_rule")
  expect_true(is.na(out$alarm))
  expect_true(is.na(out$upper_limit))

  # With the rule switched off, the same week is assessed.
  assessed <- islh_farrington(
    quiet,
    period_start,
    count,
    from = "2026-01-19",
    low_count = 0
  )
  expect_equal(assessed$status, "assessed")
  expect_false(is.na(assessed$alarm))
})

test_that("assessed weeks have a status and the settings are recorded", {
  skip_if_not_installed("surveillance")
  out <- islh_farrington(
    islh_seasons,
    period_start,
    count,
    by = hsda,
    from = "2026-01-05",
    to = "2026-01-19",
    alpha = 0.01
  )
  expect_true(all(
    out$status %in% c("assessed", "low_count_rule", "fit_failed")
  ))
  expect_equal(is.na(out$alarm), out$status != "assessed")
  settings <- attr(out, "islh_farrington")
  expect_equal(settings$alpha, 0.01)
  expect_equal(settings$low_count, 5L)
  expect_equal(settings$low_count_weeks, 4L)
})

test_that("groups are modelled separately even when their labels collide", {
  skip_if_not_installed("surveillance")
  north <- islh_seasons[islh_seasons$hsda == "North Vancouver Island", ]
  first <- north
  first$site <- NA_character_
  second <- north
  second$site <- "NA"
  second$count <- second$count * 10

  out <- islh_farrington(
    rbind(first, second),
    period_start,
    count,
    by = site,
    from = "2026-01-19"
  )
  week <- north$count[north$period_start == as.Date("2026-01-19")]
  expect_equal(out$observed[is.na(out$site)], week)
  expect_equal(out$observed[out$site %in% "NA"], week * 10)
})

test_that("low-count settings are validated", {
  skip_if_not_installed("surveillance")
  expect_error(
    islh_farrington(
      islh_seasons,
      period_start,
      count,
      by = hsda,
      from = "2026-01-19",
      low_count_weeks = 0
    ),
    "low_count_weeks",
    class = "islh_error"
  )
})
