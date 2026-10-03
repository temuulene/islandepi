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
    c("period_start", "observed", "expected", "upper_limit", "alarm", "pvalue")
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
