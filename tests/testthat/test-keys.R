# Keys are matched as tuples of values, never as pasted strings.

test_that("a separator inside a composite ID is not a false duplicate", {
  ids <- data.frame(
    site = c("A | B", "A"),
    id = c("C", "B | C"),
    day = as.Date("2026-01-05")
  )
  issues <- islh_check_events(ids, id = c(site, id), date = day)
  expect_false("duplicate_id" %in% issues$.issue)
})

test_that("a missing group and the text \"NA\" are different groups", {
  grouped <- data.frame(
    site = rep(c(NA_character_, "NA"), each = 2),
    day = rep(as.Date("2026-01-05") + 0:1, 2),
    n = c(2, 3, 20, 30)
  )
  out <- islh_compare_periods(
    grouped,
    day,
    n,
    by = site,
    interval = "day",
    current = "2026-01-06",
    comparison = "previous_period"
  )
  expect_equal(nrow(out), 2L)
  expect_equal(out$current[is.na(out$site)], 3)
  expect_equal(out$reference[is.na(out$site)], 2)
  expect_equal(out$current[out$site %in% "NA"], 30)
  expect_equal(out$reference[out$site %in% "NA"], 20)
})

test_that("key ids keep missing values apart from text", {
  x <- data.frame(a = c(NA, "NA", "x"), b = c(1, 1, 1))
  expect_identical(anyDuplicated(.islh_key_ids(list(x), c("a", "b"))[[1]]), 0L)
  expect_equal(.islh_key_match(x[c(2, 1), ], x, c("a", "b")), c(2L, 1L))
})

test_that("denominator joins do not confuse a missing key with \"NA\"", {
  counts <- data.frame(area = c("NA", "B"), cases = c(1, 2))
  population <- data.frame(
    area = c("NA", "B"),
    population = c(100, 200)
  )
  joined <- islh_join_denominator(counts, population, by = "area")
  expect_equal(joined$population, c(100, 200))
})

test_that("output comparisons match missing keys only to missing keys", {
  x <- data.frame(area = c(NA, "NA"), value = c(1, 2))
  y <- data.frame(area = c("NA", NA), value = c(2, 1))
  out <- islh_compare_outputs(x, y, by = "area")
  expect_equal(nrow(out), 0L)
})
