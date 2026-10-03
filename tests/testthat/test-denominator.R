counts <- data.frame(
  year = 2025,
  age_group = c("65+", "0-19", "20-64"),
  cases = c(31, 4, 22)
)
denominators <- data.frame(
  year = 2025,
  age_group = c("0-19", "20-64", "65+"),
  population = c(18000, 51000, 23000)
)

test_that("denominators attach by key and keep the count order", {
  out <- islh_join_denominator(counts, denominators, c("year", "age_group"))
  expect_identical(out$age_group, counts$age_group)
  expect_equal(out$population, c(23000, 18000, 51000))
  expect_identical(names(out), c(names(counts), "population"))
})

test_that("a differently named denominator column is carried across", {
  pt <- denominators
  names(pt)[3] <- "person_years"
  out <- islh_join_denominator(
    counts,
    pt,
    c("year", "age_group"),
    denominator = "person_years"
  )
  expect_equal(out$person_years, c(23000, 18000, 51000))
})

test_that("repeated denominator keys are refused rather than duplicating", {
  by_sex <- rbind(denominators, denominators)
  expect_error(
    islh_join_denominator(counts, by_sex, c("year", "age_group")),
    "more than one row",
    class = "islh_error"
  )
})

test_that("a count with no denominator stops and names the key", {
  short <- denominators[1:2, ]
  expect_error(
    islh_join_denominator(
      counts,
      short,
      c("year", "age_group"),
      extra = "ignore"
    ),
    "65\\+",
    class = "islh_error"
  )
})

test_that("text codes never match numbers", {
  x <- data.frame(code = c("011", "012"), cases = 1:2)
  y <- data.frame(code = c(11, 12), population = c(100, 200))
  expect_error(
    islh_join_denominator(x, y, "code"),
    "different types",
    class = "islh_error"
  )
})

test_that("factor and text keys match as labels", {
  y <- denominators
  y$age_group <- factor(y$age_group)
  out <- islh_join_denominator(counts, y, c("year", "age_group"))
  expect_equal(out$population, c(23000, 18000, 51000))
})

test_that("unused denominators follow the extra setting", {
  wider <- rbind(
    denominators,
    data.frame(year = 2024, age_group = "0-19", population = 17500)
  )
  keys <- c("year", "age_group")
  expect_message(
    islh_join_denominator(counts, wider, keys),
    "1 row",
    class = "islh_message"
  )
  expect_no_message(islh_join_denominator(
    counts,
    wider,
    keys,
    extra = "ignore"
  ))
  expect_error(
    islh_join_denominator(counts, wider, keys, extra = "error"),
    class = "islh_error"
  )
})

test_that("unsafe inputs are refused", {
  keys <- c("year", "age_group")
  bad <- denominators
  bad$population[2] <- NA
  expect_error(islh_join_denominator(counts, bad, keys), "missing")
  bad$population[2] <- -1
  expect_error(islh_join_denominator(counts, bad, keys), "negative")
  missing_key <- counts
  missing_key$age_group[1] <- NA
  expect_error(
    islh_join_denominator(missing_key, denominators, keys),
    "missing"
  )
  clash <- counts
  clash$population <- 1
  expect_error(
    islh_join_denominator(clash, denominators, keys),
    "already has"
  )
  expect_error(islh_join_denominator(counts, denominators, "sex"), "sex")
  expect_error(islh_join_denominator(counts, denominators, character()), "key")
  expect_error(
    islh_join_denominator(counts, denominators, keys, denominator = "year"),
    "must not be a key"
  )
})

test_that("zero denominators pass through for the rate function to refuse", {
  zero <- denominators
  zero$population[1] <- 0
  out <- islh_join_denominator(counts, zero, c("year", "age_group"))
  expect_equal(out$population[2], 0)
  expect_error(islh_crude_rate(out$cases, out$population), "positive")
})
