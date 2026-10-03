# Totals printed by SEER (stdpop.19ages.html) and, for Canada 2011, by
# Statistics Canada's age-standardization page.
published_totals <- c(
  canada_2011 = 34342780,
  canada_2021 = 38239864,
  who_2000_2025 = 1e6,
  us_2000 = 1e6,
  europe_2013 = 1e6
)

test_that("every standard sums to its published total", {
  for (standard in names(published_totals)) {
    out <- islh_standard_population(standard)
    expect_equal(sum(out$population), published_totals[[standard]])
    expect_equal(sum(out$weight), 1)
    expect_equal(nrow(out), 18)
  }
})

test_that("spot values match the published table", {
  canada <- islh_standard_population("canada_2011")
  expect_equal(canada$population[canada$age_group == "0-4"], 376321 + 1522743)
  expect_equal(canada$population[canada$age_group == "85+"], 643070)
  who <- islh_standard_population("who_2000_2025", c(0, 1, 5))
  expect_equal(who$population[1:2], c(17917, 70652))
})

test_that("labels match islh_age_group so keyed joins line up", {
  out <- islh_standard_population("canada_2021")
  expect_identical(
    levels(out$age_group),
    levels(islh_age_group(0, breaks = "five_year"))
  )
  expect_true(is.ordered(out$age_group))
  expect_equal(unique(out$standard), "canada_2021")
  expect_match(attr(out, "islh_source"), "seer.cancer.gov")
})

test_that("custom breaks combine bands and never split them", {
  out <- islh_standard_population("us_2000", c(0, 20, 65))
  expect_equal(as.character(out$age_group), c("0-19", "20-64", "65+"))
  expect_equal(sum(out$population), 1e6)
  expect_error(
    islh_standard_population("us_2000", c(0, 18, 65)),
    "splits",
    class = "islh_error"
  )
  expect_error(islh_standard_population("us_2000", c(5, 20)), "starting at 0")
})

test_that("there is no default standard", {
  expect_error(islh_standard_population(), "must be supplied")
  expect_error(islh_standard_population("canada"), "must be one of")
})

test_that("the standard feeds islh_dsr_joined directly", {
  standard <- islh_standard_population("canada_2011", c(0, 20, 65))
  standard$age_group <- as.character(standard$age_group)
  cases <- data.frame(
    age_group = c("0-19", "20-64", "65+"),
    cases = c(4, 22, 31)
  )
  population <- data.frame(
    age_group = c("0-19", "20-64", "65+"),
    population = c(18000, 51000, 23000)
  )
  out <- islh_dsr_joined(
    cases,
    population,
    standard,
    by = "age_group",
    standard_id = standard$standard_id[1]
  )
  expected <- sum(standard$weight * cases$cases / population$population) * 1e5
  expect_equal(out$rate, expected)
})
