# Builds data/islh_lha_population.rda: BC Stats' 2025 population estimates for
# Island Health's 14 local health areas, by sex and five-year age group.
#
# The rows are read from tests/testthat/fixtures/bc-lha-population.csv, which
# holds the catalogue rows unchanged (see the README beside it), and tidied by
# the same internal function islh_bc_population() uses. So the dataset is
# exactly what islh_bc_population("lha", years = 2025) returns for these areas,
# without a network request.
#
# The data are published by BC Stats under the Open Government Licence -
# British Columbia.
#
# Run with: Rscript data-raw/islh_lha_population.R (from the package root).

devtools::load_all(quiet = TRUE)
library(dplyr)

raw <- utils::read.csv(
  "tests/testthat/fixtures/bc-lha-population.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

hsda_names <- c(
  "41" = "South Vancouver Island",
  "42" = "Central Vancouver Island",
  "43" = "North Vancouver Island"
)

islh_lha_population <- .islh_tidy_bc_population(
  raw,
  geography = "lha",
  years = 2025,
  sex = c("F", "M"),
  age_breaks = "five_year",
  age_labels = NULL,
  validate_age_coverage = TRUE
) |>
  filter(substr(geography_code, 1, 2) %in% names(hsda_names)) |>
  mutate(hsda = unname(hsda_names[substr(geography_code, 1, 2)])) |>
  select(
    geography_code,
    geography_name,
    hsda,
    year,
    estimate_type,
    sex,
    age_group,
    population
  ) |>
  arrange(geography_code, sex, age_group)

# Every area, both sexes, every age group, and totals that match the
# catalogue's own total column.
totals <- raw |>
  filter(Year == 2025, Gender == "T") |>
  transmute(geography_code = sprintf("%03d", Region), total = Total)
check <- islh_lha_population |>
  summarise(population = sum(population), .by = geography_code) |>
  inner_join(totals, by = join_by(geography_code))
stopifnot(
  n_distinct(islh_lha_population$geography_code) == 14L,
  nrow(islh_lha_population) == 14L * 2L * 18L,
  all(check$population == check$total)
)

save(
  islh_lha_population,
  file = "data/islh_lha_population.rda",
  compress = "bzip2"
)
cat("wrote data/islh_lha_population.rda:", nrow(islh_lha_population), "rows\n")
