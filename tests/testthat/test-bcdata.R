test_that("catalogue identifiers are pinned and inspectable", {
  sources <- islh_bc_sources()

  expect_equal(nrow(sources), 6L)
  expect_setequal(sources$source_type, c("population", "boundary"))
  expect_setequal(sources$geography, c("ha", "hsda", "lha", "chsa"))
  expect_true(all(nzchar(sources$record_id)))
  expect_true(all(grepl(
    "^https://catalogue.data.gov.bc.ca",
    sources$catalogue_url
  )))

  population <- sources[sources$source_type == "population", ]
  expect_true(all(nzchar(population$resource_id)))
})

test_that("population downloads become standardized age denominators", {
  skip_if_not_installed("bcdata")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  raw <- data.frame(
    Region = c(71, 71, 0, 11),
    Region.Name = c("Courtenay", "Courtenay", "BC", "East Kootenay"),
    Region.Type = c(
      "Local Health Area",
      "Local Health Area",
      "Local Health Area",
      "Health Service Delivery Area"
    ),
    Year = c(2025, 2025, 2025, 2025),
    Type = c("Estimate", "Estimate", "Estimate", "Estimate"),
    Gender = c("F", "M", "F", "F"),
    check.names = FALSE
  )
  for (age in 0:89) {
    raw[[as.character(age)]] <- rep(0, 4)
  }
  raw[["0"]] <- c(10, 11, 100, 20)
  raw[["1"]] <- c(12, 13, 100, 20)
  raw[["5"]] <- c(30, 31, 100, 20)
  raw[["90+"]] <- c(4, 5, 100, 20)

  local_mocked_bindings(
    .islh_fetch_bc_population = function(record_id, resource_id) raw,
    .package = "islandepi"
  )

  out <- islh_bc_population(
    "lha",
    years = 2025,
    age_breaks = c(0, 5, 85)
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(unique(out$geography), "lha")
  expect_equal(unique(out$geography_code), "071")
  expect_setequal(as.character(out$age_group), c("0-4", "5-84", "85+"))
  expect_equal(sum(out$population[out$sex == "F"]), 56)
  expect_equal(sum(out$population[out$sex == "M"]), 60)

  metadata <- attr(out, "bcdata_source")
  expect_identical(
    metadata$record_id,
    "86839277-986a-4a29-9f70-fa9b1166f6cb"
  )
  expect_identical(
    metadata$resource_id,
    "d4bbb2a0-aff7-403f-b52a-a634d05ee70f"
  )
})

test_that("population requests prevent double counting and schema drift", {
  expect_error(islandepi:::.islh_check_sex(c("T", "F")), "double counts")
  expect_error(islandepi:::.islh_check_sex("X"), "Unknown")
  expect_error(islandepi:::.islh_check_years(2025.5), "whole-number")

  raw <- data.frame(
    Region = 71,
    Region.Name = "Courtenay",
    Region.Type = "Local Health Area",
    Year = 2025,
    Type = "Estimate",
    Gender = "F"
  )
  expect_error(
    islandepi:::.islh_tidy_bc_population(
      raw,
      "lha",
      NULL,
      "F",
      "five_year",
      NULL
    ),
    "no one-year age columns"
  )

  raw[["0"]] <- "ten"
  expect_error(
    islandepi:::.islh_tidy_bc_population(
      raw,
      "lha",
      NULL,
      "F",
      "five_year",
      NULL
    ),
    "non-numeric age columns"
  )
})

test_that("boundary downloads use stable join keys and Island Health alias", {
  skip_if_not_installed("bcdata")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("sf")

  raw <- sf::st_as_sf(
    data.frame(
      LOCAL_HLTH_AREA_CODE = c(71, 11),
      LOCAL_HLTH_AREA_NAME = c("Courtenay", "East Kootenay"),
      HLTH_AUTHORITY_NAME = c("Vancouver Island", "Interior"),
      x = c(1000000, 1100000),
      y = c(500000, 600000)
    ),
    coords = c("x", "y"),
    crs = 3005
  )

  local_mocked_bindings(
    .islh_fetch_bc_geography = function(record_id, crs) raw,
    .package = "islandepi"
  )

  out <- islh_bc_geography("lha", health_authority = "Island Health")

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1L)
  expect_equal(out$geography, "lha")
  expect_equal(out$geography_code, "071")
  expect_equal(out$geography_name, "Courtenay")
  expect_equal(out$health_authority_name, "Vancouver Island")
  expect_equal(sf::st_crs(out)$epsg, 3005)

  all_bc <- islh_bc_geography("lha", health_authority = NULL)
  expect_equal(nrow(all_bc), 2L)
  expect_setequal(all_bc$geography_code, c("071", "011"))

  source <- attr(out, "bcdata_source")
  expect_equal(source$arguments$health_authority, "Island Health")
  expect_type(source$content_md5, "character")
  expect_error(
    islh_bc_geography("lha", expected_codes = c("071", "072")),
    "072"
  )
  expect_s3_class(islh_bc_geography("lha", expected_codes = "071"), "sf")
})

test_that("boundary arguments and catalogue changes fail clearly", {
  skip_if_not_installed("sf")

  expect_error(islandepi:::.islh_check_crs(4326.5), "whole-number")
  expect_error(islandepi:::.islh_check_tolerance(-1), "non-negative")
  expect_error(islandepi:::.islh_check_authority(""), "non-empty")

  raw <- sf::st_as_sf(
    data.frame(
      LOCAL_HLTH_AREA_CODE = 71,
      LOCAL_HLTH_AREA_NAME = "Courtenay",
      HLTH_AUTHORITY_NAME = "Vancouver Island",
      x = 1000000,
      y = 500000
    ),
    coords = c("x", "y"),
    crs = 3005
  )
  source <- islandepi:::.islh_bc_sources$geography$lha

  expect_error(
    islandepi:::.islh_standardize_bc_geography(
      raw,
      "lha",
      source,
      "Northern"
    ),
    "No boundary matched"
  )

  raw$LOCAL_HLTH_AREA_CODE <- NULL
  expect_error(
    islandepi:::.islh_standardize_bc_geography(
      raw,
      "lha",
      source,
      "Vancouver Island"
    ),
    "schema has changed"
  )
})

test_that("bcdata network failures preserve the underlying reason", {
  expect_error(
    islandepi:::.islh_with_bcdata_errors(stop("proxy unavailable")),
    "proxy unavailable"
  )
})

test_that("missing data packages produce an actionable error", {
  packages <- c("islandepi_missing_one", "islandepi_missing_two")

  expect_error(
    islandepi:::.islh_require_packages(packages, "test data"),
    paste0(
      "Missing packages required for test data: ",
      "islandepi_missing_one, islandepi_missing_two"
    ),
    fixed = TRUE
  )
})

test_that("a genuine zero population is kept rather than dropped", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  # A small local health area with nobody in an age band. That is a real
  # measurement, not a gap: dropping the row would turn a complete denominator
  # table into an incomplete one, and a later join would report a missing
  # population that looks like a failed download.
  raw <- data.frame(
    Region = c(71, 71),
    Region.Name = c("Courtenay", "Courtenay"),
    Region.Type = c("Local Health Area", "Local Health Area"),
    Year = c(2025, 2025),
    Type = c("Estimate", "Estimate"),
    Gender = c("F", "M"),
    check.names = FALSE
  )
  raw[["0"]] <- c(0, 11)
  raw[["5"]] <- c(30, 0)
  raw[["90+"]] <- c(0, 0)

  out <- islandepi:::.islh_tidy_bc_population(
    raw,
    "lha",
    2025,
    c("F", "M"),
    c(0, 5, 85),
    NULL
  )

  expect_equal(nrow(out), 6L)
  expect_setequal(as.character(out$age_group), c("0-4", "5-84", "85+"))

  female <- out[out$sex == "F", ]
  expect_equal(female$population[female$age_group == "0-4"], 0)
  expect_equal(female$population[female$age_group == "85+"], 0)
  expect_equal(female$population[female$age_group == "5-84"], 30)

  # Every age band is present for both sexes, zero or not.
  expect_equal(sum(out$population == 0), 4L)
})

test_that("a roster check stops when an expected code is missing", {
  expect_silent(.islh_check_roster(c("411", "412", "421"), c("411", "412")))
  expect_silent(.islh_check_roster(c("411"), NULL))
  expect_error(
    .islh_check_roster(c("411", "412"), c("411", "413")),
    "413",
    class = "islh_error"
  )
  expect_error(.islh_check_expected_codes(411), "text codes")
  expect_error(.islh_check_expected_codes(c("411", NA)), "text codes")
  expect_equal(.islh_check_expected_codes(c("411", "411")), "411")
})

# A slice of the real pinned LHA population resource, with every column:
# Island Health's 14 LHAs, one LHA elsewhere and the BC total, for 2025
# (estimates) and 2026 (projections). See fixtures/README.md.
real_lha_population <- function() {
  utils::read.csv(
    test_path("fixtures", "bc-lha-population.csv"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

island_lha_codes <- c(
  "411",
  "412",
  "413",
  "414",
  "421",
  "422",
  "423",
  "424",
  "425",
  "426",
  "431",
  "432",
  "433",
  "434"
)

test_that("the adapter reads the current catalogue schema", {
  raw <- real_lha_population()
  local_mocked_bindings(
    .islh_require_packages = function(...) invisible(TRUE),
    .islh_fetch_bc_population = function(record_id, resource_id) raw,
    .package = "islandepi"
  )

  out <- islh_bc_population(
    "lha",
    years = 2025,
    age_breaks = "five_year",
    expected_codes = island_lha_codes
  )
  island <- out[out$geography_code %in% island_lha_codes, ]

  expect_setequal(unique(island$geography_code), island_lha_codes)
  expect_equal(unique(out$estimate_type), "Estimate")
  expect_false(any(c("000", "0") %in% out$geography_code))

  # Female plus male age bands add up to the source's own total-sex row.
  totals <- raw[raw$Year == 2025 & raw$Gender == "T", c("Region", "Total")]
  totals$code <- sprintf("%03d", totals$Region)
  summed <- tapply(island$population, island$geography_code, sum)
  expect_equal(
    as.vector(summed[island_lha_codes]),
    totals$Total[match(island_lha_codes, totals$code)]
  )
  expect_equal(
    island$geography_name[island$geography_code == "433"][1],
    "Vancouver Island West"
  )
})

test_that("projections are requested explicitly and missing codes stop", {
  raw <- real_lha_population()
  local_mocked_bindings(
    .islh_require_packages = function(...) invisible(TRUE),
    .islh_fetch_bc_population = function(record_id, resource_id) raw,
    .package = "islandepi"
  )

  projected <- islh_bc_population(
    "lha",
    years = 2026,
    sex = "T",
    estimate_type = "Projection"
  )
  expect_equal(unique(projected$estimate_type), "Projection")
  expect_equal(unique(projected$sex), "T")

  expect_error(
    islh_bc_population(
      "lha",
      years = 2025,
      expected_codes = c(island_lha_codes, "499")
    ),
    "499",
    class = "islh_error"
  )
})
