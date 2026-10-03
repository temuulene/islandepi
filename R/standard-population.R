# Standard populations.
#
# Values are the 19-age-group tables published by the SEER Program, retrieved
# 2026-09-30 from https://seer.cancer.gov/stdpopulations/stdpop.19ages.html.
# Every column sums exactly to the total SEER prints, and the Canadian 2011
# total matches Statistics Canada's age-standardization page. See
# data-raw/standard-populations.md before adding a standard.

# Lower bounds of the published bands: under 1, 1-4, five-year bands, 85+.
.islh_standard_bounds <- c(0, 1, seq(5, 85, by = 5))

.islh_standard_sources <- function() {
  data.frame(
    standard = c(
      "canada_2011",
      "canada_2021",
      "who_2000_2025",
      "us_2000",
      "europe_2013"
    ),
    standard_id = c(
      "Canada 2011 standard population (Statistics Canada)",
      "Canada 2021 standard population (Statistics Canada)",
      "WHO world standard population 2000-2025",
      "US 2000 standard million",
      "European standard population 2013 (EU-27 plus EFTA 2011-2030)"
    ),
    source = paste(
      "SEER Program, Standard Populations - 19 Age Groups,",
      "https://seer.cancer.gov/stdpopulations/stdpop.19ages.html,",
      "retrieved 2026-09-30"
    ),
    stringsAsFactors = FALSE
  )
}

.islh_standard_table <- function() {
  list(
    canada_2011 = c(
      376321,
      1522743,
      1810433,
      1918164,
      2238952,
      2354354,
      2369841,
      2327955,
      2273087,
      2385918,
      2719909,
      2691260,
      2353090,
      2050443,
      1532940,
      1153822,
      919338,
      701140,
      643070
    ),
    canada_2021 = c(
      362725,
      1540158,
      2072565,
      2114016,
      2059975,
      2404398,
      2674655,
      2704004,
      2643161,
      2505252,
      2381758,
      2427770,
      2690509,
      2612928,
      2226492,
      1845779,
      1272339,
      841433,
      859947
    ),
    who_2000_2025 = c(
      17917,
      70652,
      86870,
      85970,
      84670,
      82171,
      79272,
      76073,
      71475,
      65877,
      60379,
      53681,
      45484,
      37187,
      29590,
      22092,
      15195,
      9097,
      6348
    ),
    us_2000 = c(
      13818,
      55317,
      72533,
      73032,
      72169,
      66478,
      64529,
      71044,
      80762,
      81851,
      72118,
      62716,
      48454,
      38793,
      34264,
      31773,
      26999,
      17842,
      15508
    ),
    europe_2013 = c(
      10000,
      40000,
      55000,
      55000,
      55000,
      60000,
      60000,
      65000,
      70000,
      70000,
      70000,
      70000,
      65000,
      60000,
      55000,
      50000,
      40000,
      25000,
      25000
    )
  )
}

#' Standard population weights
#'
#' Returns a published standard population in age bands, with weights that sum
#' to 1, ready for [islh_dsr_joined()]. There is deliberately no default
#' standard: a standardized rate can only be compared with rates that used the
#' same standard, so the right choice is whichever one the comparison series
#' uses.
#'
#' @section Choosing a standard:
#'
#' * `"canada_2011"` is the Statistics Canada 2011 standard, used by many
#'   Canadian series.
#' * `"canada_2021"` is the newer Statistics Canada 2021 standard, used by some
#'   recent Statistics Canada series.
#' * `"who_2000_2025"` is the WHO world standard, for international
#'   comparison.
#' * `"us_2000"` and `"europe_2013"` are for comparison with American or
#'   European publications.
#'
#' Matching the comparison series matters more than which standard is newest.
#' Rates standardized to different standards are not comparable, even for the
#' same population.
#'
#' @section Age bands:
#'
#' The published tables have 19 bands: under 1, 1 to 4, five-year bands to 84
#' and 85 and over. `age_breaks` combines them. It takes `"five_year"`, the
#' same bands [islh_age_group()] and [islh_bc_population()] produce, or a
#' numeric vector of lower bounds starting at 0. Every bound must be one of the
#' published ones, because a published band cannot be split: 85 and over
#' cannot become 85 to 89 and 90 and over.
#'
#' @param standard One of `"canada_2011"`, `"canada_2021"`,
#'   `"who_2000_2025"`, `"us_2000"` or `"europe_2013"`.
#' @param age_breaks `"five_year"` or a numeric vector of band lower bounds,
#'   each one of 0, 1, 5, 10, ..., 85.
#'
#' @return A data frame with columns `age_group` (an ordered factor labelled
#'   as [islh_age_group()] labels it), `population`, `weight`, `standard` and
#'   `standard_id`. The source is attached as the `islh_source` attribute.
#' @export
#'
#' @references
#' Statistics Canada. Age-standardized rates.
#' <https://www.statcan.gc.ca/en/dai/btd/asr>
#'
#' Ahmad OB, Boschi-Pinto C, Lopez AD, Murray CJL, Lozano R, Inoue M (2001).
#' *Age standardization of rates: a new WHO standard*. GPE Discussion Paper
#' No. 31. World Health Organization.
#'
#' National Cancer Institute, SEER Program. Standard populations (millions)
#' for age-adjustment. <https://seer.cancer.gov/stdpopulations/>
#'
#' @examples
#' standard <- islh_standard_population("canada_2011")
#' head(standard)
#' sum(standard$weight)
#'
#' # Broad bands for a small area.
#' islh_standard_population("canada_2011", age_breaks = c(0, 20, 65))
#'
#' # Use it directly with keyed standardization.
#' cases <- data.frame(
#'   age_group = c("0-19", "20-64", "65+"),
#'   cases = c(4, 22, 31)
#' )
#' population <- data.frame(
#'   age_group = c("0-19", "20-64", "65+"),
#'   population = c(18000, 51000, 23000)
#' )
#' standard <- islh_standard_population("canada_2011", c(0, 20, 65))
#' standard$age_group <- as.character(standard$age_group)
#'
#' islh_dsr_joined(
#'   cases,
#'   population,
#'   standard,
#'   by = "age_group",
#'   standard_id = standard$standard_id[1]
#' )
islh_standard_population <- function(standard, age_breaks = "five_year") {
  sources <- .islh_standard_sources()
  if (missing(standard)) {
    .islh_abort(c(
      "{.arg standard} must be supplied.",
      i = "Use the standard your comparison series uses. Choices:
           {.val {sources$standard}}."
    ))
  }
  if (
    !is.character(standard) ||
      length(standard) != 1L ||
      is.na(standard) ||
      !standard %in% sources$standard
  ) {
    .islh_abort(
      "{.arg standard} must be one of {.val {sources$standard}}."
    )
  }

  if (identical(age_breaks, "five_year")) {
    bounds <- seq(0, 85, by = 5)
  } else {
    if (
      !is.numeric(age_breaks) ||
        length(age_breaks) < 2L ||
        anyNA(age_breaks) ||
        age_breaks[1] != 0 ||
        any(diff(age_breaks) <= 0)
    ) {
      .islh_abort(
        "{.arg age_breaks} must be {.val five_year} or increasing lower
         bounds starting at 0."
      )
    }
    unknown <- setdiff(age_breaks, .islh_standard_bounds)
    if (length(unknown) > 0L) {
      .islh_abort(c(
        "{.arg age_breaks} splits a published band.",
        x = "Not a published lower bound: {.val {unknown}}.",
        i = "Use bounds from 0, 1, 5, 10, ..., 85."
      ))
    }
    bounds <- age_breaks
  }

  counts <- .islh_standard_table()[[standard]]
  band <- findInterval(.islh_standard_bounds, bounds)
  population <- as.numeric(tapply(counts, band, sum))
  age_group <- islh_age_group(bounds, breaks = bounds)

  info <- sources[sources$standard == standard, ]
  out <- data.frame(
    age_group = age_group,
    population = population,
    weight = population / sum(population),
    standard = standard,
    standard_id = info$standard_id,
    stringsAsFactors = FALSE
  )
  attr(out, "islh_source") <- info$source
  out
}
