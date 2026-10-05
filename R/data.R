#' Simulated respiratory outbreak on Vancouver Island
#'
#' A line list of 790 simulated cases from a winter respiratory virus wave,
#' November 2025 to March 2026, spread across Island Health's local health
#' areas in proportion to their 2025 population. The package examples and
#' articles use it, so they show what real surveillance data look like:
#' uneven weekly counts, reporting delays, a few admissions, and far more
#' cases in Greater Victoria than in Vancouver Island West.
#'
#' **Every record is simulated.** None describes a real person or a real
#' outbreak, and the counts are not Island Health surveillance data.
#'
#' @format A data frame with 790 rows, one per case, and 12 columns:
#' \describe{
#'   \item{case_id}{Case identifier, `"C0001"` to `"C0790"` in onset order.}
#'   \item{case_type}{`"Confirmed"`, `"Probable"` or `"Suspected"`.}
#'   \item{sex}{`"Female"` or `"Male"`.}
#'   \item{age}{Age in whole years, 1 to 94.}
#'   \item{date_onset}{Date of symptom onset.}
#'   \item{date_reported}{Date the case was reported, 0 to 17 days after
#'     onset.}
#'   \item{date_admission}{Date of hospital admission, or `NA` for cases not
#'     admitted.}
#'   \item{outcome}{`"Recovered"` or `"Died"`.}
#'   \item{date_outcome}{Date of recovery or death.}
#'   \item{hsda}{Health service delivery area, such as
#'     `"South Vancouver Island"`.}
#'   \item{lha_code}{Three-digit local health area code, such as `"411"`.
#'     It matches `geography_code` from [islh_bc_population()] and
#'     [islh_bc_geography()].}
#'   \item{lha_name}{Local health area name, such as `"Greater Victoria"`.}
#' }
#'
#' @source Simulated with the
#'   [simulist](https://epiverse-trace.github.io/simulist/) package, which
#'   draws an outbreak as a branching process, so onsets, delays, ages and
#'   outcomes hang together. Cases are assigned to local health areas using BC
#'   Stats' 2025 population estimates. The script is `data-raw/islh_outbreak.R`
#'   in the package source.
#'
#' The final counts by week of onset are the 2025-26 season of
#' [islh_seasons], so the two datasets describe the same season. A report
#' written during the season sees only the cases reported by then: filter on
#' `date_reported` first, as the surveillance guide shows.
#'
#' @examples
#' library(dplyr)
#'
#' islh_outbreak |>
#'   select(case_id, age, sex, date_onset, date_reported, hsda)
#'
#' # The line list as it stood on 1 February 2026.
#' known <- islh_outbreak |>
#'   filter(date_reported <= as.Date("2026-02-01"))
#'
#' known |>
#'   islh_count_events(
#'     date = date_onset,
#'     id = case_id,
#'     by = hsda,
#'     interval = "week",
#'     from = "2025-11-03",
#'     to = "2026-02-01"
#'   ) |>
#'   tail()
"islh_outbreak"

#' Simulated weekly counts over seven seasons
#'
#' Weekly counts of a winter respiratory illness for each health service
#' delivery area, from September 2019 to August 2026. Each season peaks
#' between early December and early February at its own height. The 2020-21
#' season barely rises, like a season disrupted by public health measures, so
#' the examples can show why an unusual season is left out of a baseline.
#'
#' The 2025-26 season is [islh_outbreak]: its final line list counted by week
#' of onset. The earlier seasons are simulated on the same scale, with
#' extra-Poisson variation between weeks, as real counts have.
#'
#' Use it to build matched seasonal baselines with
#' [islh_reference_periods()] and [islh_surveillance_baseline()], and to try
#' [islh_compare_periods()] and [islh_farrington()].
#'
#' **Every count is simulated.** They are not Island Health surveillance data.
#'
#' @format A data frame with 1,095 rows, one per HSDA and week, as returned by
#'   [islh_count_events()], with its reporting-period metadata:
#' \describe{
#'   \item{hsda}{Health service delivery area.}
#'   \item{period_start}{Monday the week starts.}
#'   \item{period_end}{Sunday the week ends.}
#'   \item{count}{Number of cases.}
#'   \item{partial_period}{Always `FALSE`.}
#' }
#'
#' @source Seasons before 2025-26 are simulated from a seasonal curve with
#'   gamma-Poisson noise, scaled by BC Stats' 2025 HSDA populations; 2025-26
#'   is counted from [islh_outbreak]. The script is
#'   `data-raw/islh_scenarios.R` in the package source.
#'
#' @examples
#' library(dplyr)
#'
#' # Season totals: 2020-21 is the quiet one.
#' islh_seasons |>
#'   mutate(
#'     season = islh_reporting_calendar(period_start, season_start_month = 9)$season
#'   ) |>
#'   summarise(cases = sum(count), .by = season)
#'
#' reference <- islh_reference_periods(
#'   "2026-01-19",
#'   years_back = 5,
#'   window = 2,
#'   exclude = data.frame(from = "2020-09-01", to = "2021-08-31")
#' )
#'
#' islh_surveillance_baseline(
#'   islh_seasons,
#'   date = period_start,
#'   value = count,
#'   by = hsda,
#'   reference_periods = reference
#' )
"islh_seasons"

#' Simulated encounters with repeat visits
#'
#' Four weeks of encounter records from three sites, August 2026. Some people
#' come back on another day or go to another site, and three records were
#' resent in a later file, so the same encounter appears twice. Use it to see
#' the difference between counting encounters and counting people, and to
#' find duplicate records.
#'
#' Site C's files for 12 and 13 August never arrived, so it has no records on
#' those days. [islh_feed_log] records which files arrived.
#'
#' **Every record is simulated.** The sites are not real facilities and no
#' record describes a real person.
#'
#' @format A data frame with 332 rows and 4 columns:
#' \describe{
#'   \item{encounter_id}{Encounter identifier. Three appear twice.}
#'   \item{person_id}{Person identifier. Many people have more than one
#'     encounter.}
#'   \item{site}{`"Site A"`, `"Site B"` or `"Site C"`.}
#'   \item{encounter_date}{Date of the encounter.}
#' }
#'
#' @source Simulated. The script is `data-raw/islh_scenarios.R` in the
#'   package source.
#'
#' @examples
#' # Resent records show as duplicates.
#' issues <- islh_check_events(
#'   islh_encounters,
#'   id = encounter_id,
#'   date = encounter_date,
#'   max_date = "2026-08-30"
#' )
#' dplyr::count(issues, .issue)
#'
#' # People, not encounters, per week.
#' islh_encounters |>
#'   dplyr::distinct() |>
#'   islh_count_events(
#'     date = encounter_date,
#'     id = person_id,
#'     interval = "week",
#'     from = "2026-08-03",
#'     to = "2026-08-30"
#'   )
"islh_encounters"

#' Simulated daily feed log
#'
#' The files received from each site for [islh_encounters], one row per file.
#' Site C's files for 12 and 13 August are missing. Site B sent nil reports,
#' files with no records, on days it had no encounters. A count table cannot
#' tell those two situations apart; this log can.
#'
#' **Every record is simulated.**
#'
#' @format A data frame with 82 rows and 3 columns:
#' \describe{
#'   \item{site}{`"Site A"`, `"Site B"` or `"Site C"`.}
#'   \item{date}{Date the file covers.}
#'   \item{records}{Number of encounter records dated that day. Zero for a
#'     nil report.}
#' }
#'
#' @source Simulated. The script is `data-raw/islh_scenarios.R` in the
#'   package source.
#'
#' @examples
#' coverage <- islh_check_coverage(
#'   islh_feed_log,
#'   date = date,
#'   by = site,
#'   expected = c("Site A", "Site B", "Site C"),
#'   from = "2026-08-03",
#'   to = "2026-08-30"
#' )
#' dplyr::filter(coverage, !received)
"islh_feed_log"

#' Population of Island Health's local health areas by age and sex, 2025
#'
#' BC Stats' population estimates for 2025 for Island Health's 14 local
#' health areas, by sex and five-year age group. It is exactly what
#' [islh_bc_population()] returns for these areas, saved so the examples and
#' articles can use real denominators without a network request.
#'
#' The estimates refer to 1 July 2025. BC Stats labels the sex column
#' `Gender`; its categories are `F` and `M`, and for each area they sum to the
#' published total. Check that definition against the case data's before
#' calculating rates by sex.
#'
#' For a report, retrieve current estimates with [islh_bc_population()] and
#' record the version used: BC Stats revises estimates, and later releases
#' replace these figures.
#'
#' @format A data frame with 504 rows, one per area, sex and age group:
#' \describe{
#'   \item{geography_code}{Three-digit local health area code, such as
#'     `"411"`.}
#'   \item{geography_name}{Local health area name.}
#'   \item{hsda}{Health service delivery area.}
#'   \item{year}{2025.}
#'   \item{estimate_type}{`"Estimate"`.}
#'   \item{sex}{`"F"` or `"M"`.}
#'   \item{age_group}{Ordered factor of five-year groups, `"0-4"` to
#'     `"85+"`, as [islh_age_group()] labels them.}
#'   \item{population}{Population on 1 July 2025.}
#' }
#'
#' @source BC Stats, BC Sub-Provincial Population Estimates and Projections,
#'   BC Data Catalogue record 86839277-986a-4a29-9f70-fa9b1166f6cb, local
#'   health area resource last modified 20 May 2026, retrieved 4 October 2026.
#'   Contains information licensed under the Open Government Licence - British
#'   Columbia. The script is `data-raw/islh_lha_population.R` in the package
#'   source.
#'
#' @examples
#' library(dplyr)
#'
#' # Population by HSDA and broad age band.
#' islh_lha_population |>
#'   mutate(
#'     band = case_when(
#'       age_group %in% c("0-4", "5-9", "10-14", "15-19") ~ "0-19",
#'       age_group >= "65-69" ~ "65+",
#'       .default = "20-64"
#'     )
#'   ) |>
#'   summarise(population = sum(population), .by = c(hsda, band)) |>
#'   tidyr::pivot_wider(names_from = band, values_from = population)
"islh_lha_population"
