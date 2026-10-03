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
#' @examples
#' head(islh_outbreak)
#'
#' # Weekly counts by health service delivery area
#' weekly <- islh_count_events(
#'   islh_outbreak,
#'   date = date_onset,
#'   id = case_id,
#'   by = hsda,
#'   interval = "week",
#'   from = "2025-11-03",
#'   to = "2026-03-08"
#' )
#' head(weekly)
"islh_outbreak"

#' Simulated weekly counts over seven seasons
#'
#' Weekly counts of a seasonal respiratory illness by health service delivery
#' area, from September 2019 to August 2026. Each season peaks between early
#' December and early February at its own height. The 2020-21 season barely
#' rises, like a season disrupted by public health measures, so the examples
#' can show why an unusual season is left out of a baseline.
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
#' @source Simulated from a seasonal curve with gamma-Poisson noise, scaled by
#'   BC Stats' 2025 HSDA populations. The script is
#'   `data-raw/islh_scenarios.R` in the package source.
#'
#' @examples
#' reference <- islh_reference_periods(
#'   "2026-01-26",
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
#' @format A data frame with 293 rows and 4 columns:
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
#' table(issues$.issue)
#'
#' # People, not encounters, per week.
#' unique_encounters <- islh_encounters[!duplicated(islh_encounters), ]
#' islh_count_events(
#'   unique_encounters,
#'   date = encounter_date,
#'   id = person_id,
#'   interval = "week",
#'   from = "2026-08-03",
#'   to = "2026-08-30"
#' )
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
#' coverage[!coverage$received, ]
"islh_feed_log"
