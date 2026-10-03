#' Reporting-calendar labels for dates
#'
#' `islh_reporting_calendar()` gives each date the labels routine reports use:
#' the start of its week, ISO and CDC epidemiological week numbers with the
#' year they belong to, the fiscal year and quarter and, optionally, the
#' season. It uses the same calendar rules as [islh_count_events()], so its
#' week starts join directly to `period_start`.
#'
#' Week years are the trap this avoids. 29 December 2025 falls in ISO week 1
#' of 2026, and 3 January 2026 falls in epidemiological week 53 of 2025.
#' Labelling either by its calendar year puts it in the wrong week.
#'
#' @param dates Dates, POSIXt timestamps or ISO `YYYY-MM-DD` text.
#' @param week_start Start of an ordinary week, from 1 (Monday) to 7 (Sunday).
#' @param fiscal_start_month Month the fiscal year starts. The default, 4, is
#'   the April-to-March year used by the BC government and health
#'   authorities.
#' @param season_start_month Optional month a surveillance season starts, such
#'   as 9 for September. `NULL` (the default) leaves out the season columns,
#'   because season definitions differ by programme.
#' @param timezone Reporting timezone for timestamps. Date values are
#'   unchanged.
#'
#' @return A data frame with one row per input date, in input order: `date`,
#'   `week_start_date`, `isoweek_start`, `iso_year`, `iso_week`,
#'   `epiweek_start`, `epi_year`, `epi_week`, `month_start`, `fiscal_year`
#'   (a label such as `"2025/26"`), `fiscal_quarter` (1 to 4, counted from
#'   `fiscal_start_month`) and, with `season_start_month`, `season` (a label
#'   such as `"2025-26"`) and `season_week` (weeks since the season started,
#'   counting the week holding the season's first day as 1).
#' @export
#'
#' @examples
#' islh_reporting_calendar(
#'   c("2025-12-28", "2025-12-29", "2026-01-03", "2026-04-01")
#' )
#'
#' # Add a September-to-August respiratory season.
#' islh_reporting_calendar(
#'   c("2025-08-31", "2025-09-01", "2026-01-15"),
#'   season_start_month = 9
#' )
islh_reporting_calendar <- function(
  dates,
  week_start = 1,
  fiscal_start_month = 4,
  season_start_month = NULL,
  timezone = "America/Vancouver"
) {
  week_start <- .islh_surv_week_start("week", week_start)
  fiscal_start_month <- .islh_check_month(
    fiscal_start_month,
    "fiscal_start_month"
  )
  if (!is.null(season_start_month)) {
    season_start_month <- .islh_check_month(
      season_start_month,
      "season_start_month"
    )
  }
  dates <- .islh_surv_as_date(dates, "dates", timezone = timezone)

  month <- as.integer(format(dates, "%m"))
  year <- as.integer(format(dates, "%Y"))

  out <- data.frame(
    date = dates,
    week_start_date = .islh_surv_period_start(dates, "week", week_start),
    isoweek_start = .islh_surv_period_start(dates, "isoweek", 1L),
    iso_year = as.integer(lubridate::isoyear(dates)),
    iso_week = as.integer(lubridate::isoweek(dates)),
    epiweek_start = .islh_surv_period_start(dates, "epiweek", 7L),
    epi_year = as.integer(lubridate::epiyear(dates)),
    epi_week = as.integer(lubridate::epiweek(dates)),
    month_start = .islh_surv_period_start(dates, "month", week_start),
    fiscal_year = .islh_year_label(year, month, fiscal_start_month, "/"),
    fiscal_quarter = as.integer(
      ((month - fiscal_start_month) %% 12L) %/% 3L + 1L
    )
  )

  if (!is.null(season_start_month)) {
    out$season <- .islh_year_label(year, month, season_start_month, "-")
    first_year <- ifelse(month >= season_start_month, year, year - 1L)
    season_first <- as.Date(sprintf(
      "%04d-%02d-01",
      first_year,
      season_start_month
    ))
    first_week <- .islh_surv_period_start(season_first, "week", week_start)
    out$season_week <- as.integer(
      as.numeric(out$week_start_date - first_week) %/% 7 + 1
    )
  }
  out
}

# "2025/26" for a year starting in `start_month`. A year starting in January
# is just the calendar year.
.islh_year_label <- function(year, month, start_month, sep) {
  if (start_month == 1L) {
    return(as.character(year))
  }
  first <- ifelse(month >= start_month, year, year - 1L)
  paste0(first, sep, sprintf("%02d", (first + 1L) %% 100L))
}

.islh_check_month <- function(x, arg, call = rlang::caller_env()) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      x != round(x) ||
      x < 1 ||
      x > 12
  ) {
    .islh_abort(
      "{.arg {arg}} must be one whole number from 1 to 12.",
      call = call
    )
  }
  as.integer(x)
}

#' Matched reference periods from earlier years
#'
#' `islh_reference_periods()` lists the periods a seasonal baseline should use:
#' the same period in each of several earlier years, plus a few periods either
#' side. Pass the result to `reference_periods` in
#' [islh_surveillance_baseline()].
#'
#' Each earlier year's centre is the same period of that year. ISO and
#' epidemiological weeks keep their week number, with week 53 mapped to week
#' 52 in a year that has no week 53. Ordinary weeks take the week starting
#' nearest to the same date a year earlier. Days and months move back whole
#' calendar years. None of these drift a day each year the way stepping back
#' 52 weeks does.
#'
#' @section Excluding unusual periods:
#'
#' A baseline should describe usual activity. Leave out periods that were not
#' usual, such as a pandemic season or a known outbreak, with `exclude`. Say
#' in the report which periods were left out and why. Leaving out periods
#' makes the baseline shorter, so check `reference_n` in the result.
#'
#' @param current Start date of the period being assessed.
#' @param years_back Number of earlier years to use.
#' @param window Number of periods either side of each year's centre period.
#'   0 uses only the matching period.
#' @param interval `"week"`, `"isoweek"`, `"epiweek"`, `"day"` or `"month"`.
#' @param week_start Start of an ordinary week, from 1 (Monday) to 7 (Sunday).
#' @param exclude Optional periods to leave out: a vector of dates, which drops
#'   any reference period containing one of them, or a data frame with `from`
#'   and `to` columns, which drops any reference period overlapping one of the
#'   ranges.
#'
#' @return A sorted vector of period start dates. It never includes `current`
#'   or any later period.
#' @export
#'
#' @examples
#' # The week of 26 January 2026, with two weeks either side, in each of the
#' # five previous years.
#' islh_reference_periods("2026-01-26", years_back = 5, window = 2)
#'
#' # Leave out the 2020-21 season.
#' islh_reference_periods(
#'   "2026-01-26",
#'   years_back = 5,
#'   window = 2,
#'   exclude = data.frame(from = "2020-09-01", to = "2021-08-31")
#' )
islh_reference_periods <- function(
  current,
  years_back = 5,
  window = 2,
  interval = c("week", "isoweek", "epiweek", "day", "month"),
  week_start = 1,
  exclude = NULL
) {
  interval <- match.arg(interval)
  week_start <- .islh_surv_week_start(interval, week_start)
  current <- .islh_surv_as_date(current, "current", scalar = TRUE)
  if (.islh_surv_period_start(current, interval, week_start) != current) {
    .islh_abort(c(
      "{.arg current} must be the start of a {interval}.",
      i = "The {interval} holding {format(current)} starts on
           {format(.islh_surv_period_start(current, interval, week_start))}."
    ))
  }
  for (arg in c("years_back", "window")) {
    value <- get(arg)
    minimum <- if (arg == "years_back") 1 else 0
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        value != round(value) ||
        value < minimum
    ) {
      .islh_abort(
        "{.arg {arg}} must be one whole number of at least {minimum}."
      )
    }
  }

  offsets <- seq(-window, window)
  starts <- do.call(
    c,
    lapply(seq_len(years_back), function(k) {
      centre <- .islh_year_back(current, k, interval, week_start)
      if (interval == "month") {
        # `centre` is the first of a month, so no day ever rolls over.
        lubridate::add_with_rollback(centre, months(offsets))
      } else if (interval == "day") {
        centre + offsets
      } else {
        centre + 7L * offsets
      }
    })
  )
  starts <- sort(unique(starts[starts < current]))

  if (!is.null(exclude)) {
    ends <- .islh_surv_period_end(starts, interval, week_start)
    drop <- .islh_excluded(starts, ends, exclude)
    starts <- starts[!drop]
  }
  if (length(starts) == 0L) {
    .islh_abort("No reference periods remain after the exclusions.")
  }
  starts
}

# The same period `k` years before the period starting on `date`.
#
# * ISO and epidemiological weeks keep their week number, which is how
#   surveillance compares them. Week 53 maps to week 52 in a 52-week year.
# * Ordinary weeks take the week starting nearest to the date a calendar year
#   earlier. Taking the week holding that date can land a week early, since a
#   year is 52 weeks and one or two days.
# * Days, months, quarters and years move back whole calendar years, with
#   February 29 rolling back to February 28.
.islh_year_back <- function(date, k, interval, week_start) {
  if (interval %in% c("isoweek", "epiweek")) {
    number <- if (interval == "isoweek") {
      lubridate::isoweek(date)
    } else {
      lubridate::epiweek(date)
    }
    year <- if (interval == "isoweek") {
      lubridate::isoyear(date)
    } else {
      lubridate::epiyear(date)
    }
    # Week 1 is the week holding 4 January, in both systems.
    week_one <- .islh_surv_period_start(
      as.Date(sprintf("%04d-01-04", year - k)),
      interval,
      week_start
    )
    target <- week_one + 7L * (number - 1L)
    next_week_one <- .islh_surv_period_start(
      as.Date(sprintf("%04d-01-04", year - k + 1L)),
      interval,
      week_start
    )
    if (target >= next_week_one) {
      target <- target - 7L
    }
    return(target)
  }
  shifted <- lubridate::add_with_rollback(date, lubridate::years(-k))
  if (interval == "week") {
    return(.islh_surv_period_start(shifted + 3L, interval, week_start))
  }
  .islh_surv_period_start(shifted, interval, week_start)
}

.islh_excluded <- function(starts, ends, exclude, call = rlang::caller_env()) {
  if (is.data.frame(exclude)) {
    if (!all(c("from", "to") %in% names(exclude))) {
      .islh_abort(
        "A data frame {.arg exclude} needs {.field from} and {.field to}
         columns.",
        call = call
      )
    }
    from <- .islh_surv_as_date(exclude$from, "exclude$from", call = call)
    to <- .islh_surv_as_date(exclude$to, "exclude$to", call = call)
    if (any(from > to)) {
      .islh_abort(
        "Each {.arg exclude} range must have {.field from} on or before
         {.field to}.",
        call = call
      )
    }
  } else {
    from <- to <- .islh_surv_as_date(exclude, "exclude", call = call)
  }
  vapply(
    seq_along(starts),
    function(i) any(starts[i] <= to & ends[i] >= from),
    logical(1)
  )
}
