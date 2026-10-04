#' Compare the current period with earlier ones
#'
#' `islh_compare_periods()` puts the current period's count beside a named
#' reference: the previous period, the same period a year earlier, or the
#' season so far against the same stretch of the previous season. Each
#' comparison says exactly which dates it covers, so "up 40% on last year" can
#' be checked.
#'
#' @section Zero and missing references:
#'
#' `difference` is always reported. `ratio` and `percent_change` are `NA`
#' when the reference is zero, because a change from zero has no size as a
#' ratio: going from 0 to 3 cases is not an infinite increase. Report the
#' counts themselves in that case.
#'
#' A reference window with a missing period gives a missing reference, not a
#' smaller one, and a message says how many comparisons were affected. Supply
#' complete counts, including zeros, from [islh_count_events()].
#'
#' @section Same period a year earlier:
#'
#' The reference is the same period one year earlier, found as in
#' [islh_reference_periods()]: ISO and epidemiological weeks keep their week
#' number, ordinary weeks take the nearest week a year back, and days and
#' months move back a calendar year. For season-to-date, both ends of the
#' window move back the same way, so the two windows can differ by one week;
#' the `current_periods` and `reference_periods` columns show when they do.
#'
#' @param data A complete count table with one row per group and period,
#'   normally from [islh_count_events()].
#' @param date Period start column.
#' @param value Whole-count column.
#' @param by Optional tidy-select specification of grouping columns.
#' @param current Start of the period to assess. Defaults to the latest date
#'   in `data`.
#' @param comparison One or more of `"previous_period"`, `"previous_year"`
#'   and `"season_to_date"`.
#' @param season_start First day of the current season, on a period boundary.
#'   Required for `"season_to_date"`.
#' @param interval Reporting period of one row. `NULL` takes it from the
#'   metadata [islh_count_events()] leaves. A table without that metadata must
#'   name it, unless its dates are consecutive days.
#' @param week_start Start of an ordinary week, from 1 (Monday) to 7 (Sunday),
#'   when `interval` is `"week"` and there is no metadata.
#' @param timezone Reporting timezone for timestamps. Date values are
#'   unchanged.
#'
#' @return A data frame with one row per group and comparison: the grouping
#'   columns, `comparison`, `current_from`, `current_to`, `reference_from`,
#'   `reference_to`, `current_periods`, `reference_periods`, `current`,
#'   `reference`, `difference`, `ratio` and `percent_change`.
#' @export
#'
#' @examples
#' weekly <- islh_count_events(
#'   islh_outbreak,
#'   date = date_onset,
#'   id = case_id,
#'   by = hsda,
#'   interval = "week",
#'   from = "2025-11-03",
#'   to = "2026-03-08"
#' )
#'
#' islh_compare_periods(
#'   weekly,
#'   date = period_start,
#'   value = count,
#'   by = hsda,
#'   current = "2026-01-26",
#'   comparison = c("previous_period", "season_to_date"),
#'   season_start = "2025-11-03"
#' )
islh_compare_periods <- function(
  data,
  date,
  value,
  by = NULL,
  current = NULL,
  comparison = c("previous_period", "previous_year"),
  season_start = NULL,
  interval = NULL,
  week_start = 1,
  timezone = "America/Vancouver"
) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  choices <- c("previous_period", "previous_year", "season_to_date")
  if (
    !is.character(comparison) ||
      length(comparison) == 0L ||
      anyNA(comparison) ||
      !all(comparison %in% choices)
  ) {
    .islh_abort("{.arg comparison} must be one or more of {.val {choices}}.")
  }
  comparison <- unique(comparison)
  meta <- .islh_surv_meta(data)

  date_name <- .islh_surv_select(data, rlang::enquo(date), "date", 1L, 1L)
  value_name <- .islh_surv_select(data, rlang::enquo(value), "value", 1L, 1L)
  by_names <- .islh_surv_select(data, rlang::enquo(by), "by")
  output_columns <- c(
    "comparison",
    "current_from",
    "current_to",
    "reference_from",
    "reference_to",
    "current_periods",
    "reference_periods",
    "current",
    "reference",
    "difference",
    "ratio",
    "percent_change"
  )
  .islh_surv_check_reserved(by_names, output_columns)

  work <- as.data.frame(data)
  if (nrow(work) == 0L) {
    .islh_abort("{.arg data} must contain at least one row.")
  }
  work$.islh_date <- .islh_surv_as_date(
    work[[date_name]],
    date_name,
    timezone = timezone
  )
  work$.islh_value <- .islh_check_counts(
    work[[value_name]],
    value_name,
    allow_na = FALSE
  )
  if (anyDuplicated(work[c(by_names, ".islh_date")])) {
    .islh_abort(c(
      "{.arg data} has more than one row per group and period.",
      i = "Aggregate it with {.fn islh_count_events} first."
    ))
  }

  interval <- .islh_surv_resolve_interval(interval, meta, work$.islh_date)
  if (is.null(interval)) {
    .islh_abort(c(
      "Supply {.arg interval}.",
      x = "{.arg data} does not record what one row covers, and its dates are
           not consecutive days.",
      i = "Rows seven days apart could be weekly totals or daily counts with
           days missing. Name the period, or build the counts with
           {.fn islh_count_events}, which records it."
    ))
  }
  anchor <- if (!is.null(meta$week_start)) {
    meta$week_start
  } else if (interval == "week") {
    week_start
  } else {
    1L
  }
  anchor <- .islh_surv_week_start(interval, anchor)
  .islh_surv_check_alignment(
    list(interval = interval, periods = 1L, week_start = anchor),
    interval,
    anchor,
    work$.islh_date
  )

  current <- if (is.null(current)) {
    max(work$.islh_date)
  } else {
    .islh_surv_as_date(current, "current", scalar = TRUE, timezone = timezone)
  }
  if (.islh_surv_period_start(current, interval, anchor) != current) {
    .islh_abort("{.arg current} must be the start of a {interval}.")
  }

  step_back <- function(x) {
    .islh_surv_periods_back(x, 2L, interval)[1]
  }
  windows <- list()
  if ("previous_period" %in% comparison) {
    windows$previous_period <- list(
      current = c(current, current),
      reference = rep(step_back(current), 2)
    )
  }
  if ("previous_year" %in% comparison) {
    last_year <- .islh_year_back(current, 1L, interval, anchor)
    windows$previous_year <- list(
      current = c(current, current),
      reference = c(last_year, last_year)
    )
  }
  if ("season_to_date" %in% comparison) {
    if (is.null(season_start)) {
      .islh_abort(
        "Supply {.arg season_start} for a {.val season_to_date} comparison."
      )
    }
    season_start <- .islh_surv_as_date(
      season_start,
      "season_start",
      scalar = TRUE,
      timezone = timezone
    )
    if (
      .islh_surv_period_start(season_start, interval, anchor) != season_start
    ) {
      .islh_abort("{.arg season_start} must be the start of a {interval}.")
    }
    if (season_start > current) {
      .islh_abort("{.arg season_start} must not be after {.arg current}.")
    }
    windows$season_to_date <- list(
      current = c(season_start, current),
      reference = c(
        .islh_year_back(season_start, 1L, interval, anchor),
        .islh_year_back(current, 1L, interval, anchor)
      )
    )
    .islh_surv_check_id_sum(
      meta,
      by_names,
      merges_periods = current > season_start
    )
  }

  # Every row a comparison reads must be a whole period.
  used <- unique(do.call(
    c,
    unname(lapply(windows, function(w) {
      c(
        .islh_surv_period_sequence(w$current[1], w$current[2], interval),
        .islh_surv_period_sequence(w$reference[1], w$reference[2], interval)
      )
    }))
  ))
  .islh_surv_check_partial(work, work$.islh_date %in% used)

  groups <- if (length(by_names) > 0L) {
    unique(work[by_names])
  } else {
    data.frame(.islh_all = 1L)
  }
  ids <- .islh_key_ids(list(work, groups), by_names)
  work$.islh_group <- ids[[1]]
  groups$.islh_group <- ids[[2]]

  window_sum <- function(from, to) {
    periods <- .islh_surv_period_sequence(from, to, interval)
    sub <- work[work$.islh_date %in% periods, , drop = FALSE]
    totals <- tapply(sub$.islh_value, sub$.islh_group, sum)
    present <- tapply(sub$.islh_date, sub$.islh_group, length)
    key <- as.character(groups$.islh_group)
    value <- as.numeric(totals[key])
    complete <- !is.na(present[key]) & present[key] == length(periods)
    value[!complete] <- NA_real_
    list(value = value, periods = length(periods))
  }

  pieces <- lapply(names(windows), function(name) {
    w <- windows[[name]]
    now <- window_sum(w$current[1], w$current[2])
    then <- window_sum(w$reference[1], w$reference[2])
    piece <- groups
    piece$comparison <- name
    piece$current_from <- w$current[1]
    piece$current_to <- .islh_surv_period_end(w$current[2], interval, anchor)
    piece$reference_from <- w$reference[1]
    piece$reference_to <- .islh_surv_period_end(
      w$reference[2],
      interval,
      anchor
    )
    piece$current_periods <- now$periods
    piece$reference_periods <- then$periods
    piece$current <- now$value
    piece$reference <- then$value
    piece
  })
  out <- do.call(rbind, pieces)

  out$difference <- out$current - out$reference
  usable <- !is.na(out$reference) & out$reference > 0
  out$ratio <- ifelse(usable, out$current / out$reference, NA_real_)
  out$percent_change <- (out$ratio - 1) * 100

  missing <- sum(is.na(out$current) | is.na(out$reference))
  if (missing > 0L) {
    .islh_inform(c(
      "{missing} comparison{?s} {?has/have} a missing period in a window, so
       {?its/their} value is {.code NA}.",
      i = "Supply one row per group and period, including zeros, for every
           window compared."
    ))
  }

  out <- out[order(match(out$comparison, names(windows))), , drop = FALSE]
  out <- out[, c(by_names, output_columns), drop = FALSE]
  rownames(out) <- NULL
  out
}
