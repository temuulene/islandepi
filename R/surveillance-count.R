#' Count events in complete reporting periods
#'
#' `islh_count_events()` converts event-level or already aggregated data into a
#' consistent period table. It handles epidemiological and ISO weeks, constructs
#' missing zero-count periods and marks periods that extend beyond the reporting
#' cutoff.
#'
#' @param timezone Reporting timezone for timestamps. Defaults to
#'   `"America/Vancouver"`; Date inputs retain their calendar date. A warning
#'   says when converting a timestamp moves its date; see the timestamps
#'   section.
#' @param data A data frame.
#' @param date Date column.
#' @param id Optional identifier column. When supplied, distinct non-missing IDs
#'   are counted within each group and period, so a person seen in two weeks
#'   counts once in each. Summing the result therefore does not give the number
#'   of distinct people. When neither `id` nor `count` is supplied, rows are
#'   counted.
#' @param source_interval Duration of preaggregated counts when no metadata
#'   accompanies them. Required for such counts unless their dates are
#'   consecutive days, since rows a week apart could be weekly totals or daily
#'   counts with gaps.
#' @param count Optional column of pre-aggregated whole counts. `id` and `count`
#'   cannot both be supplied.
#' @param by Optional tidy-select specification of grouping columns.
#' @param interval Reporting interval: `"day"`, `"week"`, `"isoweek"`,
#'   `"epiweek"`, `"month"`, `"quarter"` or `"year"`.
#' @param week_start Start of an ordinary week, where 1 is Monday and 7 is
#'   Sunday. ISO and epidemiological weeks always use Monday and Sunday,
#'   respectively.
#' @param from,to Optional inclusive date boundaries. Defaults to the observed
#'   date range.
#' @param fill Whether to add zero-count periods within the declared coverage
#'   window. `from` and `to` must describe confirmed reporting coverage, not
#'   simply the first and last event. Missing feeds must be resolved first.
#' @param include_partial Whether to retain a period extending before `from`
#'   or after `to`. Baselines and snapshots refuse partial periods. When
#'   `FALSE` (the default) drops events, a message says how many and from which
#'   periods. With `from` and `to` left out, the window runs from the first to
#'   the last event, so weekly and longer periods at either end are usually
#'   partial.
#' @param groups Optional expected groups. For one `by` column this may be a
#'   vector; for multiple columns supply a data frame containing valid group
#'   combinations. This is how groups with no events anywhere in the selected
#'   window can still be shown.
#'
#' @return A data frame containing the grouping columns, `period_start`,
#'   `period_end`, `count` and `partial_period`. The result carries the
#'   reporting period it was built with; see the reporting-period section.
#'
#' @section Timestamps:
#'
#' A date-time is converted to `timezone` before its calendar date is taken.
#' That is right for true instants. It is wrong for local clock times that
#' were labelled UTC when they were read, which is what database drivers such
#' as `odbc` do by default with a SQL Server `datetime` column: an event at
#' 02:00 on January 15 is then counted on January 14.
#'
#' A warning says when the conversion moves any date. Settle it before
#' counting. `as.POSIXct(format(x), tz = "America/Vancouver")` relabels local
#' clock times without moving them, and
#' `attr(x, "tzone") <- "America/Vancouver"` converts true instants. Either
#' way the warning stops. Date columns are never converted.
#'
#' The conversion uses the time zone rules that R has. British Columbia
#' stopped changing its clocks in November 2026, and rules older than that
#' place winter instants an hour early. To check an installation, this should
#' give 05:00:
#'
#' ```r
#' noon_utc <- as.POSIXct("2027-01-15 12:00", tz = "UTC")
#' format(noon_utc, "%H:%M", tz = "America/Vancouver")
#' ```
#'
#' @section Reporting-period metadata:
#'
#' The result carries `interval`, `week_start` and the `from`-`to` window as
#' attributes. [islh_surveillance_baseline()] and
#' [islh_surveillance_snapshot()] read them to check that a comparison covers
#' the same duration on both sides, so a seven-day snapshot cannot be measured
#' against a baseline of single daily counts.
#'
#' With `id`, the result also records how many identifiers appear in more
#' than one period, and in more than one group. Summing the counts later, with
#' `count` into longer periods or in [islh_surveillance_snapshot()], warns
#' when those numbers are not zero, because the sum would count those people
#' more than once.
#'
#' Transformations may preserve stale attributes or drop them. Calendar and
#' coverage checks use the actual rows as well as metadata. After changing the
#' aggregation, recount events or declare the correct source interval.
#'
#' @examples
#' # Weekly cases by health service delivery area in the simulated outbreak.
#' # The window runs Monday to Sunday, so every week is complete.
#' islh_count_events(
#'   islh_outbreak,
#'   date = date_onset,
#'   id = case_id,
#'   by = hsda,
#'   interval = "week",
#'   week_start = 1,
#'   from = "2025-11-03",
#'   to = "2026-03-08"
#' )
#'
#' # Left to the event dates, the window starts and ends mid-week, and the
#' # partial weeks at either end are dropped with a message.
#' weekly <- islh_count_events(
#'   islh_outbreak,
#'   date = date_reported,
#'   id = case_id,
#'   interval = "week"
#' )
#'
#' @export
islh_count_events <- function(
  data,
  date,
  id = NULL,
  count = NULL,
  by = NULL,
  interval = c(
    "day",
    "week",
    "isoweek",
    "epiweek",
    "month",
    "quarter",
    "year"
  ),
  week_start = 1,
  from = NULL,
  to = NULL,
  fill = TRUE,
  include_partial = FALSE,
  groups = NULL,
  timezone = "America/Vancouver",
  source_interval = NULL
) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  interval <- .islh_surv_interval(interval)
  week_start <- .islh_surv_week_start(interval, week_start)
  fill <- .islh_check_flag(fill, "fill")
  include_partial <- .islh_check_flag(include_partial, "include_partial")

  date_name <- .islh_surv_select(data, rlang::enquo(date), "date", 1L, 1L)
  id_name <- .islh_surv_select(data, rlang::enquo(id), "id", 0L, 1L)
  count_name <- .islh_surv_select(data, rlang::enquo(count), "count", 0L, 1L)
  by_names <- .islh_surv_select(data, rlang::enquo(by), "by")

  if (length(id_name) > 0L && length(count_name) > 0L) {
    .islh_abort("Supply only one of {.arg id} and {.arg count}.")
  }
  .islh_surv_check_reserved(
    by_names,
    c("period_start", "period_end", "count", "partial_period")
  )

  source_meta <- .islh_surv_meta(data)
  if (length(count_name) > 0L && !is.null(source_interval)) {
    source_interval <- .islh_surv_interval(
      source_interval,
      arg = "source_interval"
    )
    if (!is.null(source_meta)) {
      .islh_surv_resolve_interval(
        source_interval,
        source_meta,
        as.Date(character()),
        arg = "source_interval"
      )
    } else {
      source_meta <- list(
        interval = .islh_surv_interval(
          source_interval,
          arg = "source_interval"
        ),
        periods = 1L,
        week_start = week_start
      )
    }
  }
  work <- as.data.frame(data)
  work$.islh_date <- .islh_surv_as_date(
    work[[date_name]],
    date_name,
    timezone = timezone
  )
  # Counts with no record of what a row covers are checked like recorded
  # ones. Consecutive days can only be daily counts; any other spacing could
  # be weekly totals or daily counts with gaps, and recounting weekly totals
  # as days would put each week on its first day.
  if (length(count_name) > 0L && is.null(source_meta)) {
    inferred <- .islh_surv_infer_interval(work$.islh_date)
    if (is.null(inferred)) {
      .islh_abort(c(
        "{.arg data} does not record what one row of {.arg count} covers.",
        x = "Its dates are not consecutive days, so a row could be a day, a
             week or longer.",
        i = "Name it with {.arg source_interval}, or keep the metadata
             {.fn islh_count_events} records."
      ))
    }
    source_meta <- list(interval = inferred, periods = 1L, week_start = NULL)
  }

  if (is.null(from)) {
    if (nrow(work) == 0L) {
      .islh_abort("{.arg from} is required when {.arg data} has no rows.")
    }
    from <- min(work$.islh_date)
  } else {
    from <- .islh_surv_as_date(from, "from", scalar = TRUE, timezone = timezone)
  }
  if (is.null(to)) {
    if (nrow(work) == 0L) {
      .islh_abort("{.arg to} is required when {.arg data} has no rows.")
    }
    to <- if (length(count_name) > 0L && !is.null(source_meta)) {
      max(.islh_surv_period_end(
        work$.islh_date,
        source_meta$interval,
        source_meta$week_start %||% week_start
      ))
    } else {
      max(work$.islh_date)
    }
  } else {
    to <- .islh_surv_as_date(to, "to", scalar = TRUE, timezone = timezone)
  }
  if (from > to) {
    .islh_abort("{.arg from} must not be after {.arg to}.")
  }

  if (length(count_name) > 0L && !is.null(source_meta)) {
    .islh_surv_check_alignment(
      source_meta,
      interval,
      week_start,
      work$.islh_date
    )
    source_ends <- .islh_surv_period_end(
      work$.islh_date,
      source_meta$interval,
      source_meta$week_start %||% week_start
    )
    overlaps <- work$.islh_date <= to & source_ends >= from
    if (any(overlaps & (work$.islh_date < from | source_ends > to))) {
      .islh_abort(
        "The requested window splits source periods; recount the events."
      )
    }
    .islh_surv_check_partial(work, overlaps)
  }
  if (length(count_name) > 0L && !is.null(source_meta)) {
    work$.islh_value <- .islh_check_counts(
      work[[count_name]],
      count_name,
      allow_na = FALSE
    )
    .islh_surv_complete_source(
      work,
      by_names,
      source_meta,
      from,
      to,
      policy = "error",
      context = "count",
      groups = if (!is.null(groups)) {
        .islh_surv_groups(groups, by_names, work)
      }
    )
  }
  selected <- work$.islh_date >= from & work$.islh_date <= to
  work <- work[selected, , drop = FALSE]

  if (length(id_name) > 0L && any(.islh_surv_missing(work[[id_name]]))) {
    n_missing <- sum(.islh_surv_missing(work[[id_name]]))
    .islh_abort(c(
      "{.arg id} contains missing identifiers in the selected date range.",
      x = "Found {n_missing} affected row{?s}.",
      i = "Use {.fn islh_check_events} to identify the records."
    ))
  }
  if (length(count_name) > 0L) {
    work$.islh_count <- .islh_check_counts(
      work[[count_name]],
      count_name,
      allow_na = FALSE
    )
  }

  group_values <- .islh_surv_groups(groups, by_names, work)
  if (length(by_names) && !is.null(groups)) {
    unexpected <- dplyr::anti_join(
      unique(work[by_names]),
      group_values,
      by = by_names,
      na_matches = "na"
    )
    if (nrow(unexpected)) {
      # Group labels go in as cli values, so braces in a label are shown.
      shown <- .islh_key_labels(unexpected, by_names)
      .islh_abort(c(
        "Observed groups are absent from {.arg groups}.",
        x = "Not in {.arg groups}: {shown}.",
        i = "Correct the group labels or filter the input explicitly."
      ))
    }
  }
  work$.islh_period_start <- .islh_surv_period_start(
    work$.islh_date,
    interval,
    week_start
  )
  work$.islh_period_end <- .islh_surv_period_end(
    work$.islh_period_start,
    interval,
    week_start
  )

  id_repeats <- NULL
  if (length(id_name) > 0L) {
    kept <- isTRUE(include_partial) |
      (work$.islh_period_start >= from & work$.islh_period_end <= to)
    id_repeats <- .islh_surv_id_repeats(
      work[kept, , drop = FALSE],
      id_name,
      by_names
    )
  } else if (length(count_name) > 0L && !is.null(source_meta)) {
    .islh_surv_check_id_sum(
      source_meta,
      by_names,
      merges_periods = !.islh_surv_same_duration(
        .islh_surv_duration(interval),
        .islh_surv_duration(source_meta$interval, source_meta$periods)
      )
    )
    id_repeats <- source_meta$id_repeats
  }

  group_columns <- c(by_names, ".islh_period_start", ".islh_period_end")
  grouped <- dplyr::group_by(
    work,
    dplyr::across(tidyselect::all_of(group_columns))
  )
  if (length(id_name) > 0L) {
    result <- dplyr::summarise(
      grouped,
      count = dplyr::n_distinct(.data[[id_name]]),
      .groups = "drop"
    )
  } else if (length(count_name) > 0L) {
    result <- dplyr::summarise(
      grouped,
      count = sum(.data$.islh_count),
      .groups = "drop"
    )
  } else {
    result <- dplyr::summarise(grouped, count = dplyr::n(), .groups = "drop")
  }
  names(result)[names(result) == ".islh_period_start"] <- "period_start"
  names(result)[names(result) == ".islh_period_end"] <- "period_end"
  result$partial_period <- result$period_start < from | result$period_end > to

  if (!isTRUE(include_partial)) {
    # Dropping these is the safe default, since a partial week or month looks
    # like a fall in activity. Doing it silently is not: with `from` and `to`
    # left out, the window starts and ends on event dates, so the first and
    # last periods are usually partial.
    dropped <- result[result$partial_period & result$count > 0, , drop = FALSE]
    if (nrow(dropped) > 0L) {
      n_dropped <- sum(dropped$count)
      spans <- unique(paste(
        format(dropped$period_start, "%Y-%m-%d"),
        "to",
        format(dropped$period_end, "%Y-%m-%d")
      ))
      .islh_inform(c(
        "Left out {n_dropped} event{?s} in {length(spans)} partial
         period{?s}: {spans}.",
        i = "A period is partial when it starts before {.arg from}
             ({format(from)}) or ends after {.arg to} ({format(to)}).",
        i = "Set {.arg from} and {.arg to} to period boundaries, or keep
             these periods with {.code include_partial = TRUE}."
      ))
    }
    result <- result[!result$partial_period, , drop = FALSE]
  }

  if (isTRUE(fill)) {
    from_period <- .islh_surv_period_start(from, interval, week_start)
    to_period <- .islh_surv_period_start(to, interval, week_start)
    periods <- .islh_surv_period_sequence(from_period, to_period, interval)
    period_ends <- .islh_surv_period_end(periods, interval, week_start)
    if (!isTRUE(include_partial)) {
      periods <- periods[periods >= from & period_ends <= to]
    }

    group_values <- .islh_surv_groups(groups, by_names, work)
    grid <- .islh_surv_grid(group_values, periods, by_names)
    if (nrow(grid) > 0L) {
      grid$period_end <- .islh_surv_period_end(
        grid$period_start,
        interval,
        week_start
      )
      join_columns <- c(by_names, "period_start", "period_end")
      result <- dplyr::left_join(
        grid,
        result[, c(join_columns, "count"), drop = FALSE],
        by = join_columns,
        relationship = "one-to-one",
        na_matches = "na"
      )
      result$count[is.na(result$count)] <- 0
      result$partial_period <- result$period_start < from |
        result$period_end > to
    } else {
      result <- result[FALSE, , drop = FALSE]
    }
  }

  order_columns <- c(by_names, "period_start")
  if (nrow(result) > 0L) {
    result <- result[do.call(order, result[order_columns]), , drop = FALSE]
  }
  result <- result[,
    c(
      by_names,
      "period_start",
      "period_end",
      "count",
      "partial_period"
    ),
    drop = FALSE
  ]
  .islh_surv_set_meta(
    result,
    interval = interval,
    week_start = week_start,
    periods = 1L,
    from = from,
    to = to,
    id_repeats = id_repeats,
    by = by_names
  )
}
