# Columns islh_surveillance_baseline() adds after the grouping columns.
.islh_baseline_columns <- c(
  "reference_method",
  "reference_n",
  "reference_mean",
  "reference_sd",
  "reference_min",
  "reference_max",
  "reference_adjustment",
  "lower_limit",
  "upper_limit"
)

#' Calculate a descriptive surveillance baseline
#'
#' The function expects a complete period table, including real zero-count
#' periods. It calculates transparent reference statistics but does not decide
#' whether an exceedance is an outbreak or requires public-health action.
#'
#' @param timezone Reporting timezone for timestamps. Defaults to
#'   `"America/Vancouver"`; Date inputs retain their calendar date.
#' @param data A data frame containing one row per group and reference period.
#' @param date Period date column.
#' @param value Whole-count column.
#' @param by Optional tidy-select specification of grouping columns. They
#'   must not share a name with a column the result creates, such as
#'   `reference_n` or `upper_limit`.
#' @param reference_periods Optional vector of dates to include. When omitted,
#'   every row is used.
#' @param method One of `"mean_sd"`, `"range"` or `"quantile"`.
#' @param multiplier Number of standard deviations used by `"mean_sd"`.
#' @param prediction_adjustment Apply `sqrt(1 + 1 / k)` to the standard
#'   deviation limit, where `k` is the number of reference periods.
#' @param probs Lower and upper probabilities used by `"quantile"`.
#' @param minimum_periods Minimum reference periods required in every group.
#' @param interval Reporting interval of one row of `data`. `NULL` takes it
#'   from the metadata [islh_count_events()] leaves on its result. A table
#'   without that metadata must name it, unless its dates are consecutive
#'   days: rows a week apart could be weekly totals or daily counts with days
#'   missing, and the dates cannot say which.
#'
#' @return One row per group with the number of reference periods, descriptive
#'   statistics and lower and upper limits. The result records the duration its
#'   limits describe; see the reporting-period section.
#'
#' @section Reporting-period metadata:
#'
#' Each limit describes one period of `interval`, and the result records that
#' duration. [islh_surveillance_snapshot()] compares it against the window it
#' displays and refuses a comparison between different durations, so a weekly
#' baseline can be used with a seven-day snapshot but a daily one cannot.
#'
#' Supply `interval` when the reporting period cannot be determined from the
#' data, otherwise the snapshot has nothing to check against and will say so.
#'
#' @examples
#' weekly <- dplyr::tibble(
#'   site = rep(c("A", "B"), each = 8),
#'   week = rep(seq(as.Date("2026-01-04"), by = "week", length.out = 8), 2),
#'   count = c(2, 4, 3, 5, 2, 4, 3, 5, 6, 7, 5, 8, 6, 7, 5, 8)
#' )
#'
#' islh_surveillance_baseline(
#'   weekly,
#'   date = week,
#'   value = count,
#'   by = site,
#'   interval = "week",
#'   method = "mean_sd"
#' )
#'
#' @export
islh_surveillance_baseline <- function(
  data,
  date,
  value,
  by = NULL,
  reference_periods = NULL,
  method = c("mean_sd", "range", "quantile"),
  multiplier = 2,
  prediction_adjustment = TRUE,
  probs = c(0.05, 0.95),
  minimum_periods = 4L,
  interval = NULL,
  timezone = "America/Vancouver"
) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  data_meta <- .islh_surv_meta(data)
  method <- match.arg(method)
  prediction_adjustment <- .islh_check_flag(
    prediction_adjustment,
    "prediction_adjustment"
  )
  if (
    !is.numeric(multiplier) ||
      length(multiplier) != 1L ||
      is.na(multiplier) ||
      !is.finite(multiplier) ||
      multiplier < 0
  ) {
    .islh_abort("{.arg multiplier} must be one non-negative finite number.")
  }
  if (
    !is.numeric(minimum_periods) ||
      length(minimum_periods) != 1L ||
      is.na(minimum_periods) ||
      minimum_periods != round(minimum_periods) ||
      minimum_periods < 1L
  ) {
    .islh_abort("{.arg minimum_periods} must be one positive whole number.")
  }
  minimum_periods <- as.integer(minimum_periods)
  if (
    !is.numeric(probs) ||
      length(probs) != 2L ||
      anyNA(probs) ||
      any(!is.finite(probs)) ||
      probs[1] < 0 ||
      probs[2] > 1 ||
      probs[1] >= probs[2]
  ) {
    .islh_abort(
      "{.arg probs} must contain increasing lower and upper probabilities."
    )
  }

  date_name <- .islh_surv_select(data, rlang::enquo(date), "date", 1L, 1L)
  value_name <- .islh_surv_select(data, rlang::enquo(value), "value", 1L, 1L)
  by_names <- .islh_surv_select(data, rlang::enquo(by), "by")
  .islh_surv_check_reserved(
    by_names,
    c(.islh_baseline_columns, "reference_q_low", "reference_q_high")
  )

  work <- as.data.frame(data)
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

  reference_groups <- unique(work[by_names])
  if (!is.null(reference_periods)) {
    reference_periods <- .islh_surv_as_date(
      reference_periods,
      "reference_periods",
      timezone = timezone
    )
    work <- work[work$.islh_date %in% reference_periods, , drop = FALSE]
  }
  if (nrow(work) == 0L) {
    .islh_abort("No rows remain in the requested reference periods.")
  }

  reference_interval <- .islh_surv_resolve_interval(
    interval,
    data_meta,
    work$.islh_date
  )

  key_columns <- c(by_names, ".islh_date")
  duplicates <- work |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(key_columns))) |>
    dplyr::summarise(.n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(.data$.n > 1L)
  if (nrow(duplicates) > 0L) {
    .islh_abort(c(
      "{.arg data} has more than one value per group and date.",
      x = "Found {nrow(duplicates)} duplicated group-period combination{?s}.",
      i = "Aggregate the data with {.fn islh_count_events} first."
    ))
  }

  if (is.null(reference_interval)) {
    .islh_abort(c(
      "Supply {.arg interval}.",
      x = "{.arg data} does not record what one row covers, and its dates are
           not consecutive days.",
      i = "Rows seven days apart could be weekly totals or daily counts with
           days missing. Name the period, or build the counts with
           {.fn islh_count_events}, which records it."
    ))
  }
  reference_week_start <- if (!is.null(data_meta$week_start)) {
    data_meta$week_start
  } else if (reference_interval %in% c("week", "isoweek", "epiweek")) {
    as.integer(lubridate::wday(min(work$.islh_date), week_start = 1))
  } else {
    1L
  }
  reference_week_start <- .islh_surv_week_start(
    reference_interval,
    reference_week_start
  )
  if (!is.null(data_meta)) {
    .islh_surv_check_alignment(
      data_meta,
      reference_interval,
      reference_week_start,
      work$.islh_date
    )
  }
  .islh_surv_check_alignment(
    list(
      interval = reference_interval,
      periods = 1L,
      week_start = reference_week_start
    ),
    reference_interval,
    reference_week_start,
    work$.islh_date
  )
  if (
    "partial_period" %in%
      names(work) &&
      any(is.na(work$partial_period) | work$partial_period)
  ) {
    .islh_abort(
      "The baseline contains partial periods; use complete reference periods."
    )
  }
  expected_dates <- if (!is.null(reference_periods)) {
    sort(unique(reference_periods))
  } else {
    .islh_surv_period_sequence(
      min(work$.islh_date),
      max(work$.islh_date),
      reference_interval
    )
  }
  expected <- .islh_surv_grid(reference_groups, expected_dates, by_names)
  names(expected)[names(expected) == "period_start"] <- ".islh_date"
  absent <- dplyr::anti_join(expected, work, by = c(by_names, ".islh_date"))
  if (nrow(absent)) {
    .islh_abort(
      "The baseline has missing reference periods; confirm coverage before filling zeros."
    )
  }
  if (method == "mean_sd" && minimum_periods < 2L) {
    .islh_abort("Mean-SD baselines require at least two reference periods.")
  }
  grouped <- dplyr::group_by(
    work,
    dplyr::across(tidyselect::all_of(by_names))
  )
  summary <- dplyr::summarise(
    grouped,
    reference_n = dplyr::n(),
    reference_mean = mean(.data$.islh_value),
    reference_sd = stats::sd(.data$.islh_value),
    reference_min = min(.data$.islh_value),
    reference_max = max(.data$.islh_value),
    reference_q_low = as.numeric(stats::quantile(
      .data$.islh_value,
      probs = probs[1],
      names = FALSE,
      type = 7
    )),
    reference_q_high = as.numeric(stats::quantile(
      .data$.islh_value,
      probs = probs[2],
      names = FALSE,
      type = 7
    )),
    .groups = "drop"
  )

  insufficient <- summary$reference_n < minimum_periods
  if (any(insufficient)) {
    n_insufficient <- sum(insufficient)
    .islh_abort(c(
      "The surveillance baseline is too short.",
      x = "{n_insufficient} group{?s} {?has/have} fewer than
           {minimum_periods} reference period{?s}.",
      i = "Add complete reference periods or lower {.arg minimum_periods} explicitly."
    ))
  }

  adjustment <- if (isTRUE(prediction_adjustment)) {
    sqrt(1 + 1 / summary$reference_n)
  } else {
    rep(1, nrow(summary))
  }
  summary$reference_adjustment <- adjustment
  summary$reference_method <- method

  if (method == "mean_sd") {
    spread <- multiplier * summary$reference_sd * adjustment
    summary$lower_limit <- pmax(0, summary$reference_mean - spread)
    summary$upper_limit <- summary$reference_mean + spread
  } else if (method == "range") {
    summary$lower_limit <- summary$reference_min
    summary$upper_limit <- summary$reference_max
  } else {
    summary$lower_limit <- summary$reference_q_low
    summary$upper_limit <- summary$reference_q_high
  }

  out <- summary[, c(by_names, .islh_baseline_columns), drop = FALSE]

  # Attach fresh provenance after assembling the output columns.
  attr(out, "islh_reference") <- list(
    dates = expected_dates,
    method = method,
    multiplier = multiplier,
    probs = probs,
    prediction_adjustment = prediction_adjustment,
    minimum_periods = minimum_periods,
    timezone = timezone,
    package_version = as.character(utils::packageVersion("islandepi"))
  )
  .islh_surv_set_meta(
    out,
    interval = reference_interval,
    week_start = reference_week_start,
    periods = 1L,
    from = min(work$.islh_date),
    to = max(work$.islh_date)
  )
}
