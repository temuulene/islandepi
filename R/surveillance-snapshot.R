#' Build a current surveillance snapshot
#'
#' `islh_surveillance_snapshot()` turns complete period counts into the familiar
#' operational table: one row per group, recent periods in columns, a current
#' total and optional historical reference statistics.
#'
#' @param timezone Reporting timezone for timestamps. Date values are unchanged.
#' @param missing_periods How to handle missing source periods: stop (default),
#'   retain missing values, or explicitly confirm they are zero. Partial
#'   periods always stop.
#' @param comparison Strict `"above"` (default) or inclusive `"at_or_above"`.
#' @param zero_reference By default a zero upper limit only flags positive
#'   totals. `"compare"` applies the chosen comparison even at zero.
#' @param data A complete count table, normally returned by
#'   [islh_count_events()].
#' @param date Period date column.
#' @param value Whole-count column.
#' @param by Optional tidy-select specification of grouping columns. They
#'   must not share a name with a column the result creates, such as `total`,
#'   `complete` or `upper_limit`.
#' @param end Last period to show. Defaults to the latest date in `data`.
#' @param periods Number of periods to show.
#' @param interval Period spacing used to construct the window.
#' @param week_start Start of an ordinary week, from 1 (Monday) to 7 (Sunday).
#' @param baseline Optional result from [islh_surveillance_baseline()]. Its
#'   limits must describe the same duration as the displayed `total`. For
#'   example, a seven-day snapshot should use a baseline of historical
#'   seven-day or weekly totals, not historical daily counts. Mismatched
#'   durations are refused rather than reported. A message names any group
#'   with no row in `baseline`, since its `exceeds_reference` is missing.
#' @param baseline_interval Duration one `baseline` limit describes, named
#'   explicitly. Use it for limits that did not come from
#'   [islh_surveillance_baseline()], such as last season's totals read from a
#'   spreadsheet. `NULL` uses the duration the baseline records.
#' @param include_total Add an `All` row. This is only available with one
#'   grouping column and should only be used for mutually exclusive groups.
#' @param total_label Label used for the total group.
#'
#' @return A wide data frame containing groups, `total`, `complete`, optional
#'   baseline fields, `exceeds_reference`, and one column per displayed period.
#'   `complete` is `TRUE` when every displayed period has a count. With
#'   `missing_periods = "missing"`, a group with a gap has `complete = FALSE`
#'   and a missing `total`, so it is never compared with the baseline as if
#'   the gap were zero.
#'
#' @section Alert boundary:
#'
#' `exceeds_reference` uses the explicit `comparison` rule. The default is
#' strictly above the upper limit; `"at_or_above"` preserves the older
#' inclusive rule. With the default `zero_reference`, a zero against an
#' all-zero baseline is not flagged. Missing totals or limits give `NA`.
#'
#' These are descriptive screening references, not calibrated outbreak
#' detection thresholds. PHASE must choose the screening policy explicitly.
#'
#' @section Reporting-period metadata:
#'
#' A `total` covering seven days cannot be compared with a baseline built from
#' single daily counts, so the function checks both sides before it reports
#' anything:
#'
#' * `data` must hold periods that divide evenly into `interval`. Daily counts
#'   can be summed into a weekly snapshot; weekly counts cannot be split into
#'   a daily one.
#' * `baseline` must describe the same duration as `periods` of `interval`.
#'   Seven daily columns total one week, so a weekly baseline fits and a daily
#'   baseline does not.
#'
#' When `data` holds distinct-ID counts from [islh_count_events()], a warning
#' says when `total`, or the `include_total` row, adds up counts in which some
#' identifiers appear on more than one day or in more than one group. Such a
#' total counts person-periods, not people.
#'
#' These checks read metadata that [islh_count_events()] and
#' [islh_surveillance_baseline()] leave on their results. When a baseline
#' carries none, the comparison cannot be verified and is refused; pass
#' `interval` to [islh_surveillance_baseline()] to record it, or name the
#' duration here with `baseline_interval`.
#'
#' @examples
#' daily <- data.frame(
#'   site = rep(c("A", "B"), each = 7),
#'   date = rep(seq(as.Date("2026-08-01"), by = "day", length.out = 7), 2),
#'   count = c(0, 1, 2, 0, 1, 0, 2, 1, 2, 1, 3, 1, 0, 1)
#' )
#'
#' islh_surveillance_snapshot(
#'   daily,
#'   date = date,
#'   value = count,
#'   by = site,
#'   periods = 7,
#'   include_total = TRUE
#' )
#'
#' @export
islh_surveillance_snapshot <- function(
  data,
  date,
  value,
  by = NULL,
  end = NULL,
  periods = 7L,
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
  baseline = NULL,
  include_total = FALSE,
  total_label = "All",
  baseline_interval = NULL,
  timezone = "America/Vancouver",
  missing_periods = c("error", "missing", "zero"),
  comparison = c("above", "at_or_above"),
  zero_reference = c("positive_only", "compare")
) {
  missing_periods <- match.arg(missing_periods)
  comparison <- match.arg(comparison)
  zero_reference <- match.arg(zero_reference)
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  # Read both sides' metadata before anything is reshaped or coerced below.
  data_meta <- .islh_surv_meta(data)
  baseline_meta <- .islh_surv_meta(baseline)
  interval <- .islh_surv_interval(interval)
  week_start <- .islh_surv_week_start(interval, week_start)
  include_total <- .islh_check_flag(include_total, "include_total")
  if (
    !is.numeric(periods) ||
      length(periods) != 1L ||
      is.na(periods) ||
      periods != round(periods) ||
      periods < 1L
  ) {
    .islh_abort("{.arg periods} must be one positive whole number.")
  }
  periods <- as.integer(periods)
  .islh_surv_check_source(data_meta, interval)
  .islh_surv_check_baseline(
    baseline,
    baseline_meta,
    interval,
    periods,
    baseline_interval
  )
  if (
    !is.character(total_label) ||
      length(total_label) != 1L ||
      is.na(total_label) ||
      !nzchar(trimws(total_label))
  ) {
    .islh_abort("{.arg total_label} must be one non-empty string.")
  }

  date_name <- .islh_surv_select(data, rlang::enquo(date), "date", 1L, 1L)
  value_name <- .islh_surv_select(data, rlang::enquo(value), "value", 1L, 1L)
  by_names <- .islh_surv_select(data, rlang::enquo(by), "by")
  .islh_surv_check_reserved(
    by_names,
    c(
      "total",
      "complete",
      "exceeds_reference",
      "period_start",
      .islh_baseline_columns
    )
  )
  if (isTRUE(include_total) && length(by_names) != 1L) {
    .islh_abort(
      "{.arg include_total} requires exactly one {.arg by} column."
    )
  }

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
  if (nrow(work) == 0L) {
    .islh_abort("{.arg data} must contain at least one row.")
  }

  if (is.null(end)) {
    end <- max(work$.islh_date)
  } else {
    end <- .islh_surv_as_date(end, "end", scalar = TRUE, timezone = timezone)
  }
  end <- .islh_surv_period_start(end, interval, week_start)
  target_periods <- .islh_surv_periods_back(end, periods, interval)
  .islh_surv_check_reserved(by_names, format(target_periods, "%Y-%m-%d"))
  if (is.null(data_meta)) {
    inferred <- .islh_surv_infer_interval(work$.islh_date)
    data_meta <- list(
      interval = inferred %||% interval,
      periods = 1L,
      week_start = if (!is.null(inferred) && inferred == "week") {
        as.integer(lubridate::wday(min(work$.islh_date), week_start = 1))
      } else {
        week_start
      }
    )
  }
  .islh_surv_check_alignment(data_meta, interval, week_start, work$.islh_date)
  .islh_surv_check_id_sum(
    data_meta,
    by_names,
    merges_periods = !.islh_surv_same_duration(
      .islh_surv_duration(interval, periods),
      .islh_surv_duration(data_meta$interval, data_meta$periods)
    ),
    total = include_total
  )
  window_end <- .islh_surv_period_end(max(target_periods), interval, week_start)
  if (
    !is.null(baseline_meta$week_start) &&
      baseline_meta$interval %in% c("week", "isoweek", "epiweek") &&
      lubridate::wday(min(target_periods), week_start = 1) !=
        baseline_meta$week_start
  ) {
    .islh_abort(
      "The baseline week anchor differs from the snapshot window; rebuild matching reference windows."
    )
  }
  work <- .islh_surv_complete_source(
    work,
    by_names,
    data_meta,
    min(target_periods),
    window_end,
    missing_periods
  )
  work$.islh_period <- .islh_surv_period_start(
    work$.islh_date,
    interval,
    week_start
  )

  group_columns <- c(by_names, ".islh_period")
  counts <- work |>
    dplyr::filter(.data$.islh_period %in% target_periods) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_columns))) |>
    dplyr::summarise(.islh_value = sum(.data$.islh_value), .groups = "drop")

  group_values <- unique(work[by_names])
  grid <- .islh_surv_grid(group_values, target_periods, by_names)
  names(grid)[names(grid) == "period_start"] <- ".islh_period"
  counts <- dplyr::left_join(
    grid,
    counts,
    by = group_columns,
    relationship = "one-to-one",
    na_matches = "na"
  )

  if (isTRUE(include_total)) {
    group_name <- by_names[[1]]
    counts[[group_name]] <- as.character(counts[[group_name]])
    if (total_label %in% counts[[group_name]]) {
      .islh_abort(
        "{.arg total_label} is already present in the grouping column."
      )
    }
    total <- counts |>
      dplyr::group_by(.data$.islh_period) |>
      dplyr::summarise(.islh_value = sum(.data$.islh_value), .groups = "drop")
    total[[group_name]] <- total_label
    total <- total[, c(group_name, ".islh_period", ".islh_value")]
    counts <- dplyr::bind_rows(counts, total)
  }

  counts$.islh_label <- format(counts$.islh_period, "%Y-%m-%d")
  wide <- counts |>
    dplyr::select(
      tidyselect::all_of(by_names),
      ".islh_label",
      ".islh_value"
    ) |>
    tidyr::pivot_wider(
      names_from = ".islh_label",
      values_from = ".islh_value",
      values_fill = NA_real_,
      names_sort = TRUE
    )

  date_columns <- format(target_periods, "%Y-%m-%d")
  wide$total <- rowSums(wide[, date_columns, drop = FALSE])
  wide$complete <- rowSums(is.na(wide[, date_columns, drop = FALSE])) == 0L

  reference_columns <- character()
  if (!is.null(baseline)) {
    if (!is.data.frame(baseline)) {
      .islh_abort("{.arg baseline} must be a data frame or NULL.")
    }
    missing <- setdiff(by_names, names(baseline))
    if (length(missing) > 0L) {
      .islh_abort(c(
        "{.arg baseline} is missing grouping columns.",
        x = "Missing: {.val {missing}}."
      ))
    }
    reference_columns <- setdiff(names(baseline), by_names)
    if (
      any(
        reference_columns %in%
          c("total", "complete", "exceeds_reference", date_columns)
      )
    ) {
      .islh_abort("Baseline columns conflict with snapshot output names.")
    }
    if (
      !"upper_limit" %in% names(baseline) ||
        !is.numeric(baseline$upper_limit) ||
        any(
          !is.na(baseline$upper_limit) &
            (!is.finite(baseline$upper_limit) | baseline$upper_limit < 0)
        )
    ) {
      .islh_abort(
        "Baseline upper_limit must contain non-negative finite values or NA."
      )
    }
    if (length(by_names) == 0L) {
      if (nrow(baseline) != 1L) {
        .islh_abort(
          "An ungrouped {.arg baseline} must contain exactly one row."
        )
      }
      wide <- cbind(wide, baseline[rep(1L, nrow(wide)), , drop = FALSE])
    } else {
      for (field in by_names) {
        baseline[[field]] <- as.character(baseline[[field]])
        wide[[field]] <- as.character(wide[[field]])
      }
      duplicate <- duplicated(baseline[by_names])
      if (any(duplicate)) {
        .islh_abort("{.arg baseline} must contain one row per group.")
      }
      baseline$.islh_matched <- TRUE
      wide <- dplyr::left_join(
        wide,
        baseline,
        by = by_names,
        relationship = "many-to-one",
        na_matches = "na"
      )
      # A group with no baseline row gets a missing limit and so a missing
      # screen. Say which groups, so a relabelled or new area is not read as
      # "nothing to report".
      unmatched <- is.na(wide$.islh_matched)
      if (any(unmatched)) {
        labels <- unique(do.call(
          paste,
          c(
            unname(as.list(wide[unmatched, by_names, drop = FALSE])),
            sep = " | "
          )
        ))
        .islh_inform(c(
          "{length(labels)} group{?s} {?has/have} no row in {.arg baseline},
           so {?its/their} {.field exceeds_reference} is missing: {.val {labels}}.",
          i = "{cli::qty(length(labels))}Check the group labels match, or add
               a reference for {?this group/these groups}."
        ))
      }
      wide$.islh_matched <- NULL
    }
  }

  if ("upper_limit" %in% names(wide)) {
    # Apply the explicitly selected descriptive screening rule.
    wide$exceeds_reference <- ifelse(
      is.na(wide$upper_limit),
      NA,
      if (comparison == "above") {
        wide$total > wide$upper_limit
      } else {
        wide$total >= wide$upper_limit
      }
    )
  }

  if (
    "exceeds_reference" %in% names(wide) && zero_reference == "positive_only"
  ) {
    zero <- !is.na(wide$upper_limit) & wide$upper_limit == 0
    wide$exceeds_reference[zero] <- wide$total[zero] > 0
  }
  attr(wide, "islh_screening") <- list(
    comparison = comparison,
    zero_reference = zero_reference,
    missing_periods = missing_periods
  )
  output_columns <- c(by_names, "total", "complete", reference_columns)
  if ("exceeds_reference" %in% names(wide)) {
    output_columns <- c(output_columns, "exceeds_reference")
  }
  output_columns <- unique(c(output_columns, date_columns))
  out <- wide[, output_columns, drop = FALSE]
  attr(out, "islh_screening") <- attr(wide, "islh_screening")

  # `total` spans the whole displayed window, so that is the duration recorded.
  .islh_surv_set_meta(
    out,
    interval = interval,
    week_start = week_start,
    periods = periods,
    from = min(target_periods),
    to = .islh_surv_period_end(max(target_periods), interval, week_start)
  )
}
