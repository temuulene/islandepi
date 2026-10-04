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
#' @param end Last period to show. Defaults to the last period of `coverage`
#'   when it is supplied, and otherwise to the latest date in `data`. Fix it
#'   in report code: the latest date in `data` moves backwards when the most
#'   recent feeds are missing, and the window moves with it.
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
#' @param source_interval What one row of `data` covers, such as `"day"` or
#'   `"week"`, when `data` does not record it. Counts from
#'   [islh_count_events()] record it, so this is for tables built another
#'   way. See the source-period section.
#' @param coverage Optional result from [islh_check_coverage()] for the feeds
#'   behind `data`. See the feed-coverage section.
#'
#' @return A wide data frame containing groups, `total`, `complete`, optional
#'   baseline fields, `exceeds_reference`, and one column per displayed period.
#'   `complete` is `TRUE` when every displayed period has a count. With
#'   `missing_periods = "missing"`, a group with a gap has `complete = FALSE`
#'   and a missing `total`, so it is never compared with the baseline as if
#'   the gap were zero.
#'
#' @section Source periods:
#'
#' A row's date says when its period starts, not how long the period is. Two
#' counts dated a week apart could be two weekly totals, or two daily counts
#' with the six days between them missing, and the totals differ. The
#' snapshot therefore needs to know what one row covers:
#'
#' * counts from [islh_count_events()] and [islh_check_coverage()] record it;
#' * `source_interval` names it for any other table;
#' * a table of consecutive days is read as daily counts, the one case its
#'   dates settle.
#'
#' Otherwise the snapshot stops and asks for `source_interval`, rather than
#' guess.
#'
#' @section Feed coverage:
#'
#' A site that sent nothing has no rows in an event line list, so it has no
#' rows in the counts either, and a snapshot built from those counts leaves
#' it out. Worse, the `All` row then looks complete. A site that sent some
#' days but not others shows filled zeros for the missing days, which look
#' like quiet days.
#'
#' Pass the result of [islh_check_coverage()] as `coverage` to prevent both.
#' Every expected group then appears in the result, and a period whose feed
#' was not received is unknown: its count is `NA` whatever `data` holds for
#' it, the group's `total` is `NA` with `complete = FALSE`, and so is the
#' `All` row. A message says how many periods were unknown. Coverage must
#' span the whole window for every expected group, and every group in `data`
#' must appear in it.
#'
#' Periods that were received but have no row in `data` are still handled by
#' `missing_periods`.
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
#'   end = "2026-08-07",
#'   periods = 7,
#'   include_total = TRUE
#' )
#'
#' # Site C is expected but sent nothing, and site B missed 6 August. With
#' # the coverage check, C appears as unknown rather than vanishing, and the
#' # All row is not reported as complete.
#' log <- data.frame(
#'   site = c(rep("A", 7), rep("B", 6)),
#'   date = as.Date("2026-08-01") + c(0:6, 0:4, 6)
#' )
#' coverage <- islh_check_coverage(
#'   log,
#'   date = date,
#'   by = site,
#'   expected = c("A", "B", "C"),
#'   from = "2026-08-01",
#'   to = "2026-08-07"
#' )
#' islh_surveillance_snapshot(
#'   daily,
#'   date = date,
#'   value = count,
#'   by = site,
#'   periods = 7,
#'   include_total = TRUE,
#'   coverage = coverage
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
  zero_reference = c("positive_only", "compare"),
  source_interval = NULL,
  coverage = NULL
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
  coverage_meta <- NULL
  if (!is.null(coverage)) {
    coverage_meta <- .islh_surv_check_coverage_arg(coverage, by_names)
  }
  if (nrow(work) == 0L && is.null(coverage)) {
    .islh_abort(c(
      "{.arg data} must contain at least one row.",
      i = "When no feed arrived at all, pass {.arg coverage} to report every
           group as unknown."
    ))
  }

  if (is.null(end)) {
    end <- if (!is.null(coverage_meta)) {
      coverage_meta$to
    } else {
      max(work$.islh_date)
    }
  } else {
    end <- .islh_surv_as_date(end, "end", scalar = TRUE, timezone = timezone)
  }
  end <- .islh_surv_period_start(end, interval, week_start)
  target_periods <- .islh_surv_periods_back(end, periods, interval)
  .islh_surv_check_reserved(by_names, format(target_periods, "%Y-%m-%d"))
  data_meta <- .islh_surv_snapshot_source(
    data_meta,
    source_interval,
    coverage_meta,
    work$.islh_date,
    week_start
  )
  if (!is.null(coverage_meta)) {
    .islh_surv_resolve_interval(
      coverage_meta$interval,
      data_meta,
      work$.islh_date,
      arg = "coverage"
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
  roster <- NULL
  if (!is.null(coverage)) {
    covered <- .islh_surv_apply_coverage(
      work,
      coverage,
      by_names,
      data_meta,
      min(target_periods),
      window_end
    )
    work <- covered$work
    roster <- covered$roster
  }
  work <- .islh_surv_complete_source(
    work,
    by_names,
    data_meta,
    min(target_periods),
    window_end,
    missing_periods,
    groups = roster
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

# What one row of a snapshot's `data` covers: the metadata it carries, then
# `source_interval`, then the coverage check's period, then consecutive days.
.islh_surv_snapshot_source <- function(
  meta,
  source_interval,
  coverage_meta,
  dates,
  week_start,
  call = rlang::caller_env()
) {
  if (!is.null(meta)) {
    if (!is.null(source_interval)) {
      .islh_surv_resolve_interval(
        source_interval,
        meta,
        dates,
        arg = "source_interval",
        call = call
      )
    }
    return(meta)
  }
  declared <- if (!is.null(source_interval)) {
    .islh_surv_interval(source_interval, arg = "source_interval", call = call)
  } else if (!is.null(coverage_meta)) {
    coverage_meta$interval
  } else {
    .islh_surv_infer_interval(dates)
  }
  if (is.null(declared)) {
    .islh_abort(
      c(
        "{.arg data} does not record what one row covers.",
        x = "Its dates are not consecutive days, so a row could be a day, a
             week or longer, and the totals would differ.",
        i = "Name it with {.arg source_interval}, or build the counts with
             {.fn islh_count_events}, which records it."
      ),
      call = call
    )
  }
  anchor <- if (!is.null(coverage_meta) && !is.null(coverage_meta$week_start)) {
    coverage_meta$week_start
  } else if (declared %in% c("week", "isoweek", "epiweek") && length(dates)) {
    as.integer(lubridate::wday(min(dates), week_start = 1))
  } else {
    week_start
  }
  list(interval = declared, periods = 1L, week_start = anchor)
}

.islh_surv_check_coverage_arg <- function(
  coverage,
  by,
  call = rlang::caller_env()
) {
  meta <- .islh_surv_meta(coverage)
  needed <- c(by, "period_start", "received", "expected")
  if (
    !is.data.frame(coverage) ||
      is.null(meta) ||
      !all(needed %in% names(coverage))
  ) {
    .islh_abort(
      c(
        "{.arg coverage} must be a result of {.fn islh_check_coverage}.",
        i = "It needs the grouping columns and {.field period_start},
             {.field received} and {.field expected}."
      ),
      call = call
    )
  }
  if (!is.logical(coverage$received) || anyNA(coverage$received)) {
    .islh_abort(
      "{.field received} in {.arg coverage} must be TRUE or FALSE.",
      call = call
    )
  }
  if (is.null(meta$to) || !inherits(meta$to, "Date")) {
    .islh_abort(
      "{.arg coverage} does not record its window; rebuild it with
       {.fn islh_check_coverage}.",
      call = call
    )
  }
  meta
}

# Mark every period whose feed was not received as unknown, and make sure
# every expected group is present. Returns the source rows and the roster of
# groups the snapshot must show.
.islh_surv_apply_coverage <- function(
  work,
  coverage,
  by,
  meta,
  from,
  to,
  call = rlang::caller_env()
) {
  coverage <- as.data.frame(coverage)
  for (field in by) {
    # Coverage compares labels as text, so the counts do too.
    if (is.factor(work[[field]])) {
      work[[field]] <- as.character(work[[field]])
    }
  }
  names(coverage)[names(coverage) == "period_start"] <- ".islh_date"
  keys <- c(by, ".islh_date")
  coverage <- coverage[
    coverage$.islh_date >= from & coverage$.islh_date <= to,
    ,
    drop = FALSE
  ]

  expected <- coverage[coverage$expected, by, drop = FALSE]
  roster <- if (length(by) > 0L) {
    unique(dplyr::bind_rows(expected, unique(work[by])))
  } else {
    NULL
  }

  # Coverage has to answer for every group and period the snapshot shows.
  anchor <- .islh_surv_week_start(meta$interval, meta$week_start %||% 1L)
  periods <- .islh_surv_period_sequence(
    .islh_surv_period_start(from, meta$interval, anchor),
    .islh_surv_period_start(to, meta$interval, anchor),
    meta$interval
  )
  if (length(by) > 0L) {
    unknown_groups <- roster[
      is.na(.islh_key_match(roster, coverage, by)),
      ,
      drop = FALSE
    ]
    if (nrow(unknown_groups) > 0L) {
      labels <- .islh_key_labels(unknown_groups, by)
      .islh_abort(
        c(
          "{.arg data} has groups that {.arg coverage} does not list.",
          x = "Not in coverage: {.val {labels}}.",
          i = "Coverage cannot say whether their feeds arrived. Add them to
               {.arg expected} in {.fn islh_check_coverage}."
        ),
        call = call
      )
    }
  }
  grid <- .islh_surv_grid(
    if (length(by) > 0L) unique(expected) else NULL,
    periods,
    by
  )
  names(grid)[names(grid) == "period_start"] <- ".islh_date"
  uncovered <- is.na(.islh_key_match(grid, coverage, keys))
  if (any(uncovered)) {
    .islh_abort(
      c(
        "{.arg coverage} does not span the snapshot window.",
        x = "{sum(uncovered)} expected group-period{?s} {?has/have} no
             coverage row.",
        i = "Check coverage over the same window as the snapshot."
      ),
      call = call
    )
  }

  missed <- coverage[!coverage$received, keys, drop = FALSE]
  if (nrow(missed) > 0L) {
    at <- .islh_key_match(work, missed, keys)
    work$.islh_value[!is.na(at)] <- NA_real_
    absent <- missed[
      is.na(.islh_key_match(missed, work, keys)),
      ,
      drop = FALSE
    ]
    if (nrow(absent) > 0L) {
      absent$.islh_value <- rep(NA_real_, nrow(absent))
      if ("partial_period" %in% names(work)) {
        absent$partial_period <- rep(FALSE, nrow(absent))
      }
      work <- dplyr::bind_rows(work, absent)
    }
    .islh_inform(c(
      "{nrow(missed)} group-period{?s} in the window had no feed received,
       so {?its/their} count{?s} {?is/are} unknown.",
      i = "Their groups show a missing {.field total} and
           {.code complete = FALSE}."
    ))
  }
  list(work = work, roster = roster)
}
