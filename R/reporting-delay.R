#' Summarize event-to-report delays
#'
#' `islh_reporting_delay()` describes how long records take to arrive: the
#' number of days from an event date, such as symptom onset, to the report
#' date. Use it to explain why counts by onset date fall away in the most
#' recent days, and to choose how long to wait before treating a period as
#' complete.
#'
#' @section Recent events bias the delays short:
#'
#' A record with a long delay only appears once it is reported. Near the end
#' of the data, only the quickly reported events have arrived, so including
#' them makes delays look shorter than they are. This is called right
#' truncation.
#'
#' Set `max_delay` to use only events at least `max_delay` days old, which
#' gives each of them time to show delays up to `max_delay` days. That
#' removes the worst of the bias, but not all of it: a record that takes
#' longer than the time since its event has still not arrived, and nothing in
#' the data says it exists. The summary describes delays among the records
#' that had arrived by `as_of`. If longer delays occur, the true delays are
#' longer than it shows. Choose `max_delay` from knowledge of the reporting
#' system, and check the summary again once the data have matured.
#'
#' Records with a missing date, or a report before the event, are left out
#' with a message. Find them with the `date_order` check in
#' [islh_check_events()].
#'
#' @param data A line list with one row per record.
#' @param onset Event date column.
#' @param report Report date column.
#' @param by Optional tidy-select specification of grouping columns.
#' @param probs Probabilities for the delay quantiles.
#' @param as_of Optional reporting cutoff. Records reported after it are left
#'   out, as they would not have been known on that date. Defaults to the
#'   latest report date.
#' @param max_delay Optional longest delay, in days, to be sure of observing.
#'   Events after `as_of - max_delay` are left out. See the right-truncation
#'   section.
#' @param timezone Reporting timezone for timestamps. Date values are
#'   unchanged.
#'
#' @return One row per group with `n`, `mean_delay`, one `delay_p` column per
#'   probability (for example `delay_p50` and `delay_p90`) and
#'   `longest_delay`, all in days.
#' @export
#'
#' @examples
#' islh_reporting_delay(
#'   islh_outbreak,
#'   onset = date_onset,
#'   report = date_reported,
#'   by = hsda
#' )
#'
#' # As the data stood on 1 February, using only onsets at least 14 days old.
#' islh_reporting_delay(
#'   islh_outbreak,
#'   onset = date_onset,
#'   report = date_reported,
#'   as_of = "2026-02-01",
#'   max_delay = 14
#' )
islh_reporting_delay <- function(
  data,
  onset,
  report,
  by = NULL,
  probs = c(0.5, 0.9),
  as_of = NULL,
  max_delay = NULL,
  timezone = "America/Vancouver"
) {
  if (
    !is.numeric(probs) ||
      length(probs) == 0L ||
      anyNA(probs) ||
      any(probs < 0 | probs > 1) ||
      anyDuplicated(probs)
  ) {
    .islh_abort("{.arg probs} must be distinct probabilities from 0 to 1.")
  }
  prepared <- .islh_delay_prepare(
    data,
    rlang::enquo(onset),
    rlang::enquo(report),
    rlang::enquo(by),
    as_of,
    max_delay,
    timezone
  )
  work <- prepared$work
  by_names <- prepared$by
  .islh_surv_check_reserved(
    by_names,
    c("n", "mean_delay", "longest_delay", paste0("delay_p", probs * 100))
  )
  if (nrow(work) == 0L) {
    .islh_abort("No records remain to summarize.")
  }

  summary <- work |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(by_names))) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_delay = mean(.data$.islh_delay),
      .quantiles = list(stats::quantile(
        .data$.islh_delay,
        probs = probs,
        names = FALSE,
        type = 7
      )),
      longest_delay = max(.data$.islh_delay),
      .groups = "drop"
    )
  quantiles <- do.call(rbind, summary$.quantiles)
  colnames(quantiles) <- paste0("delay_p", format(probs * 100, trim = TRUE))
  out <- cbind(
    as.data.frame(summary[c(by_names, "n", "mean_delay")]),
    as.data.frame(quantiles),
    longest_delay = summary$longest_delay
  )
  out
}

#' Expected completeness of recent counts
#'
#' `islh_reporting_completeness()` labels recent event dates with how complete
#' their counts are likely to be. It learns an empirical delay profile, the
#' share of records that arrive within each number of days, from older events
#' that you judge to be fully reported, and applies it to the event dates in
#' the last `max_delay` days before `as_of`.
#'
#' Read `expected_complete` as: if recent events report like the older ones,
#' about this share of their eventual records had arrived by `as_of`. A value
#' of 0.4 means the count is likely to more than double. Use it to label recent
#' periods as incomplete, not as a forecast of the final count.
#'
#' @section What the estimate rests on:
#'
#' Two assumptions, and the result is only as good as either.
#'
#' **Maturity.** An event at least `maturity` days before `as_of` has all its
#' records in. Delays are learned only from those events. The data cannot
#' check this: a record that has not arrived leaves no trace. If some records
#' take longer than `maturity` days, the profile misses them and
#' `expected_complete` is too high. Choose `maturity` from what you know of the
#' reporting system, not from this data. When older events in the data do show
#' delays longer than `maturity`, a warning says so, because the assumption is
#' then visibly wrong.
#'
#' **Stability.** Recent events report like the older ones learned from. A
#' holiday, a new data feed or a surge that slows reporting breaks this.
#' `learn_days` limits learning to the most recent mature events, which
#' follows a changing system more closely but uses fewer records.
#'
#' With `by`, each group learns from its own delays, which is unstable for
#' small groups. `delay_records` gives the number of records each estimate
#' rests on; set `pool = TRUE` to learn one profile from all groups.
#'
#' `expected_complete` is an empirical estimate from a limited number of
#' records, not a known fraction, and it carries the uncertainty of the
#' delays it was learned from. This is not a nowcast. It does not model
#' trends in the delays or in the epidemic curve, and it gives no interval.
#' When the final count itself matters, use a nowcasting method that adjusts
#' for right truncation (Charniga et al. 2024).
#'
#' @inheritParams islh_reporting_delay
#' @param as_of Reporting cutoff: the date the data stood at. Required.
#' @param max_delay Number of recent days to assess: event dates from
#'   `as_of - max_delay + 1` to `as_of`. Required.
#' @param maturity Days after an event by which you judge all its records to
#'   have arrived. Delays are learned only from events at least this old. Must
#'   be at least `max_delay`. Required: see the section on assumptions.
#' @param learn_days Optional number of event days to learn from, counting
#'   back from `as_of - maturity`. `NULL` learns from every mature event.
#' @param pool Learn one delay pattern from all groups rather than one per
#'   group.
#'
#' @return One row per group and event date in the last `max_delay` days up to
#'   `as_of`: the grouping columns, `onset_date`, `days_since_onset`,
#'   `reported` (records for that date reported by `as_of`),
#'   `expected_complete`, `delay_records` (records in the learning window), and
#'   `learn_from` and `learn_to` (the event dates delays were learned from).
#' @export
#'
#' @references
#' Charniga K, Park SW, Akhmetzhanov AR, et al. (2024). Best practices for
#' estimating and reporting epidemiological delay distributions of
#' infectious diseases. *PLOS Computational Biology* 20(10):e1012520.
#' \doi{10.1371/journal.pcbi.1012520}
#'
#' @examples
#' # How complete were the counts by onset date on 20 January? Records are
#' # judged complete 21 days after onset.
#' completeness <- islh_reporting_completeness(
#'   islh_outbreak,
#'   onset = date_onset,
#'   report = date_reported,
#'   as_of = "2026-01-20",
#'   max_delay = 10,
#'   maturity = 21
#' )
#' completeness |>
#'   dplyr::slice_tail(n = 6)
islh_reporting_completeness <- function(
  data,
  onset,
  report,
  as_of,
  max_delay,
  maturity,
  by = NULL,
  pool = FALSE,
  learn_days = NULL,
  timezone = "America/Vancouver"
) {
  if (missing(as_of) || missing(max_delay) || missing(maturity)) {
    .islh_abort(c(
      "Supply {.arg as_of}, {.arg max_delay} and {.arg maturity}.",
      i = "Completeness depends on the date the data stood at, the days
           assessed, and how long you judge records to take to arrive."
    ))
  }
  pool <- .islh_check_flag(pool, "pool")
  onset_quo <- rlang::enquo(onset)
  report_quo <- rlang::enquo(report)
  by_quo <- rlang::enquo(by)

  # Everything known on `as_of`, before the learning/assessment split.
  known <- .islh_delay_prepare(
    data,
    onset_quo,
    report_quo,
    by_quo,
    as_of,
    NULL,
    timezone
  )
  by_names <- known$by
  .islh_surv_check_reserved(
    by_names,
    c(
      "onset_date",
      "days_since_onset",
      "reported",
      "expected_complete",
      "delay_records",
      "learn_from",
      "learn_to"
    )
  )
  max_delay <- .islh_check_max_delay(max_delay)
  maturity <- .islh_check_max_delay(maturity, arg = "maturity")
  if (maturity < max_delay) {
    .islh_abort(c(
      "{.arg maturity} must be at least {.arg max_delay}.",
      x = "Records still arriving after {maturity} day{?s} would be counted as
           complete for some of the {max_delay} days assessed."
    ))
  }
  if (!is.null(learn_days)) {
    learn_days <- .islh_check_max_delay(learn_days, arg = "learn_days")
  }
  as_of <- known$as_of
  work <- known$work

  learn_to <- as_of - maturity
  learn_from <- if (is.null(learn_days)) {
    suppressWarnings(min(work$.islh_onset[work$.islh_onset <= learn_to]))
  } else {
    learn_to - learn_days + 1L
  }
  learn <- work[
    work$.islh_onset <= learn_to & work$.islh_onset >= learn_from,
    ,
    drop = FALSE
  ]
  if (nrow(learn) == 0L) {
    .islh_abort(c(
      "No events are old enough to learn delays from.",
      i = "Every event is within {maturity} days of {.arg as_of}. Use a later
           {.arg as_of}, more history, or a shorter {.arg maturity} if the
           reporting system supports it."
    ))
  }
  if (is.infinite(learn_from)) {
    learn_from <- min(learn$.islh_onset)
  }

  beyond <- sum(learn$.islh_delay > maturity)
  if (beyond > 0L) {
    .islh_warn(
      c(
        "{beyond} learned record{?s} took longer than {.arg maturity}
         ({maturity} day{?s}) to arrive.",
        x = "Records still arrive after {maturity} day{?s}, so the assumption
             behind {.field expected_complete} does not hold, and the values
             are too high.",
        i = "Use a longer {.arg maturity}."
      ),
      class = "islh_warning_maturity"
    )
  }

  groups <- if (length(by_names) > 0L) {
    unique(work[by_names])
  } else {
    data.frame(.islh_all = 1L)
  }
  ids <- .islh_key_ids(list(work, learn, groups), by_names)
  work_key <- ids[[1]]
  learn_key <- if (pool) rep(1L, nrow(learn)) else ids[[2]]
  group_key <- ids[[3]]

  days <- 0:(max_delay - 1L)
  onset_dates <- as_of - days
  rows <- lapply(seq_len(nrow(groups)), function(g) {
    delays <- learn$.islh_delay[learn_key == if (pool) 1L else group_key[g]]
    share <- vapply(days, function(d) mean(delays <= d), numeric(1))
    if (length(delays) == 0L) {
      share <- rep(NA_real_, length(days))
    }
    in_group <- work_key == group_key[g]
    reported <- vapply(
      onset_dates,
      function(day) sum(in_group & work$.islh_onset == day),
      numeric(1)
    )
    piece <- data.frame(
      onset_date = onset_dates,
      days_since_onset = days,
      reported = reported,
      expected_complete = share,
      delay_records = length(delays),
      learn_from = learn_from,
      learn_to = learn_to
    )
    if (length(by_names) > 0L) {
      piece <- cbind(
        groups[rep(g, length(days)), by_names, drop = FALSE],
        piece
      )
    }
    piece
  })
  out <- do.call(rbind, rows)
  order_cols <- c(by_names, "onset_date")
  out <- out[do.call(order, unname(as.list(out[order_cols]))), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Shared by both delay functions: select columns, read dates, apply the
# cutoff, and drop unusable records with a message saying how many.
.islh_delay_prepare <- function(
  data,
  onset_quo,
  report_quo,
  by_quo,
  as_of,
  max_delay,
  timezone,
  call = rlang::caller_env()
) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.", call = call)
  }
  onset_name <- .islh_surv_select(data, onset_quo, "onset", 1L, 1L, call = call)
  report_name <- .islh_surv_select(
    data,
    report_quo,
    "report",
    1L,
    1L,
    call = call
  )
  by_names <- .islh_surv_select(data, by_quo, "by", call = call)

  onset <- .islh_surv_date_info(data[[onset_name]], timezone, call = call)
  report <- .islh_surv_date_info(data[[report_name]], timezone, call = call)
  work <- as.data.frame(data)
  work$.islh_onset <- onset$value
  work$.islh_report <- report$value
  unusable <- onset$missing | onset$invalid | report$missing | report$invalid
  negative <- !unusable & work$.islh_report < work$.islh_onset

  if (is.null(as_of)) {
    usable_reports <- work$.islh_report[!unusable]
    if (length(usable_reports) == 0L) {
      .islh_abort("No records have usable dates.", call = call)
    }
    as_of <- max(usable_reports)
  } else {
    as_of <- .islh_surv_as_date(
      as_of,
      "as_of",
      scalar = TRUE,
      timezone = timezone,
      call = call
    )
  }
  late <- !unusable & !negative & work$.islh_report > as_of

  too_recent <- rep(FALSE, nrow(work))
  if (!is.null(max_delay)) {
    max_delay <- .islh_check_max_delay(max_delay, call = call)
    too_recent <- !unusable &
      !negative &
      !late &
      work$.islh_onset > as_of - max_delay
  }

  notes <- c(
    if (any(unusable)) {
      "{sum(unusable)} record{?s} with a missing or invalid date."
    },
    if (any(negative)) {
      "{sum(negative)} record{?s} reported before the event date."
    },
    if (any(late)) {
      "{sum(late)} record{?s} reported after {.arg as_of} ({format(as_of)})."
    },
    if (any(too_recent)) {
      "{sum(too_recent)} record{?s} with an event in the last {max_delay}
       day{?s}, too recent for every delay to be seen."
    }
  )
  if (length(notes) > 0L) {
    names(notes) <- rep("*", length(notes))
    .islh_inform(c("Left out:", notes))
  }

  keep <- !unusable & !negative & !late & !too_recent
  work <- work[keep, , drop = FALSE]
  work$.islh_delay <- as.numeric(work$.islh_report - work$.islh_onset)
  list(work = work, by = by_names, as_of = as_of)
}

.islh_check_max_delay <- function(
  max_delay,
  arg = "max_delay",
  call = rlang::caller_env()
) {
  if (
    !is.numeric(max_delay) ||
      length(max_delay) != 1L ||
      is.na(max_delay) ||
      max_delay != round(max_delay) ||
      max_delay < 1
  ) {
    .islh_abort(
      "{.arg {arg}} must be one whole number of days, at least 1.",
      call = call
    )
  }
  as.integer(max_delay)
}
