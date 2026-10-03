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
#' truncation. Set `max_delay` to use only events old enough for every delay
#' up to `max_delay` days to have been seen. Quantiles up to `max_delay` are
#' then unbiased.
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
#' `islh_reporting_completeness()` estimates, for each recent event date, what
#' share of its eventual records had been reported by `as_of`. It uses the
#' delays seen in older events, where every delay up to `max_delay` days has
#' had time to appear.
#'
#' Read `expected_complete` as: of all the records that will eventually be
#' reported for that date, about this share had arrived. A value of 0.4 means
#' the count is likely to more than double. It is a guide for labelling recent
#' periods as incomplete, not a forecast.
#'
#' @section Assumptions:
#'
#' The estimate assumes recent reporting delays look like those of the older
#' events it learned from. A holiday, a new data feed or a surge that slows
#' reporting breaks that. Delays longer than `max_delay` count as not yet
#' reported, so `expected_complete` never reaches 1 when such delays occur.
#'
#' With `by`, each group learns from its own delays, which is unstable for
#' small groups. `delay_records` gives the number of records each estimate
#' rests on; set `pool = TRUE` to learn one delay pattern from all groups.
#'
#' @inheritParams islh_reporting_delay
#' @param as_of Reporting cutoff: the date the data stood at. Required.
#' @param max_delay Longest delay, in days, to learn. Required. Events from
#'   the last `max_delay` days are the ones assessed; older events supply the
#'   delays.
#' @param pool Learn one delay pattern from all groups rather than one per
#'   group.
#'
#' @return One row per group and event date in the last `max_delay` days up to
#'   `as_of`: the grouping columns, `onset_date`, `days_since_onset`,
#'   `reported` (records for that date reported by `as_of`),
#'   `expected_complete` and `delay_records`.
#' @export
#'
#' @examples
#' # How complete were the counts by onset date on 20 January?
#' completeness <- islh_reporting_completeness(
#'   islh_outbreak,
#'   onset = date_onset,
#'   report = date_reported,
#'   as_of = "2026-01-20",
#'   max_delay = 10
#' )
#' tail(completeness)
islh_reporting_completeness <- function(
  data,
  onset,
  report,
  as_of,
  max_delay,
  by = NULL,
  pool = FALSE,
  timezone = "America/Vancouver"
) {
  if (missing(as_of) || missing(max_delay)) {
    .islh_abort(c(
      "Supply both {.arg as_of} and {.arg max_delay}.",
      i = "Completeness depends on the date the data stood at and on how
           long a delay to learn."
    ))
  }
  pool <- .islh_check_flag(pool, "pool")
  onset_quo <- rlang::enquo(onset)
  report_quo <- rlang::enquo(report)
  by_quo <- rlang::enquo(by)

  # Everything known on `as_of`, before the old/recent split.
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
      "delay_records"
    )
  )
  max_delay <- .islh_check_max_delay(max_delay)
  as_of <- known$as_of
  work <- known$work
  learn_until <- as_of - max_delay
  learn <- work[work$.islh_onset <= learn_until, , drop = FALSE]
  if (nrow(learn) == 0L) {
    .islh_abort(c(
      "No events are old enough to learn delays from.",
      i = "Every event is within {max_delay} days of {.arg as_of}. Use a
           shorter {.arg max_delay} or a later {.arg as_of}."
    ))
  }

  groups <- if (length(by_names) > 0L) {
    unique(work[by_names])
  } else {
    data.frame(.islh_all = 1L)
  }
  key <- function(x) {
    if (length(by_names) == 0L) {
      return(rep("", nrow(x)))
    }
    do.call(paste, c(unname(lapply(x[by_names], as.character)), sep = "\r"))
  }
  learn_key <- if (pool) rep("", nrow(learn)) else key(learn)
  group_key <- key(groups)

  days <- 0:(max_delay - 1L)
  onset_dates <- as_of - days
  rows <- lapply(seq_len(nrow(groups)), function(g) {
    delays <- learn$.islh_delay[learn_key == if (pool) "" else group_key[g]]
    share <- vapply(days, function(d) mean(delays <= d), numeric(1))
    if (length(delays) == 0L) {
      share <- rep(NA_real_, length(days))
    }
    in_group <- key(work) == group_key[g]
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
      delay_records = length(delays)
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

.islh_check_max_delay <- function(max_delay, call = rlang::caller_env()) {
  if (
    !is.numeric(max_delay) ||
      length(max_delay) != 1L ||
      is.na(max_delay) ||
      max_delay != round(max_delay) ||
      max_delay < 1
  ) {
    .islh_abort(
      "{.arg max_delay} must be one whole number of days, at least 1.",
      call = call
    )
  }
  as.integer(max_delay)
}
