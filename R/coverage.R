#' Check which expected feeds were received
#'
#' `islh_check_coverage()` compares a log of received submissions with the
#' groups that should have reported, period by period. It separates two things
#' that look identical in a count table: a site that reported no events, and a
#' site that did not report at all.
#'
#' Pass a log with one row per received submission, such as a file, a daily
#' extract or a site's "nil report". Do not pass the event line list: a site
#' that reported zero events has no rows there, so every quiet day would look
#' like a missing feed.
#'
#' @section Using the result:
#'
#' A period with `received = FALSE` is unknown, not zero. Pass the result to
#' [islh_surveillance_snapshot()] as `coverage`. Every expected group then
#' appears in the snapshot, even one that sent nothing, and each group with an
#' unreceived period shows a missing `total` and `complete = FALSE`, as does
#' the `All` row.
#'
#' Do not drop unreceived periods with a join and pass the counts on alone. A
#' site that sent nothing loses every row that way, so it vanishes from the
#' snapshot and the `All` row looks complete.
#'
#' Rows from groups that are not in `expected` are kept, with
#' `expected = FALSE`, so a new site or a mistyped site code is visible rather
#' than dropped.
#'
#' @param received A data frame with one row per received submission.
#' @param date Column holding the date each submission covers.
#' @param by Optional tidy-select specification of the columns identifying
#'   who reports, such as a site or a data source.
#' @param expected The groups that should report. A vector for one `by` column,
#'   or a data frame of combinations for several. Required when `by` is used:
#'   coverage is judged against the groups that should report, not the ones
#'   that did.
#' @param from,to First and last dates of the coverage window. They must fall
#'   on period boundaries of `interval`.
#' @param interval Reporting period of one submission: `"day"`, `"week"`,
#'   `"isoweek"`, `"epiweek"`, `"month"`, `"quarter"` or `"year"`.
#' @param week_start Start of an ordinary week, from 1 (Monday) to 7 (Sunday).
#' @param timezone Reporting timezone for timestamps. Date values are
#'   unchanged.
#'
#' @return A data frame with the grouping columns, `period_start`,
#'   `period_end`, `records` (the number of submissions), `received` and
#'   `expected`. There is one row for every expected group and period, plus
#'   any periods in which an unexpected group reported. A message summarizes
#'   the gaps.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Each site sends one file a day, including days with no cases. Site C
#' # sent nothing on 3 and 4 August.
#' log <- tibble(
#'   site = c(rep(c("A", "B"), each = 7), rep("C", 5)),
#'   date = as.Date("2026-08-01") + c(0:6, 0:6, 0, 1, 4, 5, 6)
#' )
#'
#' coverage <- islh_check_coverage(
#'   log,
#'   date = date,
#'   by = site,
#'   expected = c("A", "B", "C"),
#'   from = "2026-08-01",
#'   to = "2026-08-07"
#' )
#' coverage |>
#'   filter(!received)
#'
#' # Daily counts from the events. Filling zeros makes 3 and 4 August look
#' # like quiet days for site C.
#' events <- tibble(
#'   site = c("A", "A", "B", "C", "C"),
#'   date = as.Date(c(
#'     "2026-08-02", "2026-08-05", "2026-08-03", "2026-08-01", "2026-08-06"
#'   ))
#' )
#' daily <- islh_count_events(
#'   events,
#'   date = date,
#'   by = site,
#'   from = "2026-08-01",
#'   to = "2026-08-07",
#'   groups = c("A", "B", "C")
#' )
#'
#' # Give the snapshot the coverage, so the gap is shown as unknown.
#' islh_surveillance_snapshot(
#'   daily,
#'   date = period_start,
#'   value = count,
#'   by = site,
#'   periods = 7,
#'   interval = "day",
#'   include_total = TRUE,
#'   coverage = coverage
#' )
islh_check_coverage <- function(
  received,
  date,
  by = NULL,
  expected = NULL,
  from,
  to,
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
  timezone = "America/Vancouver"
) {
  if (!is.data.frame(received)) {
    .islh_abort("{.arg received} must be a data frame.")
  }
  interval <- .islh_surv_interval(interval)
  week_start <- .islh_surv_week_start(interval, week_start)
  if (missing(from) || missing(to)) {
    .islh_abort(c(
      "Supply both {.arg from} and {.arg to}.",
      i = "Coverage is judged over a window you choose, not over the dates
           that happen to appear in the log."
    ))
  }
  from <- .islh_surv_as_date(from, "from", scalar = TRUE, timezone = timezone)
  to <- .islh_surv_as_date(to, "to", scalar = TRUE, timezone = timezone)
  if (from > to) {
    .islh_abort("{.arg from} must not be after {.arg to}.")
  }
  first <- .islh_surv_period_start(from, interval, week_start)
  last <- .islh_surv_period_start(to, interval, week_start)
  if (
    first != from || .islh_surv_period_end(last, interval, week_start) != to
  ) {
    .islh_abort(c(
      "{.arg from} and {.arg to} must fall on {interval} boundaries.",
      i = "The first {interval} in the window starts on {format(first)} and
           the last ends on
           {format(.islh_surv_period_end(last, interval, week_start))}."
    ))
  }

  date_name <- .islh_surv_select(
    received,
    rlang::enquo(date),
    "date",
    1L,
    1L
  )
  by_names <- .islh_surv_select(received, rlang::enquo(by), "by")
  .islh_surv_check_reserved(
    by_names,
    c("period_start", "period_end", "records", "received", "expected")
  )
  if (length(by_names) > 0L && is.null(expected)) {
    .islh_abort(c(
      "Supply {.arg expected}.",
      i = "Coverage is judged against the groups that should report. A
           group that never reported does not appear in {.arg received}."
    ))
  }
  expected_groups <- .islh_surv_groups(expected, by_names, received)
  if (length(by_names) > 0L && nrow(expected_groups) == 0L) {
    .islh_abort("{.arg expected} must name at least one group.")
  }

  work <- as.data.frame(received)
  work$.islh_date <- .islh_surv_as_date(
    work[[date_name]],
    date_name,
    timezone = timezone
  )
  work <- work[work$.islh_date >= from & work$.islh_date <= to, , drop = FALSE]
  work$period_start <- .islh_surv_period_start(
    work$.islh_date,
    interval,
    week_start
  )

  periods <- .islh_surv_period_sequence(first, last, interval)
  keys <- c(by_names, "period_start")
  for (field in by_names) {
    # Compare labels as text, so a factor log matches a character roster.
    if (is.factor(work[[field]])) {
      work[[field]] <- as.character(work[[field]])
    }
    if (is.factor(expected_groups[[field]])) {
      expected_groups[[field]] <- as.character(expected_groups[[field]])
    }
  }

  tally <- work |>
    dplyr::count(dplyr::across(tidyselect::all_of(keys)), name = "records")

  grid <- .islh_surv_grid(expected_groups, periods, by_names)
  out <- dplyr::left_join(
    grid,
    tally,
    by = keys,
    relationship = "one-to-one",
    na_matches = "na"
  )
  out$records[is.na(out$records)] <- 0L
  out$expected <- rep(TRUE, nrow(out))

  unexpected <- dplyr::anti_join(tally, grid, by = keys, na_matches = "na")
  if (nrow(unexpected) > 0L) {
    unexpected$expected <- FALSE
    out <- dplyr::bind_rows(out, unexpected)
  }

  out$period_end <- .islh_surv_period_end(
    out$period_start,
    interval,
    week_start
  )
  out$received <- out$records > 0L
  out$records <- as.integer(out$records)
  out <- as.data.frame(out)
  out <- out[do.call(order, out[keys]), , drop = FALSE]
  out <- out[,
    c(
      by_names,
      "period_start",
      "period_end",
      "records",
      "received",
      "expected"
    ),
    drop = FALSE
  ]
  rownames(out) <- NULL

  gaps <- sum(out$expected & !out$received)
  n_expected <- sum(out$expected)
  if (gaps > 0L || nrow(unexpected) > 0L) {
    unexpected_labels <- if (length(by_names) > 0L) {
      unique(do.call(
        paste,
        c(unname(as.list(unexpected[by_names])), list(sep = " | "))
      ))
    } else {
      character()
    }
    .islh_inform(c(
      "{gaps} of {n_expected} expected {cli::qty(gaps)}submission{?s}
       {?is/are} missing.",
      "!" = if (length(unexpected_labels) > 0L) {
        "{length(unexpected_labels)} group{?s} reported without being
         expected: {.val {unexpected_labels}}."
      },
      i = if (gaps > 0L) {
        "Treat those periods as unknown, not zero."
      }
    ))
  }

  .islh_surv_set_meta(
    out,
    interval = interval,
    week_start = week_start,
    periods = 1L,
    from = from,
    to = to
  )
}
