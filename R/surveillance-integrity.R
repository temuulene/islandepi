# Internal calendar and coverage checks; metadata never replaces row validation.
`%||%` <- function(x, y) if (is.null(x)) y else x

# Grouping columns must not share a name with a column the function creates.
# The generated column would overwrite the group labels, so a table grouped by
# a column called `total` would show the calculated totals where the group
# names should be. Names starting `.islh_` are used for working columns.
.islh_surv_check_reserved <- function(
  by,
  reserved,
  call = rlang::caller_env()
) {
  clash <- by[by %in% reserved | startsWith(by, ".islh_")]
  if (length(clash) > 0L) {
    .islh_abort(
      c(
        "Grouping columns use names reserved by the result.",
        x = "Reserved: {.val {clash}}.",
        i = "Rename {cli::qty(length(clash))}{?this column/these columns}
             before calling this function."
      ),
      call = call
    )
  }
  invisible(by)
}

# How many identifiers appear in more than one period, and in more than one
# group? Distinct-ID counts only add up to distinct people when both are zero.
.islh_surv_id_repeats <- function(work, id, by) {
  ids <- work[[id]]
  repeated <- function(key) {
    pairs <- unique(data.frame(id = ids, key = key, stringsAsFactors = FALSE))
    length(unique(pairs$id[duplicated(pairs$id)]))
  }
  groups <- if (length(by) > 0L) {
    repeated(.islh_key_ids(list(work), by)[[1]])
  } else {
    0L
  }
  list(
    periods = repeated(as.character(work$.islh_period_start)),
    groups = groups,
    by = by
  )
}

# Summing distinct-ID counts across periods or groups counts a person once for
# every period or group they appear in. That is a count of person-periods, not
# of people, and nothing in the numbers says so. Warn when the metadata shows
# it happened.
.islh_surv_check_id_sum <- function(
  meta,
  by,
  merges_periods,
  total = FALSE
) {
  repeats <- meta$id_repeats
  if (!is.list(repeats) || is.null(repeats$periods)) {
    return(invisible(NULL))
  }
  merges_groups <- isTRUE(total) || length(setdiff(repeats$by, by)) > 0L
  n_periods <- if (isTRUE(merges_periods)) repeats$periods else 0L
  n_groups <- if (merges_groups) repeats$groups else 0L
  if (n_periods == 0L && n_groups == 0L) {
    return(invisible(NULL))
  }
  .islh_warn(
    c(
      "This sums distinct-ID counts, so some people may be counted more
       than once.",
      "*" = if (n_periods > 0L) {
        "{n_periods} identifier{?s} appear{?s/} in more than one period of
         the source counts."
      },
      "*" = if (n_groups > 0L) {
        "{n_groups} identifier{?s} appear{?s/} in more than one group of the
         source counts."
      },
      i = "To count people, recount the line list with {.arg id} at the
           period and grouping you report. If repeat events should count
           separately, this warning can be ignored."
    ),
    class = "islh_warning_id_sum"
  )
  invisible(NULL)
}

.islh_surv_timezone <- function(timezone, call = rlang::caller_env()) {
  if (
    !is.character(timezone) ||
      length(timezone) != 1L ||
      is.na(timezone) ||
      !timezone %in% c("UTC", OlsonNames())
  ) {
    .islh_abort("{.arg timezone} must be a valid IANA timezone.", call = call)
  }
  timezone
}

.islh_surv_check_alignment <- function(
  meta,
  interval,
  week_start,
  dates,
  call = rlang::caller_env()
) {
  .islh_surv_check_source(meta, interval, call = call)
  if (is.null(meta)) {
    return(invisible(NULL))
  }
  if (!is.null(meta$periods) && meta$periods != 1L) {
    .islh_abort(
      "Aggregated windows cannot be recounted; supply one row per source period.",
      call = call
    )
  }
  anchor <- .islh_surv_week_start(
    meta$interval,
    meta$week_start %||% week_start,
    call = call
  )
  starts <- .islh_surv_period_start(dates, meta$interval, anchor)
  ends <- .islh_surv_period_end(starts, meta$interval, anchor)
  target_end <- .islh_surv_period_end(
    .islh_surv_period_start(dates, interval, week_start),
    interval,
    week_start
  )
  if (any(starts != dates) || any(ends > target_end)) {
    .islh_abort(
      "Source periods are not aligned with the requested calendar; recount the original events.",
      call = call
    )
  }
  invisible(NULL)
}

# A partial period holds only part of its days, so its count is not a count
# for the period its date names. `partial_period` comes from
# islh_count_events(include_partial = TRUE). A missing flag is unknown, and
# unknown is refused too. Every function that treats a row as a whole period
# calls this on the rows it uses.
.islh_surv_check_partial <- function(
  work,
  rows = rep(TRUE, nrow(work)),
  call = rlang::caller_env()
) {
  if (!"partial_period" %in% names(work)) {
    return(invisible(NULL))
  }
  flag <- work$partial_period[rows]
  if (!is.logical(flag)) {
    .islh_abort(
      "{.field partial_period} must be logical.",
      call = call
    )
  }
  bad <- is.na(flag) | flag
  if (any(bad)) {
    dates <- format(sort(unique(work$.islh_date[rows][bad])))
    .islh_abort(
      c(
        "{.arg data} contains partial periods.",
        x = "{cli::qty(length(dates))}Partial or unknown: {.val {dates}}.",
        i = "A partial period's count does not cover the period its date
             names. Recount the original events with
             {.code include_partial = FALSE}, or end the analysis before the
             partial period."
      ),
      call = call
    )
  }
  invisible(NULL)
}

# Shared by islh_count_events() (recounting preaggregated input) and
# islh_surveillance_snapshot(). `context` words each error for the function
# that raised it, since only the snapshot has a `missing_periods` argument.
.islh_surv_complete_source <- function(
  work,
  by,
  meta,
  from,
  to,
  policy,
  context = c("snapshot", "count"),
  groups = NULL,
  call = rlang::caller_env()
) {
  context <- match.arg(context)
  keys <- c(by, ".islh_date")
  if (anyDuplicated(work[keys])) {
    .islh_abort(
      "Source counts must contain one row per group and period.",
      call = call
    )
  }
  anchor <- .islh_surv_week_start(
    meta$interval,
    meta$week_start %||% 1L,
    call = call
  )
  starts <- .islh_surv_period_start(from, meta$interval, anchor)
  last <- .islh_surv_period_start(to, meta$interval, anchor)
  if (
    starts != from || .islh_surv_period_end(last, meta$interval, anchor) != to
  ) {
    .islh_abort(
      if (context == "snapshot") {
        "The snapshot window splits a source period; recount the original events."
      } else {
        c(
          "The {.arg from}-{.arg to} window splits a source period.",
          i = "Set {.arg from} and {.arg to} to source period boundaries, or
               recount the original events."
        )
      },
      call = call
    )
  }
  selected <- work$.islh_date >= from & work$.islh_date <= to
  .islh_surv_check_partial(work, selected, call = call)
  dates <- .islh_surv_period_sequence(starts, last, meta$interval)
  # `groups` is the roster that must appear even without rows, such as an
  # expected site that sent nothing.
  grid <- .islh_surv_grid(groups %||% unique(work[by]), dates, by)
  names(grid)[names(grid) == "period_start"] <- ".islh_date"
  work$.islh_present <- TRUE
  out <- dplyr::left_join(
    grid,
    work[c(keys, ".islh_value", ".islh_present")],
    by = keys,
    relationship = "one-to-one"
  )
  missing <- is.na(out$.islh_present)
  if (any(missing) && policy == "error") {
    n_missing <- sum(missing)
    .islh_abort(
      c(
        "{.arg data} is missing {n_missing} source period{?s} in the window.",
        i = if (context == "snapshot") {
          "Supply complete coverage, or say how to treat the gaps with
           {.arg missing_periods}."
        } else {
          "Supply one row per group and source period, including zeros, or
           recount the original events."
        }
      ),
      call = call
    )
  }
  if (policy == "zero") {
    out$.islh_value[missing] <- 0
  }
  out$.islh_present <- NULL
  out
}
