# Surveillance data helpers --------------------------------------------------

.islh_surv_select <- function(
  data,
  quo,
  arg,
  minimum = 0L,
  maximum = Inf,
  call = rlang::caller_env()
) {
  if (rlang::quo_is_null(quo)) {
    selected <- character()
  } else {
    selected <- tryCatch(
      names(tidyselect::eval_select(quo, data = data)),
      error = function(error) {
        .islh_abort(
          c(
            "Could not select {.arg {arg}} from {.arg data}.",
            x = conditionMessage(error)
          ),
          call = call
        )
      }
    )
  }

  if (length(selected) < minimum || length(selected) > maximum) {
    if (minimum == 1L && maximum == 1L) {
      .islh_abort("{.arg {arg}} must select exactly one column.", call = call)
    }
    .islh_abort(
      "{.arg {arg}} selected an unsupported number of columns.",
      call = call
    )
  }

  selected
}

.islh_surv_missing <- function(x) {
  missing <- is.na(x)
  if (is.character(x) || is.factor(x)) {
    missing <- missing | trimws(as.character(x)) == ""
  }
  missing
}

.islh_surv_date_info <- function(
  x,
  timezone = "America/Vancouver",
  call = rlang::caller_env()
) {
  timezone <- .islh_surv_timezone(timezone, call = call)
  n <- length(x)
  value <- rep(as.Date(NA), n)
  missing <- .islh_surv_missing(x)
  invalid <- rep(FALSE, n)

  if (inherits(x, "Date")) {
    value <- as.Date(x)
  } else if (inherits(x, "POSIXt")) {
    value <- as.Date(as.POSIXct(x), tz = timezone)
  } else if (is.character(x) || is.factor(x)) {
    text <- trimws(as.character(x))
    shape_ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text)
    parsed <- suppressWarnings(as.Date(text, format = "%Y-%m-%d"))
    round_trip <- !is.na(parsed) & format(parsed, "%Y-%m-%d") == text
    valid <- !missing & shape_ok & round_trip
    value[valid] <- parsed[valid]
    invalid <- !missing & !valid
  } else {
    invalid <- !missing
  }

  invalid <- invalid | (!missing & !is.finite(as.numeric(value)))
  list(value = value, missing = missing, invalid = invalid)
}

.islh_surv_as_date <- function(
  x,
  arg,
  scalar = FALSE,
  timezone = "America/Vancouver",
  call = rlang::caller_env()
) {
  info <- .islh_surv_date_info(x, timezone, call = call)
  if (any(info$missing)) {
    .islh_abort(
      c(
        "{.arg {arg}} must not contain missing dates.",
        x = "Found {sum(info$missing)} missing date value{?s}."
      ),
      call = call
    )
  }
  if (any(info$invalid)) {
    bad <- unique(as.character(x[info$invalid]))
    .islh_abort(
      c(
        "{.arg {arg}} contains invalid dates.",
        x = "Invalid: {.val {bad}}.",
        i = "Use Date values or ISO dates written as YYYY-MM-DD."
      ),
      call = call
    )
  }
  if (isTRUE(scalar) && length(info$value) != 1L) {
    .islh_abort("{.arg {arg}} must be one date.", call = call)
  }
  info$value
}

# Works like match.arg(), including partial matching and taking the first
# choice for an untouched default, but raises a package error.
.islh_surv_interval <- function(
  interval,
  arg = "interval",
  call = rlang::caller_env()
) {
  choices <- c("day", "week", "isoweek", "epiweek", "month", "quarter", "year")
  if (identical(interval, choices)) {
    return(choices[[1]])
  }
  matched <- NA_character_
  if (is.character(interval) && length(interval) == 1L && !is.na(interval)) {
    matched <- choices[pmatch(interval, choices)]
  }
  if (is.na(matched)) {
    .islh_abort("{.arg {arg}} must be one of {.val {choices}}.", call = call)
  }
  matched
}

# How long is one reporting period?
#
# Days and months are not interchangeable, because a month is not a fixed
# number of days. A duration therefore carries its unit, and two durations only
# compare within the same unit. That is what stops a 30-day window being
# treated as a month.
.islh_surv_duration <- function(interval, periods = 1L) {
  base <- switch(
    interval,
    day = list(unit = "day", amount = 1),
    week = list(unit = "day", amount = 7),
    isoweek = list(unit = "day", amount = 7),
    epiweek = list(unit = "day", amount = 7),
    month = list(unit = "month", amount = 1),
    quarter = list(unit = "month", amount = 3),
    year = list(unit = "month", amount = 12)
  )
  base$amount <- base$amount * periods
  base
}

.islh_surv_duration_label <- function(duration) {
  unit <- if (duration$amount == 1) {
    duration$unit
  } else {
    paste0(duration$unit, "s")
  }
  paste(duration$amount, unit)
}

.islh_surv_same_duration <- function(x, y) {
  identical(x$unit, y$unit) && isTRUE(all.equal(x$amount, y$amount))
}

# Reporting-period metadata travels with a result so a later function can tell
# what one row actually covers. Without it a seven-day snapshot can be compared
# against a baseline of single daily counts, and nothing in the numbers says
# the comparison is wrong.
#
# `periods` is how many `interval` periods one row summarizes: 1 for a period
# count or a baseline limit, and the window length for a snapshot total.
#
# `id_repeats` is set when counts are distinct identifiers. It records how many
# identifiers appear in more than one period, and in more than one group, so a
# later sum can say when it counts the same person twice. See
# .islh_surv_check_id_sum().
.islh_surv_set_meta <- function(
  x,
  interval,
  week_start = NULL,
  periods = 1L,
  from = NULL,
  to = NULL,
  id_repeats = NULL
) {
  if (is.null(interval)) {
    return(x)
  }
  attr(x, "islh_interval") <- interval
  attr(x, "islh_week_start") <- week_start
  attr(x, "islh_periods") <- as.integer(periods)
  attr(x, "islh_from") <- from
  attr(x, "islh_to") <- to
  attr(x, "islh_id_repeats") <- id_repeats
  x
}

.islh_surv_meta <- function(x) {
  interval <- attr(x, "islh_interval", exact = TRUE)
  if (
    is.null(interval) ||
      !is.character(interval) ||
      length(interval) != 1L ||
      is.na(interval) ||
      !interval %in%
        c(
          "day",
          "week",
          "isoweek",
          "epiweek",
          "month",
          "quarter",
          "year"
        )
  ) {
    return(NULL)
  }
  periods <- attr(x, "islh_periods", exact = TRUE)
  if (
    is.null(periods) ||
      !is.numeric(periods) ||
      length(periods) != 1L ||
      is.na(periods) ||
      periods < 1
  ) {
    periods <- 1L
  }
  list(
    interval = interval,
    week_start = attr(x, "islh_week_start", exact = TRUE),
    periods = as.integer(periods),
    from = attr(x, "islh_from", exact = TRUE),
    to = attr(x, "islh_to", exact = TRUE),
    id_repeats = attr(x, "islh_id_repeats", exact = TRUE)
  )
}

# Last resort when a table carries no metadata. The spacing of the dates only
# bounds what one row covers: rows seven days apart could be weekly totals or
# daily counts with six days missing in between, and nothing in the table says
# which. Only consecutive days settle it, because no period starting on each
# day can be longer than a day. Anything else returns NULL, and the caller asks
# for the period to be named.
.islh_surv_infer_interval <- function(dates) {
  dates <- sort(unique(dates))
  if (length(dates) < 2L) {
    return(NULL)
  }
  if (all(as.numeric(diff(dates)) == 1)) {
    return("day")
  }
  NULL
}

# Which interval does this table use? An explicit argument wins, then the
# metadata a previous function stamped on, then the spacing of the dates.
# An explicit argument that contradicts the metadata is a mistake worth
# stopping for, so the two are compared by duration rather than by name:
# "week", "isoweek" and "epiweek" all describe seven days.
.islh_surv_resolve_interval <- function(
  interval,
  meta,
  dates,
  arg = "interval",
  call = rlang::caller_env()
) {
  if (is.null(interval)) {
    if (!is.null(meta)) {
      return(meta$interval)
    }
    return(.islh_surv_infer_interval(dates))
  }

  interval <- .islh_surv_interval(interval, arg = arg, call = call)
  if (!is.null(meta)) {
    supplied <- .islh_surv_duration_label(.islh_surv_duration(interval))
    recorded <- .islh_surv_duration_label(
      .islh_surv_duration(meta$interval, meta$periods)
    )
    if (!identical(supplied, recorded)) {
      .islh_abort(
        c(
          "{.arg {arg}} does not match the reporting period of {.arg data}.",
          x = "{.arg {arg}} is {.val {interval}} ({supplied}), but {.arg data}
             holds {recorded} periods.",
          i = "Recount the data at {.val {interval}}, or drop {.arg {arg}}."
        ),
        call = call
      )
    }
  }
  interval
}

# Can the rows of `data` be summed into the periods a snapshot displays?
#
# Daily counts add up into a week. Weekly counts cannot be split back into
# days: flooring a week-start date to a day leaves one populated day and six
# empty ones, and the table looks plausible while being wrong.
.islh_surv_check_source <- function(
  meta,
  interval,
  arg = "data",
  call = rlang::caller_env()
) {
  if (is.null(meta)) {
    return(invisible(NULL))
  }

  source <- .islh_surv_duration(meta$interval, meta$periods)
  target <- .islh_surv_duration(interval)
  divides <- identical(source$unit, target$unit) &&
    source$amount <= target$amount &&
    isTRUE(all.equal(target$amount %% source$amount, 0))
  if (divides || (meta$interval == "day" && meta$periods == 1L)) {
    return(invisible(NULL))
  }

  have <- .islh_surv_duration_label(source)
  want <- .islh_surv_duration_label(target)
  .islh_abort(
    c(
      "{.arg {arg}} cannot be summarized into {.val {interval}} periods.",
      x = "{.arg {arg}} holds {have} periods, and this snapshot shows {want}
         periods.",
      i = "Recount the events at {.val {interval}} with
         {.fn islh_count_events}, or show the snapshot at the interval the
         data already uses."
    ),
    call = call
  )
}

# Does the baseline describe the same duration as the displayed total?
#
# The total spans `periods` of `interval`; a baseline limit spans one of its
# own reporting periods. Seven daily columns total one week, so a weekly
# baseline fits and a daily one is seven times too small.
.islh_surv_check_baseline <- function(
  baseline,
  meta,
  interval,
  periods,
  baseline_interval = NULL,
  call = rlang::caller_env()
) {
  if (is.null(baseline)) {
    return(invisible(NULL))
  }
  if (!is.data.frame(baseline)) {
    .islh_abort("{.arg baseline} must be a data frame or NULL.", call = call)
  }

  window <- .islh_surv_duration(interval, periods)
  shown <- .islh_surv_duration_label(window)

  # An explicit declaration covers limits that came from somewhere else
  # entirely, such as last season's totals read from a spreadsheet. When the
  # baseline also carries metadata the two must agree, or one of them is wrong.
  if (!is.null(baseline_interval)) {
    baseline_interval <- .islh_surv_interval(
      baseline_interval,
      arg = "baseline_interval",
      call = call
    )
    declared <- .islh_surv_duration(baseline_interval)
    if (!is.null(meta)) {
      recorded <- .islh_surv_duration(meta$interval, meta$periods)
      said <- .islh_surv_duration_label(declared)
      stamped <- .islh_surv_duration_label(recorded)
      if (!.islh_surv_same_duration(declared, recorded)) {
        .islh_abort(
          c(
            "{.arg baseline_interval} contradicts {.arg baseline}.",
            x = "{.arg baseline_interval} says {said}, but the baseline records
               {stamped}.",
            i = "Drop {.arg baseline_interval} to use the recorded period."
          ),
          call = call
        )
      }
    }
    meta <- list(interval = baseline_interval, periods = 1L)
  }

  if (is.null(meta)) {
    .islh_abort(
      c(
        "{.arg baseline} does not record the duration its limits describe.",
        x = "This snapshot totals {shown}, and comparing it against limits of an
           unknown duration could be wrong by any factor.",
        i = "Build it with {.fn islh_surveillance_baseline}, or name the duration
           its limits describe with {.arg baseline_interval}."
      ),
      call = call
    )
  }

  reference <- .islh_surv_duration(meta$interval, meta$periods)
  if (.islh_surv_same_duration(window, reference)) {
    return(invisible(NULL))
  }

  described <- .islh_surv_duration_label(reference)
  .islh_abort(
    c(
      "{.arg baseline} describes a different reporting period from this
     snapshot.",
      x = "The snapshot totals {shown}, but the baseline limits describe
         {described}.",
      i = "Build the baseline from historical totals of the same duration, for
         example weekly counts for a seven-day snapshot."
    ),
    call = call
  )
}

.islh_surv_week_start <- function(
  interval,
  week_start,
  call = rlang::caller_env()
) {
  if (interval == "isoweek") {
    return(1L)
  }
  if (interval == "epiweek") {
    return(7L)
  }
  if (
    !is.numeric(week_start) ||
      length(week_start) != 1L ||
      is.na(week_start) ||
      week_start != round(week_start) ||
      week_start < 1L ||
      week_start > 7L
  ) {
    .islh_abort(
      "{.arg week_start} must be one whole number from 1 to 7.",
      call = call
    )
  }
  as.integer(week_start)
}

.islh_surv_period_start <- function(date, interval, week_start) {
  unit <- if (interval %in% c("isoweek", "epiweek")) "week" else interval
  as.Date(lubridate::floor_date(date, unit = unit, week_start = week_start))
}

.islh_surv_period_end <- function(period_start, interval, week_start) {
  if (interval == "day") {
    return(period_start)
  }
  unit <- if (interval %in% c("isoweek", "epiweek")) "week" else interval
  as.Date(lubridate::ceiling_date(
    period_start,
    unit = unit,
    week_start = week_start,
    change_on_boundary = TRUE
  )) -
    1
}

.islh_surv_period_sequence <- function(from, to, interval) {
  by <- switch(
    interval,
    day = "day",
    week = "week",
    isoweek = "week",
    epiweek = "week",
    month = "month",
    quarter = "3 months",
    year = "year"
  )
  seq.Date(from = from, to = to, by = by)
}

.islh_surv_periods_back <- function(end, periods, interval) {
  by <- switch(
    interval,
    day = "-1 day",
    week = "-1 week",
    isoweek = "-1 week",
    epiweek = "-1 week",
    month = "-1 month",
    quarter = "-3 months",
    year = "-1 year"
  )
  sort(seq.Date(from = end, by = by, length.out = periods))
}

.islh_surv_groups <- function(
  groups,
  by,
  observed,
  call = rlang::caller_env()
) {
  if (length(by) == 0L) {
    if (!is.null(groups)) {
      .islh_abort(
        "{.arg groups} can only be supplied when {.arg by} is used.",
        call = call
      )
    }
    return(data.frame(.islh_all = 1L)[FALSE, , drop = FALSE])
  }

  if (is.null(groups)) {
    return(unique(observed[by]))
  }

  if (is.data.frame(groups)) {
    missing <- setdiff(by, names(groups))
    if (length(missing) > 0L) {
      .islh_abort(
        c(
          "{.arg groups} is missing grouping columns.",
          x = "Missing: {.val {missing}}."
        ),
        call = call
      )
    }
    return(unique(groups[by]))
  }

  if (length(by) != 1L) {
    .islh_abort(
      "For multiple {.arg by} columns, {.arg groups} must be a data frame.",
      call = call
    )
  }

  out <- data.frame(groups, stringsAsFactors = FALSE)
  names(out) <- by
  unique(out)
}

.islh_surv_grid <- function(groups, periods, by) {
  period_data <- data.frame(
    period_start = periods,
    stringsAsFactors = FALSE
  )

  if (length(by) == 0L) {
    return(period_data)
  }
  if (nrow(groups) == 0L) {
    return(groups[FALSE, , drop = FALSE])
  }
  dplyr::cross_join(groups, period_data)
}
