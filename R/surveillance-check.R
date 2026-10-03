#' Find record-level problems in event data
#'
#' `islh_check_events()` returns one row for every problem it finds. It does not
#' alter the input and does not hide the affected records, making the result
#' suitable for review with a data owner before analysis continues.
#'
#' @param timezone Reporting timezone for timestamps. Defaults to
#'   `"America/Vancouver"`; Date inputs retain their calendar date.
#' @param data A data frame containing one row per event or encounter.
#' @param id Column or columns identifying an event. Several columns, such as
#'   `c(person_id, encounter_date)`, form a composite key: a record is a
#'   duplicate only when every key column matches, and `.id` joins the key
#'   values with `" | "`.
#' @param date Column containing the event date. Date, POSIXt and ISO
#'   `YYYY-MM-DD` character values are supported.
#' @param required Optional tidy-select specification of fields that must be
#'   present and non-missing.
#' @param one_row_per_id Whether repeated non-missing identifiers are problems.
#' @param min_date,max_date Optional inclusive date limits. `max_date` defaults
#'   to today's date in `timezone`, the same calendar the event dates are read
#'   in; use `NULL` when future-dated records are expected.
#' @param allowed Optional named list of permitted values, such as
#'   `list(sex = c("Female", "Male"))`. Each name is a column; a present value
#'   outside the list is reported as `"value_not_allowed"`. Missing values are
#'   left to `required`.
#' @param date_order Optional named character vector of date pairs that must
#'   be in order, written `c(later = "earlier")`. For example,
#'   `c(date_reported = "date_onset")` reports a record whose report date falls
#'   before its onset date as `"date_out_of_order"`. Dates on the same day
#'   pass.
#'
#' @return A data frame with `.row`, `.id`, `.issue`, `.field` and `.value`.
#'   A valid input returns a zero-row data frame with the same columns.
#'
#' @section Issues:
#'
#' `.issue` takes one of these values:
#'
#' * `"missing_id"`, `"missing_date"` and `"missing_required"`: the field is
#'   `NA` or blank.
#' * `"invalid_date"`: the value cannot be read as a date.
#' * `"duplicate_id"`: more than one record shares the identifier. Every copy
#'   is listed, so the data owner can choose which to keep.
#' * `"date_before_minimum"` and `"date_after_maximum"`: the event date is
#'   outside `min_date` to `max_date`.
#' * `"value_not_allowed"`: the value is not in `allowed`.
#' * `"date_out_of_order"`: the `.field` date falls before the date it must
#'   follow. `.value` shows both dates.
#'
#' The function only reports. Deciding which record is right, and correcting
#' it, belongs with the data owner, so no check changes the data.
#'
#' @examples
#' # The simulated outbreak is clean, so no problems are found.
#' islh_check_events(
#'   islh_outbreak,
#'   id = case_id,
#'   date = date_onset,
#'   required = c(hsda, case_type),
#'   max_date = "2026-03-08"
#' )
#'
#' # Damage a few records to see what the review looks like.
#' damaged <- islh_outbreak[1:6, ]
#' damaged$case_id[2] <- damaged$case_id[1]
#' damaged$date_onset[3] <- NA
#' damaged$hsda[4] <- ""
#' damaged$date_onset[5] <- as.Date("2027-01-01")
#'
#' islh_check_events(
#'   damaged,
#'   id = case_id,
#'   date = date_onset,
#'   required = hsda,
#'   max_date = "2026-03-08"
#' )
#'
#' # Check coded values and the order of two dates.
#' damaged$sex[6] <- "F"
#' damaged$date_reported[1] <- damaged$date_onset[1] - 3
#'
#' islh_check_events(
#'   damaged,
#'   id = case_id,
#'   date = date_onset,
#'   max_date = "2026-03-08",
#'   allowed = list(sex = c("Female", "Male")),
#'   date_order = c(date_reported = "date_onset")
#' )
#'
#' @export
islh_check_events <- function(
  data,
  id,
  date,
  required = NULL,
  one_row_per_id = TRUE,
  min_date = NULL,
  max_date = as.Date(Sys.time(), tz = timezone),
  timezone = "America/Vancouver",
  allowed = NULL,
  date_order = NULL
) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  one_row_per_id <- .islh_check_flag(one_row_per_id, "one_row_per_id")

  id_names <- .islh_surv_select(data, rlang::enquo(id), "id", 1L)
  date_name <- .islh_surv_select(data, rlang::enquo(date), "date", 1L, 1L)
  required_names <- .islh_surv_select(
    data,
    rlang::enquo(required),
    "required"
  )
  required_names <- unique(c(id_names, date_name, required_names))
  allowed <- .islh_check_allowed(allowed, data)
  date_order <- .islh_check_date_order(date_order, data)

  ids <- if (length(id_names) == 1L) {
    data[[id_names]]
  } else {
    do.call(paste, c(lapply(data[id_names], as.character), list(sep = " | ")))
  }
  date_info <- .islh_surv_date_info(data[[date_name]], timezone)
  # Each check appends its rows. Assigning NULL to a new list element is a
  # no-op, so a check that finds nothing adds nothing.
  issues <- list()
  issue_rows <- function(rows, issue, field, values) {
    rows <- which(rows)
    if (length(rows) == 0L) {
      return(NULL)
    }
    data.frame(
      .row = rows,
      .id = as.character(ids[rows]),
      .issue = issue,
      .field = field,
      .value = as.character(values[rows]),
      stringsAsFactors = FALSE
    )
  }

  for (field in required_names) {
    missing <- .islh_surv_missing(data[[field]])
    issue <- if (field %in% id_names) {
      "missing_id"
    } else if (field == date_name) {
      "missing_date"
    } else {
      "missing_required"
    }
    issues[[length(issues) + 1L]] <- issue_rows(
      missing,
      issue,
      field,
      data[[field]]
    )
  }

  issues[[length(issues) + 1L]] <- issue_rows(
    date_info$invalid,
    "invalid_date",
    date_name,
    data[[date_name]]
  )

  present_id <- Reduce(
    `&`,
    lapply(data[id_names], function(x) !.islh_surv_missing(x))
  )
  if (isTRUE(one_row_per_id)) {
    duplicate <- present_id &
      (duplicated(ids) | duplicated(ids, fromLast = TRUE))
    issues[[length(issues) + 1L]] <- issue_rows(
      duplicate,
      "duplicate_id",
      paste(id_names, collapse = " | "),
      ids
    )
  }

  usable_date <- !date_info$missing & !date_info$invalid
  if (!is.null(min_date)) {
    min_date <- .islh_surv_as_date(
      min_date,
      "min_date",
      scalar = TRUE,
      timezone = timezone
    )
    before <- usable_date & date_info$value < min_date
    issues[[length(issues) + 1L]] <- issue_rows(
      before,
      "date_before_minimum",
      date_name,
      data[[date_name]]
    )
  }
  if (!is.null(max_date)) {
    max_date <- .islh_surv_as_date(
      max_date,
      "max_date",
      scalar = TRUE,
      timezone = timezone
    )
    after <- usable_date & date_info$value > max_date
    issues[[length(issues) + 1L]] <- issue_rows(
      after,
      "date_after_maximum",
      date_name,
      data[[date_name]]
    )
  }

  for (field in names(allowed)) {
    values <- data[[field]]
    outside <- !.islh_surv_missing(values) &
      !as.character(values) %in% as.character(allowed[[field]])
    issues[[length(issues) + 1L]] <- issue_rows(
      outside,
      "value_not_allowed",
      field,
      values
    )
  }

  for (later in names(date_order)) {
    earlier <- date_order[[later]]
    later_info <- .islh_surv_date_info(data[[later]], timezone)
    earlier_info <- .islh_surv_date_info(data[[earlier]], timezone)
    # The event date's own invalid values are already reported above.
    for (field in setdiff(c(later, earlier), date_name)) {
      info <- if (field == later) later_info else earlier_info
      issues[[length(issues) + 1L]] <- issue_rows(
        info$invalid,
        "invalid_date",
        field,
        data[[field]]
      )
    }
    usable <- !later_info$missing &
      !later_info$invalid &
      !earlier_info$missing &
      !earlier_info$invalid
    shown <- paste0(
      format(later_info$value),
      " before ",
      earlier,
      " ",
      format(earlier_info$value)
    )
    issues[[length(issues) + 1L]] <- issue_rows(
      usable & later_info$value < earlier_info$value,
      "date_out_of_order",
      later,
      shown
    )
  }

  if (length(issues) == 0L) {
    return(data.frame(
      .row = integer(),
      .id = character(),
      .issue = character(),
      .field = character(),
      .value = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- dplyr::bind_rows(issues)
  out <- out[order(out$.row, out$.issue, out$.field), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.islh_check_allowed <- function(allowed, data, call = rlang::caller_env()) {
  if (is.null(allowed)) {
    return(list())
  }
  fields <- names(allowed)
  if (
    !is.list(allowed) ||
      is.data.frame(allowed) ||
      length(allowed) == 0L ||
      is.null(fields) ||
      anyNA(fields) ||
      any(!nzchar(fields)) ||
      anyDuplicated(fields)
  ) {
    .islh_abort(
      c(
        "{.arg allowed} must be a named list of permitted values.",
        i = "For example, {.code list(sex = c(\"Female\", \"Male\"))}."
      ),
      call = call
    )
  }
  unknown <- setdiff(fields, names(data))
  if (length(unknown) > 0L) {
    .islh_abort(
      "{.arg data} has no column{?s} named {.field {unknown}}.",
      call = call
    )
  }
  empty <- fields[
    !vapply(allowed, function(x) is.atomic(x) && length(x) > 0L, logical(1))
  ]
  if (length(empty) > 0L) {
    .islh_abort(
      "{.arg allowed} gives no permitted values for {.field {empty}}.",
      call = call
    )
  }
  allowed
}

.islh_check_date_order <- function(
  date_order,
  data,
  call = rlang::caller_env()
) {
  if (is.null(date_order)) {
    return(character())
  }
  later <- names(date_order)
  if (
    !is.character(date_order) ||
      length(date_order) == 0L ||
      anyNA(date_order) ||
      is.null(later) ||
      anyNA(later) ||
      any(!nzchar(later))
  ) {
    .islh_abort(
      c(
        "{.arg date_order} must be a named character vector.",
        i = "Write each pair as {.code c(later = \"earlier\")}, for example
             {.code c(date_reported = \"date_onset\")}."
      ),
      call = call
    )
  }
  unknown <- setdiff(c(later, unname(date_order)), names(data))
  if (length(unknown) > 0L) {
    .islh_abort(
      "{.arg data} has no column{?s} named {.field {unknown}}.",
      call = call
    )
  }
  same <- later[later == unname(date_order)]
  if (length(same) > 0L) {
    .islh_abort(
      "{.arg date_order} compares {.field {same}} with itself.",
      call = call
    )
  }
  date_order
}
