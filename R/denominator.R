#' Attach denominators to counts with checked keys
#'
#' `islh_join_denominator()` adds a population or person-time column to a
#' table of counts, matching rows on explicit keys such as geography, year, sex
#' and age group. It stops, rather than returning a table with gaps, when the
#' two tables do not line up.
#'
#' A plain join can go wrong without a sign. A population table with one row
#' per sex, joined to counts for both sexes together, doubles every row. A
#' code stored as the number `11` does not match the text `"011"`. A count
#' row with no denominator becomes a missing rate that looks like a gap in the
#' data. Each of those is an error here.
#'
#' @section Checks:
#'
#' * Every column in `by` exists in both tables, holds no missing values, and
#'   has a compatible type. Text and factors match each other. Text never
#'   matches numbers, because the padding of codes such as `"011"` is lost.
#' * `population` has at most one row per key, so the join cannot duplicate
#'   counts. Aggregate it first, for example by summing sexes.
#' * Every row of `data` finds a denominator. The error lists the first keys
#'   that do not.
#' * The denominator is numeric, non-missing, finite and not negative. Zero is
#'   allowed here, since it is a real value, but [islh_crude_rate()] and
#'   [islh_dsr()] refuse to divide by it.
#'
#' Denominator rows that no count matches are handled by `extra`. They are
#' expected when the population covers all of BC and the counts cover Island
#' Health. They are a problem when the counts left out strata with no events:
#' a stratum with zero cases still belongs in a rate. Complete the counts with
#' [islh_count_events()] and its `groups` argument, or add the zero rows,
#' before joining.
#'
#' @param data A data frame of counts, one row per stratum.
#' @param population A data frame of denominators, one row per stratum.
#' @param by Character vector naming the key columns, which must have the same
#'   names in both tables.
#' @param denominator Name of the denominator column in `population`. It is
#'   added to `data` under the same name, so `data` must not already have a
#'   column of that name.
#' @param extra What to do with denominator rows that no row of `data`
#'   matches: `"inform"` (default) sends a message with the number of rows,
#'   `"ignore"` says nothing, and `"error"` stops.
#'
#' @return `data`, in its original row order and class, with the denominator
#'   column added.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' cases <- tibble(
#'   year = 2025,
#'   age_group = c("0-19", "20-64", "65+"),
#'   cases = c(4, 22, 31)
#' )
#' population <- tibble(
#'   year = 2025,
#'   age_group = c("0-19", "20-64", "65+"),
#'   population = c(18000, 51000, 23000)
#' )
#'
#' cases |>
#'   islh_join_denominator(population, by = c("year", "age_group")) |>
#'   mutate(
#'     islh_crude_rate(cases, population) |>
#'       select(rate, lower, upper)
#'   )
#'
#' # A denominator table with one row per sex would double the counts, so
#' # it is refused.
#' by_sex <- bind_rows(
#'   population |> mutate(sex = "F"),
#'   population |> mutate(sex = "M")
#' )
#' try(islh_join_denominator(cases, by_sex, by = c("year", "age_group")))
islh_join_denominator <- function(
  data,
  population,
  by,
  denominator = "population",
  extra = c("inform", "ignore", "error")
) {
  extra <- match.arg(extra)
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  if (!is.data.frame(population)) {
    .islh_abort("{.arg population} must be a data frame.")
  }
  by <- .islh_check_keys(by)
  if (
    !is.character(denominator) ||
      length(denominator) != 1L ||
      is.na(denominator) ||
      !nzchar(denominator)
  ) {
    .islh_abort("{.arg denominator} must be one column name.")
  }
  if (denominator %in% by) {
    .islh_abort(c(
      "{.arg denominator} must not be a key.",
      x = "{.field {denominator}} is also in {.arg by}."
    ))
  }
  if (!denominator %in% names(population)) {
    .islh_abort(
      "{.arg population} has no column named {.field {denominator}}."
    )
  }
  if (denominator %in% names(data)) {
    .islh_abort(c(
      "{.arg data} already has a column named {.field {denominator}}.",
      i = "Rename or drop it first, so the joined denominator cannot be
           confused with it."
    ))
  }
  if (nrow(population) == 0L) {
    .islh_abort("{.arg population} has no rows.")
  }

  .islh_check_key_columns(data, population, by, "data", "population")
  .islh_check_key_columns(population, data, by, "population", "data")
  keys <- .islh_key_ids(list(data, population), by)
  data_key <- keys[[1]]
  population_key <- keys[[2]]

  repeated <- unique(population_key[duplicated(population_key)])
  if (length(repeated) > 0L) {
    .islh_abort(c(
      "{.arg population} has more than one row for some keys.",
      x = "{length(repeated)} key{?s} {?is/are} repeated, so the join would
           duplicate counts.",
      i = "Aggregate {.arg population} to one row per key first, for
           example by summing over sex or age."
    ))
  }

  values <- population[[denominator]]
  .islh_check_population(values, denominator, allow_zero = TRUE)

  position <- match(data_key, population_key)
  unmatched <- which(is.na(position))
  if (length(unmatched) > 0L) {
    shown <- .islh_describe_keys(data[unmatched, by, drop = FALSE])
    .islh_abort(c(
      "{length(unmatched)} row{?s} of {.arg data} {?has/have} no
       denominator.",
      x = "First unmatched: {shown}.",
      i = "Check code padding, geography type, year, sex and age-group
           labels."
    ))
  }

  unused <- sum(!population_key %in% data_key)
  if (unused > 0L && extra != "ignore") {
    message <- c(
      "{unused} row{?s} of {.arg population} {?matches/match} no row of
       {.arg data}.",
      i = "Expected when the denominators cover a wider area. Otherwise,
           check whether strata with no events were left out of {.arg data}."
    )
    if (extra == "error") {
      .islh_abort(message)
    }
    .islh_inform(message)
  }

  data[[denominator]] <- values[position]
  data
}

# Key columns named by a character vector, shared by the table-joining
# functions.
.islh_check_keys <- function(by, arg = "by", call = rlang::caller_env()) {
  if (
    missing(by) ||
      !is.character(by) ||
      length(by) == 0L ||
      anyNA(by) ||
      any(!nzchar(by)) ||
      anyDuplicated(by)
  ) {
    .islh_abort(
      "{.arg {arg}} must name one or more distinct key columns.",
      call = call
    )
  }
  by
}

# Checks the key columns exist and are compatible with the other table's.
# Text and factors compare as text; text never matches a number, because
# leading zeros in a code are lost. Rows are then matched with
# .islh_key_ids().
.islh_check_key_columns <- function(
  x,
  other,
  by,
  arg,
  other_arg,
  allow_na = FALSE,
  call = rlang::caller_env()
) {
  absent <- setdiff(by, names(x))
  if (length(absent) > 0L) {
    .islh_abort(
      "{.arg {arg}} has no column{?s} named {.field {absent}}.",
      call = call
    )
  }
  kind <- function(v) {
    if (is.character(v) || is.factor(v)) {
      "text"
    } else if (inherits(v, "Date")) {
      "date"
    } else if (is.numeric(v)) {
      "number"
    } else if (is.logical(v)) {
      "logical"
    } else {
      class(v)[1]
    }
  }
  for (field in by) {
    if (!allow_na && anyNA(x[[field]])) {
      .islh_abort(
        "{.arg {arg}} has missing values in key {.field {field}}.",
        call = call
      )
    }
    if (field %in% names(other)) {
      mine <- kind(x[[field]])
      theirs <- kind(other[[field]])
      if (!identical(mine, theirs)) {
        .islh_abort(
          c(
            "Key {.field {field}} has different types in the two tables.",
            x = "It is {mine} in {.arg {arg}} and {theirs} in
                 {.arg {other_arg}}.",
            i = if (
              "text" %in% c(mine, theirs) && "number" %in% c(mine, theirs)
            ) {
              "Convert the numbers to text with the same padding, for
               example {.code sprintf(\"%03d\", code)}."
            }
          ),
          call = call
        )
      }
    }
  }
  invisible(by)
}

# "year = 2025, age_group = \"0-19\"" for the first few rows of a key table.
.islh_describe_keys <- function(keys, limit = 3L) {
  keys <- utils::head(unique(keys), limit)
  rows <- vapply(
    seq_len(nrow(keys)),
    function(i) {
      paste0(
        names(keys),
        " = ",
        vapply(keys[i, , drop = FALSE], function(v) format(v[[1]]), ""),
        collapse = ", "
      )
    },
    character(1)
  )
  paste0("(", rows, ")", collapse = "; ")
}
