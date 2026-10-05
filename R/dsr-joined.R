#' Direct standardization with validated stratum keys
#'
#' Aligns three tables using explicit keys before calling [islh_dsr()]. All
#' tables must cover exactly the same unique, non-missing strata. Aggregate
#' counts and denominators to a common age scheme before using this function.
#' @param events Data frame containing stratum keys and case counts.
#' @param population Data frame containing stratum keys and denominators.
#' @param standard Data frame containing stratum keys and standard weights.
#' @param by Character vector naming the stratum keys in all three tables.
#' @param cases Name of the case-count column in events.
#' @param denominator Name of the denominator column in population.
#' @param weight Name of the standard-population column in standard.
#' @param standard_id Required non-empty standard identifier and vintage.
#' @param ... Other arguments to [islh_dsr()], such as per, conf and method.
#' @return A one-row rate table with the exact stratum key table attached as
#'   `islh_strata`. [islh_dsr_detail()] shows each stratum's weight, rate,
#'   contribution and share of the variance, labelled by the keys.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Each table can arrive in its own row order; the keys line them up.
#' events <- tibble(
#'   age = c("0-44", "45-64", "65+"),
#'   cases = c(12, 30, 85)
#' )
#' population <- tibble(
#'   age = c("65+", "0-44", "45-64"),
#'   population = c(21000, 60000, 31000)
#' )
#' standard <- tibble(
#'   age = c("45-64", "65+", "0-44"),
#'   population = c(26000, 17000, 57000)
#' )
#'
#' islh_dsr_joined(
#'   events,
#'   population,
#'   standard,
#'   by = "age",
#'   standard_id = "Illustrative standard, 2026"
#' )
islh_dsr_joined <- function(
  events,
  population,
  standard,
  by,
  cases = "cases",
  denominator = "population",
  weight = "population",
  standard_id,
  ...
) {
  if (
    !is.character(by) ||
      !length(by) ||
      anyNA(by) ||
      any(!nzchar(by)) ||
      anyDuplicated(by)
  ) {
    .islh_abort("{.arg by} must name unique stratum columns.")
  }
  if (
    missing(standard_id) ||
      !is.character(standard_id) ||
      length(standard_id) != 1L ||
      is.na(standard_id) ||
      !nzchar(trimws(standard_id))
  ) {
    .islh_abort("Supply a non-empty {.arg standard_id} including its vintage.")
  }
  tables <- list(events = events, population = population, standard = standard)
  values <- list(cases, denominator, weight)
  value_args <- c("cases", "denominator", "weight")
  for (i in seq_along(tables)) {
    tab <- tables[[i]]
    table_arg <- names(tables)[[i]]
    column <- values[[i]]
    column_arg <- value_args[[i]]
    if (!is.data.frame(tab)) {
      .islh_abort(
        "{.arg {table_arg}} must be a data frame, not {.cls {class(tab)[1]}}."
      )
    }
    if (!is.character(column) || length(column) != 1L || is.na(column)) {
      .islh_abort("{.arg {column_arg}} must be one column name.")
    }
    if (column %in% by) {
      .islh_abort(c(
        "{.arg {column_arg}} must not be a stratum key.",
        x = "{.field {column}} is also in {.arg by}."
      ))
    }
    absent <- setdiff(c(by, column), names(tab))
    if (length(absent)) {
      .islh_abort(
        "{.arg {table_arg}} has no column{?s} named {.field {absent}}."
      )
    }
    if (!nrow(tab)) {
      .islh_abort("{.arg {table_arg}} has no rows.")
    }
    if (anyNA(tab[by])) {
      .islh_abort("{.arg {table_arg}} has missing stratum keys.")
    }
    if (anyDuplicated(tab[by])) {
      .islh_abort(c(
        "{.arg {table_arg}} has more than one row for some strata.",
        i = "Aggregate it to one row per stratum first."
      ))
    }
    if (i > 1L) {
      extra <- dplyr::anti_join(tab[by], events[by], by = by)
      absent <- dplyr::anti_join(events[by], tab[by], by = by)
      if (nrow(extra) || nrow(absent)) {
        .islh_abort(c(
          "The strata in {.arg {table_arg}} do not match those in
           {.arg events}.",
          x = if (nrow(absent)) {
            "{nrow(absent)} {?stratum/strata} in {.arg events} {?is/are}
             missing from {.arg {table_arg}}."
          },
          x = if (nrow(extra)) {
            "{nrow(extra)} {?stratum/strata} in {.arg {table_arg}} {?is/are}
             not in {.arg events}."
          },
          i = "Aggregate all three tables to the same strata first."
        ))
      }
    }
  }
  aligned <- lapply(seq_along(tables), function(i) {
    tab <- tables[[i]][c(by, values[[i]])]
    names(tab)[ncol(tab)] <- ".islh_measure"
    dplyr::left_join(
      events[by],
      tab,
      by = by,
      relationship = "one-to-one"
    )$.islh_measure
  })
  result <- islh_dsr(
    aligned[[1]],
    aligned[[2]],
    aligned[[3]],
    standard_id = standard_id,
    strata = seq_len(nrow(events)),
    ...
  )
  attr(result, "islh_strata") <- events[by]
  detail <- attr(result, "islh_dsr_strata")
  keys <- events[by]
  rownames(keys) <- NULL
  attr(result, "islh_dsr_strata") <- cbind(
    as.data.frame(keys),
    detail[setdiff(names(detail), "stratum")]
  )
  result
}
