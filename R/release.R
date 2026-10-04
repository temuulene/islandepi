#' Make a copy of a table that is safe to share
#'
#' Results from this package carry working information as attributes: the
#' suppression audit from [islh_suppress_table()], the stratum counts behind a
#' rate from [islh_dsr()], and the calendar settings of a surveillance count
#' table. Printing a table or writing chosen columns to CSV leaves them out,
#' but they stay with the R object when it is saved with `saveRDS()` or passed
#' to another R session, and some of them can reveal hidden values.
#'
#' `islh_release_copy()` returns a plain data frame holding only the columns
#' you name, with none of that working information. Keep the original as your
#' internal working object, with its audit, and share the copy.
#'
#' There is no default for `cols`: deciding which columns leave your team is
#' part of the release, like the suppression threshold.
#'
#' @param data A data frame, usually the result of [islh_suppress_table()].
#' @param cols Columns to keep, as character names or whole numeric positions,
#'   in the order they should appear.
#'
#' @return A plain `data.frame` with the chosen columns and row names
#'   `1:nrow(data)`. Each column keeps only the attributes that define its type
#'   (`class`, `levels`, `tzone` and `units`), so dates, factors and times
#'   still work.
#' @export
#'
#' @examples
#' rate <- islh_dsr(
#'   cases = c(1, 2),
#'   population = c(1000, 2000),
#'   std_population = c(1000, 2000),
#'   strata = c("0-64", "65+")
#' )
#' hidden <- islh_suppress_table(
#'   rate, "cases", threshold = 5,
#'   linked = list(cases = c("rate", "lower", "upper"))
#' )
#'
#' # The working object keeps its audit for reviewers.
#' islh_suppression_audit(hidden)
#'
#' # The copy to share keeps only the approved columns.
#' shared <- islh_release_copy(hidden, c("cases", "rate", "lower", "upper"))
#' shared
#' names(attributes(shared))
islh_release_copy <- function(data, cols) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  cols <- .islh_check_cols(cols, data, purpose = "release")

  nested <- cols[!vapply(data[cols], is.atomic, logical(1))]
  if (length(nested) > 0L) {
    .islh_abort(c(
      "{.field {nested}} {?is a list column/are list columns}.",
      x = "A list column can hold anything, including working detail, and
           does not export as a flat table.",
      i = "Leave {cli::qty(length(nested))}{?it/them} out of {.arg cols}, or
           convert to text first."
    ))
  }

  keep <- c("class", "levels", "tzone", "units")
  out <- lapply(data[cols], function(column) {
    attributes(column) <- attributes(column)[intersect(
      names(attributes(column)),
      keep
    )]
    column
  })
  names(out) <- cols
  structure(out, class = "data.frame", row.names = seq_len(nrow(data)))
}
