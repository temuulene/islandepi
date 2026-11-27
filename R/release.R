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
#'   still work. `POSIXlt` times are converted to `POSIXct`. Columns of other
#'   classes, matrix columns and S4 objects are refused, because removing
#'   their attributes would change or break them; convert them to plain
#'   values first.
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

  # unclass() takes exactly the named columns. A subclass such as sf adds its
  # geometry to every subset of columns.
  columns <- unclass(data)[cols]
  columns <- lapply(columns, function(column) {
    if (inherits(column, "POSIXlt")) as.POSIXct(column) else column
  })

  nested <- cols[!vapply(columns, is.atomic, logical(1))]
  if (length(nested) > 0L) {
    .islh_abort(c(
      "{.field {nested}} {?is a list column/are list columns}.",
      x = "A list column can hold anything, including working detail, and
           does not export as a flat table.",
      i = "Leave {cli::qty(length(nested))}{?it/them} out of {.arg cols}, or
           convert to text first."
    ))
  }

  # Only classes whose meaning survives keeping just these attributes. A
  # time series loses its `tsp`, and a matrix column its shape, which would
  # leave a corrupted table.
  safe_classes <- c(
    "factor",
    "ordered",
    "Date",
    "POSIXct",
    "POSIXt",
    "difftime",
    "integer64"
  )
  unsafe <- cols[vapply(
    columns,
    function(column) {
      isS4(column) ||
        !is.null(dim(column)) ||
        (is.object(column) && !all(class(column) %in% safe_classes))
    },
    logical(1)
  )]
  if (length(unsafe) > 0L) {
    .islh_abort(c(
      "{.field {unsafe}} cannot be copied safely.",
      x = "{cli::qty(length(unsafe))}{?It is a matrix, S4 or other classed
           column/They are matrix, S4 or other classed columns} whose
           working attributes cannot be removed without changing
           {?it/them}.",
      i = "Convert {cli::qty(length(unsafe))}{?it/them} to plain values first,
           for example with
           {.fn as.vector}, {.fn as.numeric} or {.fn as.character}."
    ))
  }

  keep <- c("class", "levels", "tzone", "units")
  out <- lapply(columns, function(column) {
    attributes(column) <- attributes(column)[intersect(
      names(attributes(column)),
      keep
    )]
    column
  })
  names(out) <- cols
  structure(out, class = "data.frame", row.names = seq_len(nrow(data)))
}
