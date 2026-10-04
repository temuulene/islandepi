# Row identity for combinations of key columns.
#
# Rows share a key only when every key column matches. A missing value matches
# another missing value and nothing else, so a missing site and a site called
# "NA" stay apart, and no character in a value can make two combinations
# collide, as it can when values are pasted together with a separator.
#
# These ids are for grouping, joining and matching. Strings built by pasting
# key values together are for messages only.

# One integer id per row of each table, comparable across the tables. Pass
# tables whose key columns have already been checked for compatible types.
.islh_key_ids <- function(tables, cols) {
  sizes <- vapply(tables, nrow, integer(1))
  if (length(cols) == 0L) {
    return(lapply(sizes, function(n) rep(1L, n)))
  }
  stacked <- dplyr::bind_rows(lapply(tables, function(x) {
    x <- as.data.frame(x)[cols]
    rownames(x) <- NULL
    x
  }))
  ids <- dplyr::group_indices(dplyr::group_by(
    stacked,
    dplyr::across(tidyselect::all_of(cols))
  ))
  unname(split(ids, factor(rep(seq_along(tables), sizes), seq_along(tables))))
}

# Like match(), for rows: the position in `table` of each row of `x`.
.islh_key_match <- function(x, table, cols) {
  ids <- .islh_key_ids(list(x, table), cols)
  match(ids[[1]], ids[[2]])
}

# Like duplicated(), for rows.
.islh_key_duplicated <- function(x, cols, from_last = FALSE) {
  duplicated(.islh_key_ids(list(x), cols)[[1]], fromLast = from_last)
}

# "site = NA, period = 2026-01-05" style labels for messages. Not for matching.
.islh_key_labels <- function(x, cols) {
  if (length(cols) == 0L || nrow(x) == 0L) {
    return(character(nrow(x)))
  }
  parts <- lapply(cols, function(col) {
    value <- x[[col]]
    shown <- format(value)
    shown[is.na(value)] <- "NA"
    shown[!is.na(value) & is.character(value)] <- encodeString(
      value[!is.na(value)],
      quote = "\""
    )
    shown
  })
  if (length(cols) == 1L) {
    return(trimws(parts[[1]]))
  }
  labelled <- Map(
    function(col, shown) paste0(col, " = ", trimws(shown)),
    cols,
    parts
  )
  do.call(paste, c(unname(labelled), list(sep = ", ")))
}
