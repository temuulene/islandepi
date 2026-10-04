#' Compare two versions of an output table
#'
#' `islh_compare_outputs()` lines up two tables on their keys and reports every
#' cell that differs, plus the rows that only one table has. Use it to
#' reconcile a new R output with a legacy report, a Power BI extract or last
#' week's run before switching over.
#'
#' A zero-row result means the tables agree on every compared value.
#'
#' @section Matching rules:
#'
#' * Keys must be unique in each table. A repeated key makes the comparison
#'   ambiguous, so it stops.
#' * Key columns must have compatible types in both tables, as in
#'   [islh_join_denominator()]. Missing key values match each other.
#' * Numeric columns match when they differ by no more than `tolerance`.
#'   With the default `tolerance = 0` they must be exactly equal: any
#'   difference at all is reported. With a positive tolerance, a difference
#'   that exceeds it only by floating-point error, such as `78.45 - 78.40`
#'   against `0.05`, counts as within it.
#' * Infinite values match only the same infinite value.
#' * Other columns must be identical as text, so a suppressed `"Suppressed"`
#'   in one table and `3` in the other is reported as different.
#' * Two missing values match. A missing value and a present one do not.
#'
#' Columns that only one table has are not compared, and a message names them.
#'
#' @param x,y Data frames to compare. By convention `x` is the new output and
#'   `y` the reference.
#' @param by Character vector naming the key columns in both tables.
#' @param values Columns to compare. `NULL` compares every non-key column the
#'   two tables share.
#' @param tolerance Largest absolute difference treated as a match for numeric
#'   columns. The default of 0 requires exact agreement, which suits counts.
#'   Use a small value, such as `0.05`, for rates rounded to one decimal.
#' @param all Return every compared cell, including matches. By default only
#'   differences are returned.
#'
#' @return A data frame with the key columns, `column`, `x`, `y`, `difference`
#'   and `status`. `status` is `"different"`, `"only_in_x"` or `"only_in_y"`,
#'   or `"match"` when `all = TRUE`. `x` and `y` are numeric when every
#'   compared column is numeric, and text otherwise. `difference` is `x - y`
#'   for numeric columns. A row only one table has appears once per compared
#'   column.
#' @export
#'
#' @examples
#' r_output <- data.frame(
#'   hsda = c("South", "Central", "North"),
#'   cases = c(412, 251, 127),
#'   rate = c(88.8, 78.4, 88.6)
#' )
#' dashboard <- data.frame(
#'   hsda = c("South", "Central", "North Island"),
#'   cases = c(412, 250, 127),
#'   rate = c(88.8, 78.0, 88.6)
#' )
#'
#' islh_compare_outputs(r_output, dashboard, by = "hsda", tolerance = 0.05)
islh_compare_outputs <- function(
  x,
  y,
  by,
  values = NULL,
  tolerance = 0,
  all = FALSE
) {
  if (!is.data.frame(x)) {
    .islh_abort("{.arg x} must be a data frame.")
  }
  if (!is.data.frame(y)) {
    .islh_abort("{.arg y} must be a data frame.")
  }
  by <- .islh_check_keys(by)
  all <- .islh_check_flag(all, "all")
  if (
    !is.numeric(tolerance) ||
      length(tolerance) != 1L ||
      is.na(tolerance) ||
      !is.finite(tolerance) ||
      tolerance < 0
  ) {
    .islh_abort("{.arg tolerance} must be one non-negative number.")
  }
  reserved <- intersect(by, c("column", "x", "y", "difference", "status"))
  if (length(reserved) > 0L) {
    .islh_abort(c(
      "Key columns use names reserved by the result.",
      x = "Reserved: {.val {reserved}}."
    ))
  }

  .islh_check_key_columns(x, y, by, "x", "y", allow_na = TRUE)
  .islh_check_key_columns(y, x, by, "y", "x", allow_na = TRUE)
  keys <- .islh_key_ids(list(x, y), by)
  x_key <- keys[[1]]
  y_key <- keys[[2]]
  for (side in c("x", "y")) {
    key <- if (side == "x") x_key else y_key
    repeated <- unique(key[duplicated(key)])
    if (length(repeated) > 0L) {
      .islh_abort(c(
        "{.arg {side}} has more than one row for some keys.",
        x = "{length(repeated)} key{?s} {?is/are} repeated.",
        i = "Add the missing key columns to {.arg by}, or aggregate first."
      ))
    }
  }

  shared <- setdiff(intersect(names(x), names(y)), by)
  if (is.null(values)) {
    values <- shared
    one_side <- setdiff(union(names(x), names(y)), c(by, shared))
    if (length(one_side) > 0L) {
      .islh_inform(
        "Not compared, because only one table has {?it/them}:
         {.field {one_side}}."
      )
    }
  } else {
    if (
      !is.character(values) ||
        length(values) == 0L ||
        anyNA(values) ||
        anyDuplicated(values)
    ) {
      .islh_abort("{.arg values} must be distinct column names.")
    }
    if (any(values %in% by)) {
      .islh_abort("{.arg values} must not include key columns.")
    }
    for (side in c("x", "y")) {
      absent <- setdiff(values, names(if (side == "x") x else y))
      if (length(absent) > 0L) {
        .islh_abort(
          "{.arg {side}} has no column{?s} named {.field {absent}}."
        )
      }
    }
  }
  if (length(values) == 0L) {
    .islh_abort("The tables share no columns to compare besides the keys.")
  }

  numeric_output <- all(vapply(
    values,
    function(v) is.numeric(x[[v]]) && is.numeric(y[[v]]),
    logical(1)
  ))

  key_order <- c(x_key, setdiff(y_key, x_key))
  in_x <- match(key_order, x_key)
  in_y <- match(key_order, y_key)
  keys <- x[in_x, by, drop = FALSE]
  from_y <- is.na(in_x)
  if (any(from_y)) {
    keys[from_y, ] <- y[in_y[from_y], by, drop = FALSE]
  }
  rownames(keys) <- NULL

  pieces <- lapply(values, function(v) {
    xv <- x[[v]][in_x]
    yv <- y[[v]][in_y]
    both_numeric <- is.numeric(x[[v]]) && is.numeric(y[[v]])
    difference <- if (both_numeric) xv - yv else rep(NA_real_, length(xv))
    same <- if (both_numeric) {
      .islh_numeric_match(xv, yv, tolerance)
    } else {
      xs <- as.character(xv)
      ys <- as.character(yv)
      (is.na(xs) & is.na(ys)) | (!is.na(xs) & !is.na(ys) & xs == ys)
    }
    status <- ifelse(same, "match", "different")
    status[is.na(in_y)] <- "only_in_x"
    status[is.na(in_x)] <- "only_in_y"
    if (!numeric_output) {
      xv <- as.character(xv)
      yv <- as.character(yv)
    }
    piece <- keys
    piece$column <- rep(v, nrow(keys))
    piece$x <- xv
    piece$y <- yv
    piece$difference <- difference
    piece$status <- status
    piece$.islh_order <- seq_len(nrow(keys))
    piece
  })

  out <- do.call(rbind, pieces)
  out <- out[order(out$.islh_order, match(out$column, values)), , drop = FALSE]
  out$.islh_order <- NULL
  if (!all) {
    out <- out[out$status != "match", , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

# Numeric cells match when both are missing, when they are equal (which
# covers two identical infinite values), or, with a positive tolerance, when
# they differ by no more than it. The allowance scales with the values, so it
# absorbs the rounding in a subtraction such as 78.45 - 78.40 and nothing
# more. A zero tolerance means exact equality.
.islh_numeric_match <- function(x, y, tolerance) {
  both_missing <- is.na(x) & is.na(y)
  equal <- !is.na(x) & !is.na(y) & x == y
  if (tolerance == 0) {
    return(both_missing | equal)
  }
  difference <- abs(x - y)
  allowance <- 64 * .Machine$double.eps * pmax(abs(x), abs(y), tolerance)
  within <- is.finite(difference) & difference <= tolerance + allowance
  both_missing | equal | within
}
