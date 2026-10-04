#' Model-based outbreak detection with the improved Farrington method
#'
#' `islh_farrington()` runs the improved Farrington algorithm from the
#' surveillance package on a complete table of weekly counts. For each week
#' assessed it fits a quasi-Poisson model to comparable weeks in earlier
#' years, allowing for trend, seasonality and overdispersion, and flags a week
#' whose count is above the model's upper limit.
#'
#' Unlike [islh_surveillance_baseline()], this is a model-based detection
#' method: `alpha` is a nominal false-alarm level under the model's
#' assumptions, not a guaranteed rate in practice. It still needs local
#' evaluation: before relying on it, run it over past seasons, count how many
#' alarms it raises and how many real events it would have caught, and set
#' `alpha` so the alert burden is workable. An alarm is a signal to review,
#' not a finding.
#'
#' @section Settings:
#'
#' The defaults are the improved-method settings described in the
#' surveillance package documentation, following Noufaily et al.: ten
#' seasonal periods (`no_periods = 10`), the last 26 weeks left out of the
#' model (`past_weeks_excluded = 26`), past outbreaks downweighted above an
#' Anscombe residual of 2.58, trend always kept, and a negative binomial
#' threshold (`"nbPlugin"`). `years_back` and `window` set which earlier weeks
#' the model sees.
#'
#' @section Weeks that are not assessed:
#'
#' `status` says what happened to each week:
#'
#' * `"assessed"`: the model gave an upper limit, and `alarm` is `TRUE` or
#'   `FALSE`.
#' * `"low_count_rule"`: fewer than `low_count` cases in the last
#'   `low_count_weeks` weeks, counting the week assessed. The method does not
#'   assess such weeks, to avoid alarms on very small counts, so `alarm`,
#'   `expected` and `upper_limit` are `NA`.
#' * `"fit_failed"`: the model could not be fitted, and `alarm` is `NA`.
#'
#' A week that was not assessed is not a week without a signal. Show the
#' status in any table of results, so a reader can tell a quiet week from one
#' the method set aside. For rare conditions the low-count rule sets aside
#' most weeks; whether that is acceptable is an alerting-policy decision, and
#' `low_count` and `low_count_weeks` make it explicit.
#'
#' The result records the settings used in its `islh_farrington` attribute.
#'
#' @param data A complete weekly count table with one row per group and week,
#'   normally from [islh_count_events()]. Every group must have every week,
#'   including zeros.
#' @param date Week start column.
#' @param value Whole-count column.
#' @param by Optional tidy-select specification of grouping columns. Each
#'   group is modelled separately.
#' @param from,to First and last week to assess, as week start dates. `to`
#'   defaults to `from`.
#' @param years_back Number of earlier years the model uses (`b`).
#' @param window Weeks either side of the matching week in each earlier year
#'   (`w`).
#' @param alpha One-sided level of the upper limit: 0.05 gives a 95% limit.
#' @param no_periods Number of seasonal periods in the model's factor.
#' @param past_weeks_excluded Recent weeks left out of the model, so a
#'   current outbreak does not raise its own limit.
#' @param threshold_method `"nbPlugin"`, `"muan"` or `"delta"`. See
#'   `surveillance::farringtonFlexible()`.
#' @param low_count,low_count_weeks A week is assessed only when the last
#'   `low_count_weeks` weeks, counting the week assessed, hold at least
#'   `low_count` cases. The defaults, 5 cases in 4 weeks, are the method's
#'   own. `low_count = 0` assesses every week.
#' @param timezone Reporting timezone for timestamps. Date values are
#'   unchanged.
#'
#' @return One row per group and week in `from` to `to`: the grouping
#'   columns, `period_start`, `observed`, `expected`, `upper_limit`, `alarm`,
#'   `pvalue` and `status`. `alarm` is `NA` for any week not assessed; see the
#'   section on weeks that are not assessed. `pvalue` is the model's, and is
#'   reported even when the low-count rule sets a week aside.
#' @export
#'
#' @references
#' Farrington CP, Andrews NJ, Beale AD, Catchpole MA (1996). A statistical
#' algorithm for the early detection of outbreaks of infectious disease.
#' *Journal of the Royal Statistical Society, Series A* 159(3):547-563.
#'
#' Noufaily A, Enki DG, Farrington P, Garthwaite P, Andrews N, Charlett A
#' (2013). An improved algorithm for outbreak detection in multiple
#' surveillance systems. *Statistics in Medicine* 32(7):1206-1222.
#'
#' Salmon M, Schumacher D, Hoehle M (2016). Monitoring count time series in
#' R: aberration detection in public health surveillance. *Journal of
#' Statistical Software* 70(10):1-35.
#'
#' @examplesIf requireNamespace("surveillance", quietly = TRUE)
#' islh_farrington(
#'   islh_seasons,
#'   date = period_start,
#'   value = count,
#'   by = hsda,
#'   from = "2025-12-01",
#'   to = "2026-01-26"
#' )
islh_farrington <- function(
  data,
  date,
  value,
  by = NULL,
  from,
  to = from,
  years_back = 5,
  window = 3,
  alpha = 0.05,
  no_periods = 10,
  past_weeks_excluded = 26,
  threshold_method = c("nbPlugin", "muan", "delta"),
  low_count = 5,
  low_count_weeks = 4,
  timezone = "America/Vancouver"
) {
  threshold_method <- match.arg(threshold_method)
  .islh_require_packages("surveillance", "Farrington outbreak detection")
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  if (missing(from)) {
    .islh_abort("Supply {.arg from}, the first week to assess.")
  }
  for (arg in c(
    "years_back",
    "window",
    "no_periods",
    "past_weeks_excluded",
    "low_count",
    "low_count_weeks"
  )) {
    x <- get(arg)
    minimum <- if (arg %in% c("years_back", "no_periods", "low_count_weeks")) {
      1
    } else {
      0
    }
    if (
      !is.numeric(x) ||
        length(x) != 1L ||
        is.na(x) ||
        x != round(x) ||
        x < minimum
    ) {
      .islh_abort(
        "{.arg {arg}} must be one whole number of at least {minimum}."
      )
    }
  }
  if (
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    .islh_abort("{.arg alpha} must be a single number between 0 and 1.")
  }

  meta <- .islh_surv_meta(data)
  date_name <- .islh_surv_select(data, rlang::enquo(date), "date", 1L, 1L)
  value_name <- .islh_surv_select(data, rlang::enquo(value), "value", 1L, 1L)
  by_names <- .islh_surv_select(data, rlang::enquo(by), "by")
  .islh_surv_check_reserved(
    by_names,
    c(
      "period_start",
      "observed",
      "expected",
      "upper_limit",
      "alarm",
      "pvalue",
      "status"
    )
  )

  work <- as.data.frame(data)
  work$.islh_date <- .islh_surv_as_date(
    work[[date_name]],
    date_name,
    timezone = timezone
  )
  work$.islh_value <- .islh_check_counts(
    work[[value_name]],
    value_name,
    allow_na = FALSE
  )
  # The method only takes weekly counts. A table without metadata is read as
  # one row per week, and the spacing check below confirms the dates are
  # seven days apart.
  interval <- if (is.null(meta)) "week" else meta$interval
  if (
    (meta$periods %||% 1L) != 1L ||
      !interval %in% c("week", "isoweek", "epiweek")
  ) {
    .islh_abort(c(
      "{.arg data} must hold weekly counts.",
      i = "The Farrington method here is set up for 52-week years."
    ))
  }
  # A partial week distorts the model's history as much as the week assessed.
  .islh_surv_check_partial(work)
  if (anyDuplicated(work[c(by_names, ".islh_date")])) {
    .islh_abort(c(
      "{.arg data} has more than one row per group and week.",
      i = "Aggregate it with {.fn islh_count_events} first."
    ))
  }

  weeks <- seq.Date(
    min(work$.islh_date),
    max(work$.islh_date),
    by = "week"
  )
  if (!all(work$.islh_date %in% weeks)) {
    .islh_abort("Week start dates in {.arg data} are not 7 days apart.")
  }
  from <- .islh_surv_as_date(from, "from", scalar = TRUE, timezone = timezone)
  to <- .islh_surv_as_date(to, "to", scalar = TRUE, timezone = timezone)
  if (!from %in% weeks || !to %in% weeks || from > to) {
    .islh_abort(c(
      "{.arg from} and {.arg to} must be week start dates in {.arg data},
       with {.arg from} first.",
      i = "{.arg data} runs from {format(min(weeks))} to
           {format(max(weeks))}."
    ))
  }
  needed <- lubridate::add_with_rollback(from, lubridate::years(-years_back)) -
    7L * window
  if (min(weeks) > needed) {
    .islh_abort(c(
      "{.arg data} does not go back far enough.",
      x = "Assessing {format(from)} with {years_back} year{?s} back and
           {window} week{?s} either side needs data from {format(needed)}.",
      i = "{.arg data} starts on {format(min(weeks))}. Supply more history
           or lower {.arg years_back}."
    ))
  }

  groups <- if (length(by_names) > 0L) {
    unique(work[by_names])
  } else {
    data.frame(.islh_all = 1L)
  }
  ids <- .islh_key_ids(list(work, groups), by_names)
  work_key <- ids[[1]]
  group_key <- ids[[2]]
  observed <- vapply(
    group_key,
    function(g) {
      rows <- work[work_key == g, , drop = FALSE]
      rows$.islh_value[match(weeks, rows$.islh_date)]
    },
    numeric(length(weeks))
  )
  observed <- matrix(observed, nrow = length(weeks))
  gaps <- colSums(is.na(observed))
  if (any(gaps > 0L)) {
    .islh_abort(c(
      "{sum(gaps > 0L)} group{?s} {?is/are} missing weeks.",
      i = "Supply every week for every group, including zeros. Complete the
           table with {.fn islh_count_events}."
    ))
  }

  counts <- surveillance::sts(
    observed = observed,
    epoch = as.numeric(weeks),
    epochAsDate = TRUE,
    frequency = 52,
    start = c(lubridate::isoyear(weeks[1]), lubridate::isoweek(weeks[1]))
  )
  range <- which(weeks >= from & weeks <= to)
  if (min(range) < low_count_weeks) {
    .islh_abort(c(
      "{.arg data} does not go back far enough for the low-count rule.",
      i = "The first week assessed needs {low_count_weeks} week{?s} of
           counts up to and including it."
    ))
  }
  fit <- surveillance::farringtonFlexible(
    counts,
    control = list(
      range = range,
      limit54 = c(as.integer(low_count), as.integer(low_count_weeks)),
      b = as.integer(years_back),
      w = as.integer(window),
      reweight = TRUE,
      weightsThreshold = 2.58,
      alpha = alpha,
      trend = TRUE,
      pThresholdTrend = 1,
      noPeriods = as.integer(no_periods),
      pastWeeksNotIncluded = as.integer(past_weeks_excluded),
      thresholdMethod = threshold_method,
      glmWarnings = FALSE
    )
  )

  n_weeks <- length(range)
  alarm <- surveillance::alarms(fit)
  storage.mode(alarm) <- "logical"
  out <- data.frame(
    .islh_group = rep(seq_along(group_key), each = n_weeks),
    period_start = rep(weeks[range], times = length(group_key)),
    observed = as.vector(surveillance::observed(fit)),
    expected = as.vector(fit@control$expected),
    upper_limit = as.vector(surveillance::upperbound(fit)),
    alarm = as.vector(alarm),
    pvalue = as.vector(fit@control$pvalue)
  )
  # Recompute the method's low-count rule, so a week it set aside is told
  # apart from one where the model failed.
  enough <- as.vector(vapply(
    seq_along(group_key),
    function(g) {
      vapply(
        range,
        function(k) {
          sum(observed[(k - low_count_weeks + 1L):k, g]) >= low_count
        },
        logical(1)
      )
    },
    logical(n_weeks)
  ))
  out$status <- ifelse(
    !is.na(out$upper_limit),
    "assessed",
    ifelse(enough, "fit_failed", "low_count_rule")
  )
  out$alarm[out$status != "assessed"] <- NA
  if (length(by_names) > 0L) {
    labels <- groups[out$.islh_group, by_names, drop = FALSE]
    rownames(labels) <- NULL
    out <- cbind(labels, out)
  }
  out$.islh_group <- NULL
  rownames(out) <- NULL
  attr(out, "islh_farrington") <- list(
    years_back = as.integer(years_back),
    window = as.integer(window),
    alpha = alpha,
    no_periods = as.integer(no_periods),
    past_weeks_excluded = as.integer(past_weeks_excluded),
    threshold_method = threshold_method,
    low_count = as.integer(low_count),
    low_count_weeks = as.integer(low_count_weeks),
    reweight_threshold = 2.58,
    trend = TRUE,
    surveillance_version = as.character(utils::packageVersion("surveillance"))
  )
  out
}
