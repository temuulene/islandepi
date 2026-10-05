#' Ratio of two crude rates with an exact interval
#'
#' Compares two crude rates, or two counts, as a ratio with a confidence
#' interval. Use it for a change between two periods or a difference between
#' two areas, when the counts are crude and independent.
#'
#' The interval is the exact conditional one. Given the total number of cases,
#' the share falling in the first group is binomial, so the Clopper-Pearson
#' interval for that share converts to an interval for the ratio. It is the
#' interval [stats::poisson.test()] gives for two counts, vectorized so it
#' works inside [dplyr::mutate()].
#'
#' @section Assumptions:
#'
#' Each count is Poisson and the two are independent, and the denominators
#' are fixed. The interval then describes the uncertainty in the ratio of the
#' underlying reported-event rates under that model. Clustered cases, such as
#' an outbreak in one facility, or recurrent events in the same people, vary
#' more than Poisson counts do, and the interval is then too narrow.
#'
#' The ratio compares crude rates. When the two populations differ in age
#' structure, compare standardized rates with [islh_dsr_ratio()] instead.
#'
#' A reference with no cases gives a missing ratio and interval: a rise from
#' zero has no size as a ratio. Report the counts.
#'
#' @param cases,population Cases and population or person-time for the group
#'   being compared.
#' @param reference_cases,reference_population Cases and population or
#'   person-time for the reference. Each argument may be length 1 or the
#'   common length.
#' @param conf Confidence level.
#'
#' @return A data frame with one row per comparison: `cases`, `population`,
#'   `reference_cases`, `reference_population`, `ratio`, `lower`, `upper`,
#'   `conf` and `method`.
#' @export
#'
#' @references
#' Breslow NE, Day NE (1987). *Statistical Methods in Cancer Research, Volume
#' II: The Design and Analysis of Cohort Studies*. IARC Scientific
#' Publications No. 82. Lyon: International Agency for Research on Cancer.
#'
#' Clopper CJ, Pearson ES (1934). The use of confidence or fiducial limits
#' illustrated in the case of the binomial. *Biometrika* 26(4):404-413.
#'
#' @examples
#' # 22 cases this winter against 14 last winter, in the same population.
#' islh_rate_ratio(22, 69000, 14, 69000)
#'
#' # The same interval as stats::poisson.test().
#' stats::poisson.test(c(22, 14), c(69000, 69000))$conf.int
#'
#' # One comparison per row of a table.
#' library(dplyr)
#'
#' winters <- tibble(
#'   area = c("Comox Valley", "Greater Nanaimo"),
#'   cases = c(22, 205),
#'   last_year = c(14, 160),
#'   population = c(80091, 133395)
#' )
#'
#' winters |>
#'   mutate(
#'     islh_rate_ratio(cases, population, last_year, population) |>
#'       select(ratio, lower, upper)
#'   )
islh_rate_ratio <- function(
  cases,
  population,
  reference_cases,
  reference_population,
  conf = 0.95
) {
  conf <- .islh_check_conf(conf)
  inputs <- list(
    cases = cases,
    population = population,
    reference_cases = reference_cases,
    reference_population = reference_population
  )
  lengths <- lengths(inputs)
  n <- max(lengths)
  if (any(lengths == 0L) || any(!lengths %in% c(1L, n))) {
    .islh_abort(c(
      "The counts and populations must have length 1 or a common length.",
      x = "Lengths are {lengths}."
    ))
  }
  cases <- rep(
    .islh_check_counts(cases, "cases", allow_na = FALSE),
    length.out = n
  )
  reference_cases <- rep(
    .islh_check_counts(reference_cases, "reference_cases", allow_na = FALSE),
    length.out = n
  )
  population <- rep(.islh_check_population(population), length.out = n)
  reference_population <- rep(
    .islh_check_population(reference_population, "reference_population"),
    length.out = n
  )

  limits <- .islh_poisson_ratio_limits(
    cases,
    reference_cases,
    exposure_ratio = reference_population / population,
    conf = conf
  )
  data.frame(
    cases = cases,
    population = population,
    reference_cases = reference_cases,
    reference_population = reference_population,
    ratio = limits$ratio,
    lower = limits$lower,
    upper = limits$upper,
    conf = conf,
    method = "exact"
  )
}

# Exact conditional limits for the ratio of two Poisson rates. Given
# n = x + y, x is binomial with p = r / (r + 1 / exposure_ratio), so the
# Clopper-Pearson limits for p convert to limits for the rate ratio r.
# `exposure_ratio` is the reference's person-time over the group's.
.islh_poisson_ratio_limits <- function(x, y, exposure_ratio = 1, conf = 0.95) {
  alpha <- 1 - conf
  usable <- y > 0
  p_lower <- ifelse(
    x == 0,
    0,
    stats::qbeta(alpha / 2, pmax(x, 1), y + 1)
  )
  p_upper <- ifelse(
    usable,
    stats::qbeta(1 - alpha / 2, x + 1, pmax(y, 1)),
    1
  )
  to_ratio <- function(p) p / (1 - p) * exposure_ratio
  list(
    ratio = ifelse(usable, x / pmax(y, 1) * exposure_ratio, NA_real_),
    lower = ifelse(usable, to_ratio(p_lower), NA_real_),
    upper = ifelse(usable, to_ratio(p_upper), NA_real_)
  )
}

#' Ratio of two directly standardized rates
#'
#' Compares two rates from [islh_dsr()] or [islh_dsr_joined()] as a ratio with
#' a confidence interval designed for standardized rates.
#'
#' A directly standardized rate is a weighted sum of stratum-specific rates,
#' so its uncertainty cannot be recovered from the total cases and total
#' population. Comparing those totals with [islh_rate_ratio()] or
#' [stats::poisson.test()] compares the crude rates instead. This function
#' reads each rate's stratum detail.
#'
#' The interval is Fay's F interval. It treats each standardized rate as
#' gamma-distributed with the mean and variance of its weighted sum of Poisson
#' counts, as the Fay and Feuer interval in [islh_dsr()] does, so the ratio of
#' the two follows an F distribution. Like that interval it is conservative:
#' each limit takes the less favourable of the two forms, adding the largest
#' stratum weight where Fay and Feuer add it. With one stratum in each rate it
#' is exactly the interval [islh_rate_ratio()] gives.
#'
#' @section Assumptions:
#'
#' Both rates use the same standard population, so they are comparable, and
#' their counts are independent Poisson counts with fixed denominators. Rates
#' for different areas, or one area in two separate periods, satisfy that. A
#' rate for an area and a rate for a region that contains it share cases, so
#' they are not independent; that comparison needs a method for overlapping
#' rates (Tiwari et al. 2006), which this function does not provide.
#'
#' A reference rate of zero gives a missing ratio and interval.
#'
#' @param x A standardized rate from [islh_dsr()] or [islh_dsr_joined()].
#' @param reference The rate `x` is compared with, built with the same
#'   standard population.
#' @param conf Confidence level.
#'
#' @return A one-row data frame with `rate`, `reference_rate`, `ratio`,
#'   `lower`, `upper`, `conf`, `method` and `standard_id`. The rates are on
#'   the `per` scale of the inputs.
#' @export
#'
#' @references
#' Fay MP (1999). Approximate confidence intervals for rate ratios from
#' directly standardized rates with sparse data. *Communications in
#' Statistics - Theory and Methods* 28(9):2141-2160.
#'
#' Fay MP, Feuer EJ (1997). Confidence intervals for directly standardized
#' rates: a method based on the gamma distribution. *Statistics in Medicine*
#' 16(7):791-801.
#'
#' Tiwari RC, Clegg LX, Zou Z (2006). Efficient interval estimation for
#' age-adjusted cancer rates. *Statistical Methods in Medical Research*
#' 15(6):547-569.
#'
#' @examples
#' standard <- islh_standard_population("canada_2011", age_breaks = c(0, 20, 65))
#'
#' north <- islh_dsr(
#'   cases = c(2, 9, 14),
#'   population = c(26000, 79000, 38000),
#'   std_population = standard$population,
#'   standard_id = standard$standard_id[1],
#'   strata = standard$age_group
#' )
#' south <- islh_dsr(
#'   cases = c(6, 25, 31),
#'   population = c(82000, 268000, 114000),
#'   std_population = standard$population,
#'   standard_id = standard$standard_id[1],
#'   strata = standard$age_group
#' )
#'
#' islh_dsr_ratio(north, south)
islh_dsr_ratio <- function(x, reference, conf = 0.95) {
  conf <- .islh_check_conf(conf)
  one <- .islh_dsr_parts(x, "x")
  two <- .islh_dsr_parts(reference, "reference")

  if (
    length(one$weight) != length(two$weight) ||
      !isTRUE(all.equal(sort(one$weight), sort(two$weight)))
  ) {
    .islh_abort(c(
      "{.arg x} and {.arg reference} use different standard weights.",
      i = "Standardize both to the same standard population and age groups
           before comparing them."
    ))
  }
  if (
    !is.na(one$standard_id) &&
      !is.na(two$standard_id) &&
      !identical(one$standard_id, two$standard_id)
  ) {
    .islh_abort(c(
      "{.arg x} and {.arg reference} name different standards.",
      x = "{.val {one$standard_id}} and {.val {two$standard_id}}."
    ))
  }

  alpha <- 1 - conf
  ratio <- lower <- upper <- NA_real_
  if (two$rate > 0) {
    ratio <- one$rate / two$rate
    lower <- if (one$rate == 0) {
      0
    } else {
      one$rate /
        (two$rate + two$w_max) *
        stats::qf(alpha / 2, one$df, two$df_plus)
    }
    upper <- (one$rate + one$w_max) /
      two$rate *
      stats::qf(1 - alpha / 2, one$df_plus, two$df)
  } else {
    .islh_inform(c(
      "The reference rate is zero, so the ratio and its interval are
       {.code NA}.",
      i = "Report the two rates instead."
    ))
  }

  data.frame(
    rate = one$rate * one$per,
    reference_rate = two$rate * two$per,
    ratio = ratio,
    lower = lower,
    upper = upper,
    conf = conf,
    method = "Fay F",
    standard_id = if (is.na(one$standard_id)) {
      two$standard_id
    } else {
      one$standard_id
    }
  )
}

# What the F interval needs from a standardized rate, on the rate scale
# (cases per person, not per 100,000): the rate, the variance of the weighted
# sum, the largest stratum weight, and the two gamma degrees of freedom of
# Fay and Feuer's lower and upper limits.
.islh_dsr_parts <- function(x, arg, call = rlang::caller_env()) {
  if (isTRUE(attr(x, "islh_detail_removed", exact = TRUE))) {
    .islh_abort(
      c(
        "{.arg {arg}} has been through {.fn islh_suppress_table}.",
        i = "Compare the rates before suppressing them."
      ),
      call = call
    )
  }
  detail <- attr(x, "islh_dsr_strata", exact = TRUE)
  if (!is.data.frame(x) || nrow(x) != 1L || is.null(detail)) {
    .islh_abort(
      c(
        "{.arg {arg}} must be one rate from {.fn islh_dsr} or
         {.fn islh_dsr_joined}.",
        i = "It carries the stratum detail the interval needs."
      ),
      call = call
    )
  }
  w <- detail$weight / detail$population
  rate <- sum(w * detail$cases)
  variance <- sum(w^2 * detail$cases)
  w_max <- max(w)
  list(
    rate = rate,
    w_max = w_max,
    df = if (rate > 0) 2 * rate^2 / variance else 0,
    df_plus = 2 * (rate + w_max)^2 / (variance + w_max^2),
    weight = detail$weight,
    per = x$per,
    standard_id = x$standard_id
  )
}
