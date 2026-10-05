# Rates and confidence intervals.
#
# The methods here are the published ones named in each function's docs. They
# are defensible defaults, not Island Health policy: confirm with PHASE that
# they match the standard your release is governed by.

# Below this count Byar's approximation drifts far enough to matter, so
# `method = "auto"` uses the exact interval instead. The cutoff is the one
# named in the Byar documentation: the approximation is accurate for counts of
# roughly 10 or more.
.islh_byar_minimum <- 10

#' Confidence interval for a Poisson count
#'
#' `"auto"` is the default. It uses the exact interval for counts below 10
#' and Byar's approximation at or above 10, which is the choice most
#' analysts would make by hand.
#'
#' `"byar"` uses Byar's approximation, the method Statistics Canada and most
#' cancer registries use for rate intervals. It is accurate for counts of
#' roughly 10 or more.
#'
#' `"exact"` inverts the Poisson distribution through the chi-squared
#' relationship (the Garwood interval). Use it for small counts, where Byar's
#' approximation drifts.
#'
#' Byar's approximation is undefined at zero, so a count of zero uses the exact
#' interval whichever method you ask for.
#'
#' @section Choosing a method by hand:
#'
#' Pick `"byar"` or `"exact"` explicitly when a release has to match a
#' published series that used one method throughout. `"auto"` mixes the two
#' within a single column. This convention makes the
#' method a property of each row rather than of the table. Say which method you
#' used in the table notes either way.
#'
#' @param x Observed counts.
#' @param conf Confidence level.
#' @param method `"auto"`, `"byar"` or `"exact"`.
#'
#' @return A data frame with columns `x`, `lower`, `upper` and `method`, where
#'   `method` records the method actually used for that count.
#' @export
#'
#' @references
#' Breslow NE, Day NE (1987). *Statistical Methods in Cancer Research,
#' Volume II*. IARC Scientific Publications No. 82, section 2.3.
#'
#' Garwood F (1936). Fiducial limits for the Poisson distribution.
#' *Biometrika* 28(3-4):437-442.
#'
#' @examples
#' islh_ci_poisson(c(0, 3, 25, 100))
#'
#' # Small counts are where the two methods disagree.
#' islh_ci_poisson(3, method = "exact")
#' islh_ci_poisson(3, method = "byar")
islh_ci_poisson <- function(
  x,
  conf = 0.95,
  method = c("auto", "byar", "exact")
) {
  method <- match.arg(method)
  conf <- .islh_check_conf(conf)
  x <- .islh_check_counts(x, arg = "x")

  alpha <- 1 - conf

  exact_lower <- function(x) {
    ifelse(x == 0, 0, stats::qchisq(alpha / 2, 2 * x) / 2)
  }
  exact_upper <- function(x) {
    stats::qchisq(1 - alpha / 2, 2 * (x + 1)) / 2
  }

  # Which method applies to each count? Byar's is undefined at zero, so a zero
  # always takes the exact interval whichever method was asked for.
  used <- rep(method, length(x))
  if (method == "auto") {
    used <- ifelse(x < .islh_byar_minimum, "exact", "byar")
  }
  used[!is.na(x) & x == 0] <- "exact"
  used[is.na(x)] <- NA_character_

  lower <- upper <- rep(NA_real_, length(x))
  exact <- !is.na(used) & used == "exact"
  lower[exact] <- exact_lower(x[exact])
  upper[exact] <- exact_upper(x[exact])

  byar <- !is.na(used) & used == "byar"
  if (any(byar)) {
    z <- stats::qnorm(1 - alpha / 2)
    b <- x[byar]
    # Byar's approximation, via the cube-root (Wilson-Hilferty) transformation
    # of the chi-squared distribution.
    lower[byar] <- pmax(b * (1 - 1 / (9 * b) - z / (3 * sqrt(b)))^3, 0)
    upper[byar] <- (b + 1) * (1 - 1 / (9 * (b + 1)) + z / (3 * sqrt(b + 1)))^3
  }

  data.frame(x = x, lower = lower, upper = upper, method = used)
}

#' Crude rate with a confidence interval
#'
#' Divides event counts by a population or person-time denominator and scales
#' to `per`. The interval comes from [islh_ci_poisson()] on the event count,
#' scaled the same way. Counts may exceed the denominator when people can
#' experience more than one event.
#'
#' @section Zero denominators:
#'
#' A zero denominator is refused rather than divided by. Nobody is at risk in
#' such a stratum, so it has no rate at all, and dividing by it would put `Inf`
#' or `NaN` into a published table.
#'
#' Zero denominators are real values and reach you legitimately: a small local
#' health area can have an age band with nobody in it, and
#' [islh_bc_population()] keeps those rows rather than dropping them. Decide
#' what they mean before calculating. Usually you either drop those strata,
#' which reports no rate where no rate exists, or combine them with a
#' neighbouring age band or area, which reports a rate over a denominator large
#' enough to carry one. Say which you did in the table notes.
#'
#' @param cases Non-negative whole event counts.
#' @param population Population or person-time at risk. Recycled if length 1.
#'   Must be positive; see the zero-denominator section.
#' @param per Rate denominator. 100,000 by convention in public health.
#' @param conf Confidence level.
#' @param method Interval method passed to [islh_ci_poisson()].
#'
#' @return A data frame with columns `cases`, `population`, `rate`, `lower`,
#'   `upper`, `method`, `conf` and `per`. `conf` and `per` repeat the settings
#'   on every row, as in [islh_dsr()], so a saved table says how it was
#'   calculated.
#' @export
#'
#' @examples
#' islh_crude_rate(cases = c(12, 45), population = c(50000, 120000))
islh_crude_rate <- function(
  cases,
  population,
  per = 100000,
  conf = 0.95,
  method = c("auto", "byar", "exact")
) {
  method <- match.arg(method)
  conf <- .islh_check_conf(conf)
  per <- .islh_check_scalar_positive(per, "per")
  cases <- .islh_check_counts(cases, arg = "cases")

  if (length(population) == 1L) {
    population <- rep(population, length(cases))
  }
  if (length(population) != length(cases)) {
    .islh_abort(
      "{.arg population} must be length 1 or the same length as {.arg cases}."
    )
  }
  population <- .islh_check_population(population)

  ci <- islh_ci_poisson(cases, conf = conf, method = method)
  scale <- per / population

  data.frame(
    cases = cases,
    population = population,
    rate = cases * scale,
    lower = ci$lower * scale,
    upper = ci$upper * scale,
    method = ci$method,
    conf = conf,
    per = per
  )
}

#' Directly standardized rate
#'
#' Weights stratum-specific rates by a standard population, so rates from
#' populations with different age structures can be compared.
#'
#' The default `"gamma"` interval is Fay and Feuer's method. It keeps its
#' generally conservative coverage under independent Poisson counts, including
#' the usual situation with age-standardized rates and small counts, where a
#' normal-approximation interval is too narrow and can fall below zero.
#' `"normal"` is provided for comparison with published figures that used it.
#'
#' Fay and Feuer's interval is exact whenever the standard population is
#' proportional to the study population: with those weights it reduces to the
#' exact Poisson interval on the pooled count. A regression test checks that
#' property across several strata.
#'
#' It is conservative by design: its coverage is usually above the nominal
#' level, more so when one small stratum has a large weight. Other tools use
#' other methods. The Fingertips profiles and the APHO and OHID calculators
#' use Dobson's method (Eayres 2008), so their intervals will not match these
#' exactly. Tiwari, Clegg and Zou's modified gamma interval and Fay and Kim's
#' mid-p gamma interval come closer to nominal coverage; neither is provided.
#'
#' To compare two standardized rates, use [islh_dsr_ratio()]. Comparing their
#' total cases and populations compares the crude rates instead.
#'
#' @section Zero denominators:
#'
#' A stratum with a zero denominator is refused, for the reason given in the
#' zero-denominator section of [islh_crude_rate()]. A standardized rate is one
#' number summed over every stratum, so decide what an empty stratum means and
#' use a common, documented pooling or restriction scheme across comparisons.
#' Dropping a positively weighted stratum and renormalizing changes the
#' estimand; do not make area-specific exclusions.
#'
#' A zero *standard* population is allowed. It gives the stratum a weight of
#' zero, which is how a standard population legitimately excludes an age band.
#'
#' Intervals assume independent Poisson counts and fixed denominators.
#' Recurrent events may be clustered; accepting these counts does not establish
#' that Poisson uncertainty is appropriate. Vector inputs must share stratum
#' order; use [islh_dsr_joined()] to validate alignment by keys.
#'
#' @param standard_id Identifier and vintage of the standard population.
#' @param strata Stratum labels in vector order, retained in the result.
#' @param cases Non-negative whole event counts per stratum. Counts may exceed
#'   the corresponding denominator when events can recur.
#' @param population Population or person-time at risk per stratum. Must be
#'   positive; see the zero-denominator section.
#' @param std_population Standard population per stratum. Only the relative
#'   sizes matter; they are normalized to weights internally.
#' @param per Rate denominator.
#' @param conf Confidence level.
#' @param method `"gamma"` for Fay-Feuer, `"normal"` for the normal
#'   approximation.
#'
#' @return A one-row data frame with columns `cases`, `population`, `rate`,
#'   `lower`, `upper`, `method`, `conf`, `per`, `standard_id` and a list
#'   column `strata`. The stratum-level calculation is attached; read it with
#'   [islh_dsr_detail()].
#' @export
#'
#' @references
#' Fay MP, Feuer EJ (1997). Confidence intervals for directly standardized
#' rates: a method based on the gamma distribution.
#' *Statistics in Medicine* 16(7):791-801.
#'
#' Eayres D (2008). *Technical Briefing 3: Commonly used public health
#' statistics and their confidence intervals*. Association of Public Health
#' Observatories.
#'
#' Tiwari RC, Clegg LX, Zou Z (2006). Efficient interval estimation for
#' age-adjusted cancer rates. *Statistical Methods in Medical Research*
#' 15(6):547-569.
#'
#' Fay MP, Kim S (2017). Confidence intervals for directly standardized rates
#' using mid-p gamma intervals. *Biometrical Journal* 59(2):377-387.
#'
#' @examples
#' cases <- c(5, 12, 40, 80)
#' population <- c(20000, 25000, 22000, 15000)
#' standard <- c(30000, 30000, 25000, 15000)
#'
#' islh_dsr(cases, population, standard)
#'
#' # Compare with the crude rate: standardizing removes the effect of this
#' # population being older than the standard.
#' islh_crude_rate(sum(cases), sum(population))
#'
#' # One standardized rate per area, from a table with a row for each area
#' # and age group.
#' library(dplyr)
#'
#' strata <- tibble(
#'   area = rep(c("North", "South"), each = 4),
#'   cases = c(5, 12, 40, 80, 9, 31, 52, 70),
#'   population = c(20000, 25000, 22000, 15000, 41000, 52000, 38000, 21000),
#'   standard = rep(c(30000, 30000, 25000, 15000), 2)
#' )
#'
#' strata |>
#'   summarise(
#'     islh_dsr(cases, population, standard) |>
#'       select(cases, population, rate, lower, upper),
#'     .by = area
#'   )
islh_dsr <- function(
  cases,
  population,
  std_population,
  per = 100000,
  conf = 0.95,
  method = c("gamma", "normal"),
  standard_id = NA_character_,
  strata = seq_along(cases)
) {
  method <- match.arg(method)
  conf <- .islh_check_conf(conf)
  per <- .islh_check_scalar_positive(per, "per")

  n <- length(cases)
  if (n == 0L) {
    .islh_abort("{.arg cases} must have at least one stratum.")
  }
  if (length(population) != n || length(std_population) != n) {
    .islh_abort(c(
      "{.arg cases}, {.arg population} and {.arg std_population} must be the
       same length (one entry per stratum).",
      x = "Lengths are {n}, {length(population)} and {length(std_population)}."
    ))
  }

  # A standardized rate is a single number summed over every stratum, so one
  # missing stratum makes the whole result missing. Refuse rather than return
  # an NA that looks like a computed answer.
  cases <- .islh_check_counts(cases, arg = "cases", allow_na = FALSE)
  population <- .islh_check_population(population)
  std_population <- .islh_check_population(
    std_population,
    "std_population",
    allow_zero = TRUE
  )

  if (!is.character(standard_id) || length(standard_id) != 1L) {
    .islh_abort("{.arg standard_id} must be one character identifier or NA.")
  }
  if (length(strata) != n || anyNA(strata) || anyDuplicated(strata)) {
    .islh_abort(
      "{.arg strata} must contain one unique non-missing label per stratum."
    )
  }
  total_standard <- sum(std_population)
  if (total_standard <= 0) {
    .islh_abort(c(
      "{.arg std_population} must have a positive total.",
      x = "Every stratum is zero, so there are no weights to standardize by."
    ))
  }

  weights <- std_population / total_standard
  rate <- sum(weights * cases / population)
  variance <- sum(weights^2 * cases / population^2)

  alpha <- 1 - conf

  if (method == "normal") {
    z <- stats::qnorm(1 - alpha / 2)
    lower <- rate - z * sqrt(variance)
    upper <- rate + z * sqrt(variance)
  } else {
    # Fay-Feuer. The gamma interval matches the first two moments of the
    # weighted sum of Poisson counts, so it behaves when one stratum's weight
    # dominates. `w_max` enters the upper limit and keeps it conservative.
    w_max <- max(weights / population)

    lower <- if (rate == 0) {
      0
    } else {
      (variance / (2 * rate)) *
        stats::qchisq(alpha / 2, 2 * rate^2 / variance)
    }
    upper <- ((variance + w_max^2) / (2 * (rate + w_max))) *
      stats::qchisq(
        1 - alpha / 2,
        2 * (rate + w_max)^2 / (variance + w_max^2)
      )
  }

  out <- data.frame(
    cases = sum(cases),
    population = sum(population),
    rate = rate * per,
    lower = max(lower, 0) * per,
    upper = upper * per,
    method = method,
    conf = conf,
    per = per,
    standard_id = standard_id,
    strata = I(list(as.character(strata)))
  )
  stratum_variance <- weights^2 * cases / population^2
  attr(out, "islh_dsr_strata") <- data.frame(
    stratum = as.character(strata),
    cases = cases,
    population = population,
    weight = weights,
    rate = cases / population * per,
    contribution = weights * cases / population * per,
    variance_share = if (variance > 0) {
      stratum_variance / variance
    } else {
      rep(NA_real_, n)
    },
    stringsAsFactors = FALSE
  )
  out
}

#' Stratum-level detail of a standardized rate
#'
#' Shows how each stratum feeds a rate from [islh_dsr()] or
#' [islh_dsr_joined()]: its weight, its own rate, what it adds to the
#' standardized rate and how much of the variance it carries.
#'
#' Use it to see why an interval is wide. A standardized rate for a small area
#' is often driven by one or two strata with few cases and a large weight,
#' usually the oldest age bands. `variance_share` shows each stratum's share
#' of the estimated variance: a stratum with a share of 0.6 carries 60% of it.
#'
#' The variance is not the whole story. Fay and Feuer's upper limit also adds
#' the largest weight per person, `weight / population`, of any stratum. A
#' stratum with no cases contributes nothing to the estimated variance, so its
#' `variance_share` is 0, yet if it is a small population with a large weight
#' it can set that term and widen the upper limit a great deal. Look at
#' `weight` and `population` as well as `variance_share`.
#'
#' When a few strata dominate, a longer period or a larger area helps.
#' Broader age bands also narrow the interval, but they leave more of the age
#' difference unadjusted inside each band; see the getting-started guide.
#' Apply any such choice the same way to every rate being compared.
#'
#' The detail holds every stratum's cases and population, so treat it as
#' internal. [islh_suppress_table()] removes it, and this function then stops
#' rather than return detail that no longer matches what is visible.
#'
#' @param x A result from [islh_dsr()] or [islh_dsr_joined()].
#'
#' @return A data frame with one row per stratum. [islh_dsr()] gives a
#'   `stratum` column holding the `strata` labels; [islh_dsr_joined()] gives
#'   its key columns instead. Then `cases`, `population`, `weight` (summing to
#'   1), `rate` and `contribution` (both on the `per` scale; contributions sum
#'   to the standardized rate) and `variance_share` (summing to 1, or `NA`
#'   when there are no cases).
#' @export
#'
#' @examples
#' dsr <- islh_dsr(
#'   cases = c(5, 12, 40, 80),
#'   population = c(20000, 25000, 22000, 15000),
#'   std_population = c(30000, 30000, 25000, 15000),
#'   strata = c("0-19", "20-44", "45-64", "65+")
#' )
#' detail <- islh_dsr_detail(dsr)
#' detail
#'
#' # The contributions add up to the standardized rate.
#' sum(detail$contribution)
#' dsr$rate
islh_dsr_detail <- function(x) {
  if (isTRUE(attr(x, "islh_detail_removed", exact = TRUE))) {
    .islh_abort(c(
      "{.arg x} has been through {.fn islh_suppress_table}.",
      x = "Its stratum detail was removed, because the stratum counts could
           reveal what suppression hid.",
      i = "Read the detail from the result before suppressing it."
    ))
  }
  detail <- attr(x, "islh_dsr_strata", exact = TRUE)
  if (!is.data.frame(x) || is.null(detail)) {
    .islh_abort(c(
      "{.arg x} carries no stratum detail.",
      i = "Pass the result of {.fn islh_dsr} or {.fn islh_dsr_joined} before
           reshaping it."
    ))
  }
  detail
}
