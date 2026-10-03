#' Proportion with a confidence interval
#'
#' Divides a count of people with an outcome by the number eligible, such as
#' immunized children over children due, or positive tests over tests
#' performed, and adds a binomial confidence interval.
#'
#' `"wilson"` is the default. It is the Wilson score interval without
#' continuity correction. It stays inside 0 to 1, behaves at 0 and at the
#' denominator, and its coverage stays close to the nominal level even for
#' small denominators, which the simple Wald interval does not.
#'
#' `"exact"` is the Clopper-Pearson interval. It guarantees at least nominal
#' coverage, so it is wider than it needs to be on average. Use it when a
#' release has to match a series that used it.
#'
#' @section What goes in the denominator:
#'
#' `n` is everyone eligible for the outcome, and `x` must be a subset of them.
#' Records with an unknown status are a decision, not a default: leaving them
#' out of both `x` and `n` assumes they look like the known records, while
#' leaving them in `n` alone counts every unknown as a "no". Say which you did
#' in the table notes.
#'
#' These intervals assume independent records from a simple random sample or a
#' complete count. They do not apply to survey-weighted prevalence, which needs
#' a method that accounts for the survey design.
#'
#' @param x Whole counts of records with the outcome.
#' @param n Whole counts of eligible records. Must be positive and at least `x`.
#'   `x` and `n` are recycled if either has length 1.
#' @param per Scale for the result: 1 for a proportion, 100 for a percentage.
#' @param conf Confidence level.
#' @param method `"wilson"` or `"exact"`.
#'
#' @return A data frame with columns `x`, `n`, `proportion`, `lower`, `upper`,
#'   `method`, `conf` and `per`. `proportion`, `lower` and `upper` are on the
#'   `per` scale. A missing `x` gives a missing estimate.
#' @export
#'
#' @references
#' Wilson EB (1927). Probable inference, the law of succession, and
#' statistical inference. *Journal of the American Statistical Association*
#' 22(158):209-212.
#'
#' Clopper CJ, Pearson ES (1934). The use of confidence or fiducial limits
#' illustrated in the case of the binomial. *Biometrika* 26(4):404-413.
#'
#' Newcombe RG (1998). Two-sided confidence intervals for the single
#' proportion: comparison of seven methods. *Statistics in Medicine*
#' 17(8):857-872.
#'
#' Brown LD, Cai TT, DasGupta A (2001). Interval estimation for a binomial
#' proportion. *Statistical Science* 16(2):101-133.
#'
#' @examples
#' # Share of simulated outbreak cases admitted to hospital, by HSDA.
#' admitted <- tapply(
#'   !is.na(islh_outbreak$date_admission),
#'   islh_outbreak$hsda,
#'   sum
#' )
#' cases <- table(islh_outbreak$hsda)
#'
#' islh_proportion(as.vector(admitted), as.vector(cases), per = 100)
#'
#' # Small denominators are where the two methods differ most.
#' islh_proportion(c(0, 1, 5), 20, method = "wilson")
#' islh_proportion(c(0, 1, 5), 20, method = "exact")
islh_proportion <- function(
  x,
  n,
  per = 1,
  conf = 0.95,
  method = c("wilson", "exact")
) {
  method <- match.arg(method)
  conf <- .islh_check_conf(conf)
  per <- .islh_check_scalar_positive(per, "per")
  x <- .islh_check_counts(x, arg = "x")
  n <- .islh_check_counts(n, arg = "n", allow_na = FALSE)

  size <- max(length(x), length(n))
  if (!length(x) %in% c(1L, size) || !length(n) %in% c(1L, size)) {
    .islh_abort(
      "{.arg x} and {.arg n} must be the same length, or one of them length 1."
    )
  }
  x <- rep_len(x, size)
  n <- rep_len(n, size)

  zero <- which(n == 0)
  if (length(zero) > 0L) {
    .islh_abort(c(
      "{.arg n} must be positive.",
      x = "{cli::qty(length(zero))}Position{?s} {.val {zero}} {?is/are} zero.",
      i = "Nobody is eligible there, so there is no proportion to report."
    ))
  }
  over <- which(!is.na(x) & x > n)
  if (length(over) > 0L) {
    .islh_abort(c(
      "{.arg x} must not be larger than {.arg n}.",
      x = "{cli::qty(length(over))}Position{?s} {.val {over}} {?has/have} more
           records with the outcome than eligible records.",
      i = "Check that {.arg x} counts a subset of the records in {.arg n}."
    ))
  }

  alpha <- 1 - conf
  p <- x / n

  if (method == "wilson") {
    z <- stats::qnorm(1 - alpha / 2)
    centre <- (x + z^2 / 2) / (n + z^2)
    half <- z * sqrt(x * (n - x) / n + z^2 / 4) / (n + z^2)
    lower <- pmax(centre - half, 0)
    upper <- pmin(centre + half, 1)
    # Rounding error can leave a limit a hair away from its exact value.
    lower[!is.na(x) & x == 0] <- 0
    upper[!is.na(x) & x == n] <- 1
  } else {
    lower <- ifelse(x == 0, 0, stats::qbeta(alpha / 2, x, n - x + 1))
    upper <- ifelse(x == n, 1, stats::qbeta(1 - alpha / 2, x + 1, n - x))
  }

  data.frame(
    x = x,
    n = n,
    proportion = p * per,
    lower = lower * per,
    upper = upper * per,
    method = method,
    conf = conf,
    per = per
  )
}
