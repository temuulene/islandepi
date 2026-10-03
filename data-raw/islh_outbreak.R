# Builds data/islh_outbreak.rda, the simulated line list used by the examples
# and articles.
#
# The cases come from simulist, which simulates an outbreak as a branching
# process, so onset dates, delays, ages and outcomes hang together the way
# they do in real surveillance data. Staff never need simulist: the package
# ships the result, and this script is only run to rebuild it.
#
# Everything here is simulated. No record describes a real person, and the
# case names simulist generates are dropped.
#
# Run with: Rscript data-raw/islh_outbreak.R
# Needs simulist (>= 0.7.0) and islandbrand (for the local health area
# populations).

seed <- 20251103L
start_date <- as.Date("2025-11-03")

# A seasonal respiratory virus. Each case has a Poisson number of contacts and
# infects each with probability 0.4, for a reproduction number a little
# above 1: a winter wave that rises over a few weeks and then fades.
contact_distribution <- function(x) stats::dpois(x = x, lambda = 2.6)
infectious_period <- function(n) stats::rlnorm(n, meanlog = 1.2, sdlog = 0.4)
onset_to_hosp <- function(n) stats::rlnorm(n, meanlog = 1.2, sdlog = 0.5)
onset_to_death <- function(n) stats::rlnorm(n, meanlog = 2.4, sdlog = 0.5)
onset_to_recovery <- function(n) stats::rlnorm(n, meanlog = 2, sdlog = 0.3)
reporting_delay <- function(n) stats::rlnorm(n, meanlog = 0.9, sdlog = 0.6)

# Hospitalization and death risk rise steeply after 65, as for influenza.
hosp_risk <- data.frame(
  age_limit = c(1, 5, 20, 65, 80),
  risk = c(0.06, 0.02, 0.02, 0.1, 0.25)
)
hosp_death_risk <- data.frame(
  age_limit = c(1, 65, 80),
  risk = c(0.02, 0.06, 0.14)
)

# Roughly Island Health's age structure: older than BC as a whole. simulist
# reads the last limit as the oldest age, so 95 closes the 80-94 band and
# carries no weight of its own.
population_age <- data.frame(
  age_limit = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 95),
  proportion = c(0.08, 0.09, 0.11, 0.12, 0.12, 0.12, 0.14, 0.14, 0.08, 0)
)

set.seed(seed)
cases <- simulist::sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.4,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  onset_to_recovery = onset_to_recovery,
  reporting_delay = reporting_delay,
  hosp_risk = hosp_risk,
  hosp_death_risk = hosp_death_risk,
  non_hosp_death_risk = 0.001,
  outbreak_start_date = start_date,
  outbreak_size = c(600, 1200),
  population_age = population_age,
  case_type_probs = c(suspected = 0.1, probable = 0.2, confirmed = 0.7)
)

# Place each case in a local health area, in proportion to its population.
lha <- sf::st_drop_geometry(islandbrand::islh_example_lha())
lha <- lha[order(lha$geography_code), ]
area <- sample(
  seq_len(nrow(lha)),
  size = nrow(cases),
  replace = TRUE,
  prob = lha$population
)

# simulist keeps fractions of a day in its dates. A line list records whole
# calendar days, so drop the fractions before anything is compared.
whole_day <- function(x) as.Date(floor(as.numeric(x)))
for (field in c("date_onset", "date_reporting", "date_admission", "date_outcome")) {
  cases[[field]] <- whole_day(cases[[field]])
}

cases <- cases[order(cases$date_onset, cases$date_reporting), ]
out <- data.frame(
  case_id = sprintf("C%04d", seq_len(nrow(cases))),
  case_type = tools::toTitleCase(cases$case_type),
  sex = c(f = "Female", m = "Male")[cases$sex],
  age = as.integer(cases$age),
  date_onset = cases$date_onset,
  date_reported = cases$date_reporting,
  date_admission = cases$date_admission,
  outcome = tools::toTitleCase(cases$outcome),
  date_outcome = cases$date_outcome,
  hsda = lha$hsda[area],
  lha_code = lha$geography_code[area],
  lha_name = lha$geography_name[area],
  stringsAsFactors = FALSE
)
rownames(out) <- NULL

stopifnot(
  !anyDuplicated(out$case_id),
  !anyNA(out[c("case_type", "sex", "age", "date_onset", "hsda", "lha_code")]),
  all(out$date_reported >= out$date_onset),
  all(out$date_admission >= out$date_onset, na.rm = TRUE),
  all(out$date_outcome >= out$date_onset, na.rm = TRUE)
)

islh_outbreak <- out
save(islh_outbreak, file = "data/islh_outbreak.rda", compress = "xz")
cat(
  "wrote data/islh_outbreak.rda:",
  nrow(islh_outbreak),
  "cases,",
  format(min(out$date_onset)),
  "to",
  format(max(out$date_onset)),
  "\n"
)
