# Builds three small simulated datasets for the difficult cases the examples
# teach:
#
# * islh_seasons: seven seasons of weekly counts by HSDA, for matched
#   seasonal baselines, including an unusually quiet 2020-21 season.
# * islh_encounters: four weeks of encounter records in which some people
#   come back on other days or to other sites, and a few records arrive twice.
# * islh_feed_log: the daily file log for the same four weeks. One site's
#   files are missing for two days; another site sent a nil report on a day
#   with no encounters.
#
# Everything here is simulated. No record describes a real person, site or
# season of activity.
#
# Run with: Rscript data-raw/islh_scenarios.R (from the package root).

devtools::load_all(quiet = TRUE)
set.seed(20260930L)

# islh_seasons --------------------------------------------------------------

hsdas <- c(
  "South Vancouver Island",
  "Central Vancouver Island",
  "North Vancouver Island"
)
# BC Stats 2025 populations, as in the getting-started guide, to scale counts.
share <- c(464081, 320319, 143305) / sum(c(464081, 320319, 143305))

weeks <- seq(as.Date("2019-09-02"), as.Date("2026-08-24"), by = "week")
season_of <- function(d) {
  year <- as.integer(format(d, "%Y"))
  month <- as.integer(format(d, "%m"))
  ifelse(month >= 9, year, year - 1L)
}
season <- season_of(weeks)
seasons <- sort(unique(season))

# Each season peaks between early December and early February, at its own
# height. 2020-21 barely rises, like a season disrupted by public health
# measures, so the examples can show why it should be excluded.
peak_week <- as.Date(sprintf("%d-12-01", seasons)) +
  sample(7 * (0:9), length(seasons), replace = TRUE)
peak_height <- stats::runif(length(seasons), 150, 260)
peak_height[seasons == 2020] <- 12

expected <- vapply(
  seq_along(weeks),
  function(i) {
    s <- match(season[i], seasons)
    distance <- as.numeric(weeks[i] - peak_week[s]) / 7
    8 + peak_height[s] * exp(-distance^2 / (2 * 4^2))
  },
  numeric(1)
)

grid <- expand.grid(week = seq_along(weeks), area = seq_along(hsdas))
mu <- expected[grid$week] * share[grid$area]
# Gamma-Poisson draws give the extra-Poisson variation of real counts.
counts <- stats::rpois(nrow(grid), stats::rgamma(nrow(grid), 10, 10 / mu))

raw <- data.frame(
  hsda = hsdas[grid$area],
  week = weeks[grid$week],
  count = counts
)
islh_seasons <- islh_count_events(
  raw,
  date = week,
  count = count,
  by = hsda,
  interval = "week",
  week_start = 1,
  source_interval = "week",
  from = min(weeks),
  to = max(weeks) + 6
)
rownames(islh_seasons) <- NULL

# islh_encounters and islh_feed_log -------------------------------------------

days <- seq(as.Date("2026-08-03"), as.Date("2026-08-30"), by = "day")
sites <- c("Site A", "Site B", "Site C")
site_rate <- c(6, 2, 4)

# Files that never arrived, and a day Site B had nobody to report.
missing_feed <- data.frame(
  site = "Site C",
  date = as.Date(c("2026-08-12", "2026-08-13"))
)
nil_report <- data.frame(site = "Site B", date = as.Date("2026-08-19"))

people <- sprintf("P%04d", 1:220)
records <- list()
for (s in seq_along(sites)) {
  for (d in seq_along(days)) {
    n <- stats::rpois(1, site_rate[s])
    if (sites[s] == nil_report$site && days[d] == nil_report$date) {
      n <- 0
    }
    if (n > 0) {
      records[[length(records) + 1L]] <- data.frame(
        site = sites[s],
        encounter_date = days[d],
        person_id = sample(people, n, replace = TRUE)
      )
    }
  }
}
encounters <- do.call(rbind, records)
# Drop encounters from the days Site C's files did not arrive: in real data
# those records are simply absent.
lost <- encounters$site == "Site C" &
  encounters$encounter_date %in% missing_feed$date
encounters <- encounters[!lost, ]
# A person seen twice on one day at one site is one encounter here.
encounters <- encounters[!duplicated(encounters), ]
encounters <- encounters[order(encounters$encounter_date, encounters$site), ]
encounters$encounter_id <- sprintf("E%05d", seq_len(nrow(encounters)))

# Three records resent in a later file, so the same encounter appears twice.
resent <- encounters[sample(nrow(encounters), 3), ]
islh_encounters <- rbind(encounters, resent)
islh_encounters <- islh_encounters[
  order(islh_encounters$encounter_date, islh_encounters$encounter_id),
  c("encounter_id", "person_id", "site", "encounter_date")
]
rownames(islh_encounters) <- NULL

log <- expand.grid(site = sites, date = days, stringsAsFactors = FALSE)
log <- log[
  !(paste(log$site, log$date) %in%
    paste(missing_feed$site, missing_feed$date)),
]
log$records <- vapply(
  seq_len(nrow(log)),
  function(i) {
    sum(
      islh_encounters$site == log$site[i] &
        islh_encounters$encounter_date == log$date[i]
    )
  },
  numeric(1)
)
log <- log[order(log$date, log$site), ]
islh_feed_log <- data.frame(
  site = log$site,
  date = log$date,
  records = as.integer(log$records)
)
rownames(islh_feed_log) <- NULL

usethis::use_data(
  islh_seasons,
  islh_encounters,
  islh_feed_log,
  overwrite = TRUE
)
