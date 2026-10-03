# islandepi

Validated epidemiological methods for Island Health analyses.

`islandepi` provides reusable mechanics for routine surveillance, rates,
standardization, disclosure control and BC population and geography data. It
does not choose case definitions, alert policy, suppression thresholds or the
public-health response to a signal.

Documentation: <https://temuulene.github.io/islandepi/>

New users should start with
[Getting started with islandepi](https://temuulene.github.io/islandepi/articles/islandepi.html).
For event-level reporting, see the evaluated
[routine surveillance guide](https://temuulene.github.io/islandepi/articles/surveillance.html).

## Installing

For staff, use a checked Windows binary from the team's approved release,
with its dependencies preinstalled in a writable user library. Run installation
outside a report render. See [the supported installation guide](inst/INSTALL.md)
and `inst/scripts/install-phase.R` for the base-R bootstrap.

Release assets are created only after release checks pass. A version number in
DESCRIPTION does not mean its ZIP has been published. Download the exact
approved ZIP from GitHub Releases or obtain it from the team lead; the installer
checks the package name and version against your requested version.

Developers can install a reviewed commit using `remotes::install_github()` with
an explicit `ref`. Record both package versions and `sessionInfo()` with every
report.

## Choose the function

| Task | Function |
|---|---|
| Find record-level event-data problems | `islh_check_events()` |
| Separate missing feeds from quiet days | `islh_check_coverage()` |
| Create complete daily or weekly counts | `islh_count_events()` |
| Label weeks, fiscal years and seasons | `islh_reporting_calendar()` |
| Choose matched reference weeks from earlier years | `islh_reference_periods()` |
| Calculate a historical reference | `islh_surveillance_baseline()` |
| Create a current-window reporting table | `islh_surveillance_snapshot()` |
| Compare with last week, last year or last season | `islh_compare_periods()` |
| Summarize reporting delays and recent completeness | `islh_reporting_delay()`, `islh_reporting_completeness()` |
| Model-based outbreak detection (needs surveillance) | `islh_farrington()` |
| Group ages | `islh_age_group()` |
| Attach denominators with checked keys | `islh_join_denominator()` |
| Crude or directly standardized rates | `islh_crude_rate()`, `islh_dsr_joined()`, `islh_dsr()` |
| Published standard populations | `islh_standard_population()` |
| See which strata drive a standardized rate | `islh_dsr_detail()` |
| Proportions and percentages | `islh_proportion()` |
| Poisson count intervals | `islh_ci_poisson()` |
| Suppression, including linked rates | `islh_suppress()`, `islh_suppress_table()` |
| Count rounding | `islh_round_base()` |
| BC population and boundaries | `islh_bc_population()`, `islh_bc_geography()` |
| Compare with a legacy report or dashboard | `islh_compare_outputs()` |

## Routine surveillance

`islh_outbreak`, a simulated line list bundled with the package, stands in
for real surveillance data in these examples.

```r
library(islandepi)

issues <- islh_check_events(
  islh_outbreak,
  id = case_id,
  date = date_onset,
  required = c(hsda, case_type)
)

daily <- islh_count_events(
  islh_outbreak,
  date = date_onset,
  id = case_id,
  by = hsda,
  interval = "day",
  from = "2025-11-03",
  to = "2026-03-08",
  fill = TRUE
)

snapshot <- islh_surveillance_snapshot(
  daily,
  date = period_start,
  value = count,
  by = hsda,
  periods = 7
)
```

When a baseline is supplied, it must describe the same duration as the
snapshot `total` (for example, historical weekly totals for seven daily
columns).

`islh_count_events()` supports ordinary weeks with a chosen start day, ISO
weeks and CDC epidemiological weeks. It constructs reporting periods from dates
instead of requiring hand-written corrections around New Year.

## Rates and disclosure control

```r
islh_crude_rate(cases = 12, population = 50000)
islh_proportion(x = 45, n = 60, per = 100)
islh_age_group(c(0, 4, 18, 67, 91))
islh_suppress(c(0, 3, 42), threshold = 5)
```

Counts may exceed population or person-time when events can recur. Suppression
thresholds and rounding bases must be supplied explicitly for each release.

## Population and geography data

`islandepi` uses permanent BC Data Catalogue identifiers instead of catalogue
search terms. This keeps the selected records stable while allowing the
Province to publish updated estimates and boundaries.

```r
library(dplyr)

population_extract <- islh_bc_population("lha", years = 2025, sex = "T")

population <- population_extract |>
  summarise(
    population = sum(population),
    .by = c(geography, geography_code, year, estimate_type)
  )

boundaries <- islh_bc_geography("lha")

map_data <- left_join(
  boundaries,
  population,
  by = join_by(geography, geography_code),
  relationship = "one-to-one",
  na_matches = "never"
)

if (anyNA(map_data$population)) {
  stop("One or more Island Health boundaries did not match population data.")
}
```

The default boundary filter uses the catalogue value `Vancouver Island`.
`health_authority = "Island Health"` is accepted as an alias, and `NULL`
returns all of BC. Population resources cover all of BC, so population rows
outside the filtered boundary object are expected to be discarded by the join.

Downloaded data are not bundled into releases. Save a dated extract in the
analysis project when the governing reproducibility or retention standard
requires it.

Use [`islandbrand`](https://github.com/temuulene/islandbrand) for Island Health
figures, tables and Quarto reports. The packages can be loaded together.
