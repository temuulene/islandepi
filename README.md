# islandepi <a href="https://temuulene.github.io/islandepi/"><img src="man/figures/logo.png" align="right" height="139" alt="islandepi website" /></a>

Validated epidemiological methods for Island Health analyses.

`islandepi` provides reusable mechanics for routine surveillance, rates,
standardization, disclosure control and BC population and geography data. It
does not choose case definitions, alert policy, suppression thresholds or the
public-health response to a signal.

Every rate and proportion comes with a confidence interval. See
[Why every estimate needs an interval](https://temuulene.github.io/islandepi/articles/intervals.html)
for worked examples of why PHASE products should report them.

Documentation: <https://temuulene.github.io/islandepi/>

New users should start with
[Getting started with islandepi](https://temuulene.github.io/islandepi/articles/islandepi.html).
For event-level reporting, see the evaluated
[routine surveillance guide](https://temuulene.github.io/islandepi/articles/surveillance.html).

## Installing

For staff, follow [the supported installation guide](inst/INSTALL.md). Each
release on GitHub Releases publishes the Windows ZIP, the standalone installer
`install-phase.R` and a manifest of the dependency versions it was built with.
Run installation outside a report render.

Release assets are created only after release checks pass and the Windows ZIP
has been test-installed into an empty library. A version number in DESCRIPTION
does not mean its ZIP has been published. The installer checks the package
name and version against your requested version, then loads the package and
runs a check calculation.

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
| Compare two rates as a ratio | `islh_rate_ratio()`, `islh_dsr_ratio()` |
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
library(dplyr)
library(islandepi)

issues <- islh_outbreak |>
  islh_check_events(
    id = case_id,
    date = date_onset,
    required = c(hsda, case_type)
  )

daily <- islh_outbreak |>
  islh_count_events(
    date = date_onset,
    id = case_id,
    by = hsda,
    interval = "day",
    from = "2025-11-03",
    to = "2026-03-08",
    fill = TRUE
  )

snapshot <- daily |>
  islh_surveillance_snapshot(
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

The functions take vectors and return a data frame, so they work inside
`mutate()` and `summarise()`. `islh_lha_population` holds BC Stats' 2025
estimates for Island Health's local health areas, for examples like this one:

```r
hsda_population <- islh_lha_population |>
  summarise(population = sum(population), .by = hsda)

islh_outbreak |>
  count(hsda, name = "cases") |>
  islh_join_denominator(hsda_population, by = "hsda") |>
  mutate(
    islh_crude_rate(cases, population, per = 100000) |>
      select(rate, lower, upper)
  )

islh_age_group(c(0, 4, 18, 67, 91))
islh_suppress(c(0, 3, 42), threshold = 5)
```

To suppress a table of rates, use `islh_suppress_table()` with `linked`, so a
rate is hidden wherever its count is.

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

Apart from `islh_lha_population`, a fixed copy for examples, catalogue data
are not bundled into releases. Save a dated extract in the analysis project
when the governing reproducibility or retention standard requires it.

Use [`islandbrand`](https://github.com/temuulene/islandbrand) for Island Health
figures, tables and Quarto reports. The packages can be loaded together.

## Licence

`islandepi` is public so staff can install it without a GitHub account, but it
is licensed for Island Health work: employees, contractors and partners may
use, copy, modify and distribute it for that purpose. See [LICENSE](LICENSE).
