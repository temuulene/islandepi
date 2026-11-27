# islandepi (development version)

## Disclosure control

* `islh_suppress_table()` with `complementary = TRUE` now also hides a
  partner cell when several small cells in a block add up to a value the rule
  would suppress, or to a value with only one solution. For example, two
  hidden cells beside a visible 40 and a total of 42 must both be 1.
* With `complementary = TRUE` and a `label`, `label` and
  `complementary_label` must be the same neutral text. Labelling small cells
  `"<5"` and their partner `"Suppressed"` showed which cell was small, and
  with a total in view a reader could often work out both values.
* Counts within floating-point error of a whole number, such as
  `5 + 1e-15`, are rounded before the rule is applied, so they are
  suppressed as the whole number they stand for. `islh_round_base()` rounds a
  near-whole `base` the same way.
* `linked` is checked entry by entry. A nested list used to pass the check
  and hide nothing.
* A table with repeated or empty column names is refused by
  `islh_suppress_table()` and `islh_release_copy()`. A position used to reach
  the first column with that name.
* A grouped tibble needs `by` for complementary suppression; its grouping
  was ignored.
* Suppressed tables write counts and linked values in full, not as
  `"1e+05"`.
* A second `islh_suppress_table()` call on the same table keeps the first
  call's audit record.
* `islh_release_copy()` converts `POSIXlt` times to `POSIXct`, takes only the
  named columns from an `sf` table, and refuses matrix, S4 and other classed
  columns it cannot copy without changing them.

## Rates and comparisons

* `islh_proportion()` no longer returns missing Wilson limits for integer
  counts above about 92,000, which overflowed 32-bit arithmetic.
* `islh_dsr_ratio()` matches standard weights stratum by stratum, refuses
  rates on different `per` scales, adds a `per` column, and reports a
  `standard_id` only when both rates name the same one.
* `islh_dsr_joined()` refuses key names the stratum detail uses, such as
  `weight`, matches factor and text keys by label, and labels `strata` with
  the key values rather than row numbers.
* `islh_crude_rate()` and `islh_proportion()` return zero rows for empty
  input, and `islh_crude_rate()` checks `population` before recycling it.
* Key matching treats ordered factors, plain factors and text as labels, so
  `islh_join_denominator()` and related functions no longer fail on an
  ordered age group from `islh_age_group()`.
* `islh_compare_outputs()` keeps the key labels of rows found only in `y`
  when one table uses text and the other a factor.
* The help for `islh_dsr()` says that `cases` and `population` include strata
  with a zero standard weight.

## Surveillance

* `islh_surveillance_snapshot()` with `coverage` shows the unreported
  periods of a group that was not expected as unknown, not zero, and ignores
  groups with no rows in the window.
* A seven-day daily snapshot can be compared with a weekly baseline whatever
  day it ends on. The week anchor check now applies to weekly snapshots only.
* `islh_count_events()` with `count` checks tables without metadata as it
  checks recorded ones: partial periods are refused, and a table whose dates
  are not consecutive days must name `source_interval`. A group named in
  `groups` with no rows is reported as missing.
* Count tables record their grouping columns, so site counts can be rolled up
  to areas. Completeness is checked site by site before the counts are added.
* `islh_reporting_calendar()` assigns seasons by whole weeks, so each season
  has a week 1, and returns zero rows for empty input.
* `islh_check_events()` checks every `date_order` rule, including several for
  the same later date, and reports each invalid date once.
* `islh_compare_periods()` refuses rows that each total several periods,
  orders its output as `comparison` was given, and leaves the ratio missing
  when the current value is.
* `islh_farrington()` refuses an empty table, reports a model failure as a
  package error, labels Sunday-start series by epidemiological week, and
  leaves `expected`, `upper_limit` and `pvalue` missing for weeks the
  low-count rule set aside.
* `islh_reporting_delay()` names quantile columns `delay_p50` and
  `delay_p97.5` rather than `delay_p50.0`, and a report dated before its
  onset no longer sets the default `as_of`.
* `islh_reporting_completeness()` checks every mature record for slow
  arrivals, not only those in the learning window.
* Whole-number settings such as `periods` refuse `Inf`.

## Messages

* Error text from the BC Data Catalogue, from column selection and from
  group labels is shown as written. Braces in it were read as code.

## Installing and releasing

* `install-phase.R` stops when the package or a dependency is already loaded,
  treats any install warning as a failure, checks dependency versions against
  `DESCRIPTION`, probes the library by writing to it, and restores the library
  paths when it fails. The install guide says to restart R first.
* The release workflow is read-only except for the publishing job, pins
  third-party actions to commit hashes, tolerates Windows line endings in the
  version check and publishes `SHA256SUMS.txt`.

# islandepi 0.1.0

First public release. It includes:

* Routine surveillance: event and feed coverage checks, complete period
  counts, reporting calendars, historical baselines, current-window snapshots,
  period comparisons, and reporting delay and completeness.
  * `islh_surveillance_snapshot()` takes the result of `islh_check_coverage()`
    as `coverage`, so a group that sent nothing stays in the table as unknown,
    and an unreceived period is unknown rather than a filled zero, in the
    group's total and in the `All` row.
  * A table without recorded metadata must say what one row covers
    (`source_interval` in the snapshot, `interval` in baselines and
    comparisons), unless its dates are consecutive days. Rows a week apart
    could be weekly totals or daily counts with days missing.
  * `islh_compare_periods()` gives exact limits for each ratio
    (`ratio_lower`, `ratio_upper`) at the level set by `conf`.
  * `islh_compare_periods()` and `islh_farrington()` refuse partial or
    unknown-partial periods, as the baseline and snapshot already did.
  * A date-time is converted to `timezone` before its calendar date is taken.
    A warning says when that moves any date, as it does for local clock times
    that a database driver labelled UTC, and the help for
    `islh_count_events()` shows how to settle it.
  * `islh_reporting_completeness()` learns delays only from events at least
    `maturity` days old, warns when older events contradict that assumption,
    and reports the learning window beside each estimate.
* Outbreak detection with the improved Farrington method, through the
  surveillance package. Each week has a `status` that tells an assessed week
  from one set aside by the low-count rule or a failed fit, the low-count rule
  is adjustable, and the settings used are recorded with the result.
* BC population and geography data from pinned BC Data Catalogue sources,
  tested against a slice of the current catalogue schema.
  * `islh_bc_population()` reads the catalogue's text columns as text. Left
    to guess, the reader took the sex column of the health service delivery
    area file for a logical one, and `"hsda"` returned no rows. A sex column
    holding anything but `F`, `M` and `T` stops with a message.
* Age groups, checked denominator joins, crude and directly standardized rates,
  proportions and Poisson intervals.
  * `islh_dsr_ratio()` compares two directly standardized rates with Fay's F
    interval, which uses each rate's stratum detail. Comparing their totals
    would compare the crude rates instead.
  * `islh_rate_ratio()` compares two crude rates or counts with the exact
    conditional interval that `stats::poisson.test()` gives, vectorized for
    use inside `mutate()`.
* Disclosure control: suppression, a suppression audit and rounding, with
  thresholds and bases supplied by the caller. `islh_suppress_table()` removes
  the stratum detail behind a standardized rate, and `islh_release_copy()`
  makes a plain copy with only approved columns to share.
* Output checks against a legacy report, a dashboard extract or an earlier run.
  A zero tolerance means exact agreement.
* Grouping, joining and duplicate checks match key columns as values, so a
  missing value and the text `"NA"` stay distinct and no character in a value
  can make two keys collide.
* The article "Why every estimate needs an interval", with worked examples of
  how confidence intervals change the reading of small-area rates, rankings,
  year-on-year changes and performance against a target. It states what the
  Poisson model assumes, compares areas with a funnel plot of standardized
  ratios, and compares rates with ratio intervals rather than by rank.
* The routine surveillance guide works from the line list as it stood on the
  reporting date, screens the last complete week rather than the current,
  partial one, and shows how reporting delay, missing feeds and the choice of
  baseline change what a signal means.
* Examples and articles use dplyr and tidyr: `|>`, `.by`, `join_by()` and
  results spliced into `mutate()` and `summarise()`.
* Staff can install directly from GitHub with one `remotes::install_github()`
  command, alone or together with `islandbrand`. Dependencies arrive as
  Windows binaries and no Rtools is needed; a check installs this way on
  Windows without compiler tools.
* Releases publish the standalone installer and a manifest of the dependency
  versions the Windows binary was built with, and test-install the binary into
  an empty library, on R 4.4.1 and on the current R, before publishing. The
  release workflow can also be run by hand as a rehearsal that publishes
  nothing.
* Example data for the examples and articles. `islh_outbreak` and
  `islh_seasons` describe the same simulated 2025-26 season, and
  `islh_lha_population` holds BC Stats' 2025 population estimates for
  Island Health's 14 local health areas.
