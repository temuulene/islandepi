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
  * `islh_compare_periods()` and `islh_farrington()` refuse partial or
    unknown-partial periods, as the baseline and snapshot already did.
  * `islh_reporting_completeness()` learns delays only from events at least
    `maturity` days old, warns when older events contradict that assumption,
    and reports the learning window beside each estimate.
* Outbreak detection with the improved Farrington method, through the
  surveillance package. Each week has a `status` that tells an assessed week
  from one set aside by the low-count rule or a failed fit, the low-count rule
  is adjustable, and the settings used are recorded with the result.
* BC population and geography data from pinned BC Data Catalogue sources,
  tested against a slice of the current catalogue schema.
* Age groups, checked denominator joins, crude and directly standardized rates,
  proportions and Poisson intervals.
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
  year-on-year changes and performance against a target.
* Releases publish the standalone installer and a manifest of the dependency
  versions the Windows binary was built with, and test-install the binary into
  an empty library before publishing.
* Simulated example data for the examples and articles.
