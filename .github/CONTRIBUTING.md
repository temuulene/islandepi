# Contributing

Guidance for working in this repository.

## What this repo is

`islandepi` holds the epidemiological methods used by the PHASE team at Island
Health: surveillance event checks and period counts, descriptive baselines and
snapshots, age grouping, crude and directly standardized rates, Poisson
intervals, small-cell suppression and rounding, and pinned BC Data Catalogue
downloads. Its sister package `islandbrand` handles branding: ggplot2
themes, tables and report templates.

## Git conventions

Conventional-commit subjects: `feat:`, `fix:`, `docs:`, `test:`, `chore:`,
`refactor:`, `ci:`. Breaking changes get `!`.

## Commands

```bash
Rscript -e 'devtools::document()'   # regenerate man/ and NAMESPACE
Rscript -e 'devtools::test()'       # run the test suite
Rscript -e 'devtools::check()'      # full R CMD check
air format .                        # format before committing (air >= 0.4)
```

## Constraints

Island Health laptops have no compiler, and group policy blocks programs run
from a user library.

- **Never add a `src/` directory.** The package must install from source
  without a compiler.
- **Never use `pak`** or any installer that unpacks with its own helper binary.
  `inst/scripts/install-phase.R` uses base R's
  `install.packages(type = "binary")`.
- **Loading must stay inert.** No network requests, `options()` or messages
  when the package loads. The BC Data Catalogue is only contacted when a
  download function is called.
- **Never use `<<-`.** Use a local variable or a small environment instead.

## Dependencies

`Imports` holds what the core methods need. `bcdata` and `sf` stay in
`Suggests` behind `.islh_require_packages()`, so staff who only calculate
rates never install the spatial stack. Do not add tidyverse dependencies
beyond `dplyr`, `tidyr`, `tidyselect` and `lubridate`.

## Design rules

- **Disclosure control is mechanics, not policy.** `islh_suppress()`,
  `islh_suppress_table()` and `islh_round_base()` have no default threshold or
  base. The right value depends on the data and the release, so the caller
  must supply it. Do not add a default.
- **Fail closed.** A wrong input must stop the calculation, not travel quietly
  into a published table. Flags go through `.islh_check_flag()`, which rejects
  `NA` and `1`, and counts go through `.islh_check_counts()`, which rejects
  factors.
- **Never drop data silently.** When a function leaves out records by design,
  such as partial periods, it says so with `.islh_inform()`.
- **Surveillance results carry calendar metadata.** Baselines and snapshots
  read it to refuse comparisons of different durations. Keep it on any new
  result that feeds them.
- **The prefix is `islh_`, shared with `islandbrand`.** Both packages are often
  attached together, so check `islandbrand`'s exports before naming a new
  function. Never use `ih_`: that is Interior Health.
- **Public API is dot-free; internals are `.islh_`-prefixed.** If you export
  something new, add it to `_pkgdown.yml`.
- Errors, warnings and messages go through `.islh_abort()`, `.islh_warn()` and
  `.islh_inform()` in `R/conditions.R`, so they carry the `islh_error`,
  `islh_warning` and `islh_message` classes. An internal helper that can abort
  takes `call = rlang::caller_env()` and passes it on, so the error names the
  exported function the user called. `test-conditions.R` checks this.

## Methods

Interval methods follow published formulas, and the tests compare them with
worked examples from the literature. When you change a method, keep those
tests and add one for the new case. Cite the source in the roxygen
`@references`.

## Style

Follow the tidyverse style guide and format with `air` before committing:
`snake_case`, `<-` for assignment, native `|>` where it helps readability,
explicit `package::function()` calls for anything outside base R.

Roxygen docs on every exported function, wrapped at 80 characters, with a
runnable example unless it needs the network; internals get `@noRd` or no
roxygen at all.

## Versioning

`DESCRIPTION`'s `Version:` is the only version string. Bumping a version means
editing `DESCRIPTION` and adding a `NEWS.md` heading. Order NEWS entries
alphabetically by function name within each section.

## Prose

The README, articles, messages and release notes are written for Island Health
staff: Canadian spelling (standardize, colour, behaviour), plain language, no
marketing tone. dplyr's `summarise()` keeps its own spelling in code.
