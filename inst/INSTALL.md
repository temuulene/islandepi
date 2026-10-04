# PHASE installation and pilot release

Staff target: Windows with R 4.4.1. Use a writable user library and the
approved Windows ZIPs. Do not install packages while knitting or rendering. Do
not use pak on managed staff laptops.

## What each release publishes

Each `islandepi` release on
[GitHub Releases](https://github.com/temuulene/islandepi/releases) carries:

- `islandepi_<version>.zip`, the Windows binary for R 4.4;
- `islandepi_<version>.tar.gz`, the source package;
- `install-phase.R`, the installer used below;
- `islandepi_<version>_windows-dependencies.csv`, every dependency and the
  exact version the Windows binary was built, checked and test-installed with.

Assets are published only after the Windows and Linux build and check jobs
succeed, and after the Windows ZIP has been installed into an empty library
with `install-phase.R`, loaded, and used for a check calculation.

`islandbrand` is released separately, at
[its own Releases page](https://github.com/temuulene/islandbrand/releases).
The two are approved as a pair: the team lead records which `islandbrand`
version goes with which `islandepi` version.

## Installing

1. Confirm the approved `islandepi` and `islandbrand` versions with the team
   lead.
2. Download from the `islandepi` release its ZIP, `install-phase.R` and the
   dependency manifest. Download the `islandbrand` ZIP from the paired
   `islandbrand` release.
3. In an interactive R session, `source()` the installer, then run it for each
   package with the actual paths and approved versions:

   ```r
   source("install-phase.R")
   install_phase_zip(file.choose(), package = "islandbrand", version = "x.y.z")
   install_phase_zip(file.choose(), package = "islandepi", version = "x.y.z")
   ```

   The installer checks that the ZIP's DESCRIPTION matches the package and
   version, installs the dependencies as CRAN Windows binaries, installs the
   package, then loads it and runs a small check calculation. It stops if any
   step fails.
4. Restart R. Save `sessionInfo()`, `packageVersion("islandepi")` and
   `packageVersion("islandbrand")` with the report.

## Reproducible and offline installs

Installing dependencies from CRAN gives the current CRAN versions, not the ones
in the dependency manifest. For a deployment that must match the release
exactly, or a laptop without internet access, the team lead prepares a bundle
of Windows binary ZIPs at the versions in the manifest. Install those ZIPs
first, then run the installer with `install_dependencies = FALSE`.

A package ZIP does not contain its dependencies.

## Before a pilot

Compare a communicable-disease, toxic-drug and heat or IMRC output with the
established team workflow. Agree on indicator inclusion rules, week
definitions, completeness, suppression relationships and standard population
vintage. These analytical policy decisions remain explicit inputs.
