# PHASE installation and pilot release

Staff target: Windows with R 4.4.1 or the current R release, and a writable
user library. Do not install packages while knitting or rendering. Do not use
pak on managed staff laptops.

## From GitHub

The simplest route installs both packages straight from GitHub. Restart R,
then run:

```r
install.packages("remotes", type = "binary")
remotes::install_github(
  c("temuulene/islandepi", "temuulene/islandbrand"),
  type = "binary", upgrade = "always", build = FALSE
)
```

Dependencies arrive as CRAN Windows binaries and neither package has code to
compile, so no Rtools, Git, GitHub account or token is needed. To install the
approved pair rather than the latest code, add each release tag, for example
`"temuulene/islandepi@vX.Y.Z"`. Rerun the command to update.

Use the release files below instead when a laptop cannot reach GitHub, or when
the deployment must match the dependency manifest exactly.

## What each release publishes

Each `islandepi` release on
[GitHub Releases](https://github.com/temuulene/islandepi/releases) carries:

- `islandepi_<version>.zip`, the Windows binary, built on R 4.4.1 and also
  test-installed on the current R release;
- `islandepi_<version>.tar.gz`, the source package;
- `install-phase.R`, the installer used below;
- `islandepi_<version>_windows-dependencies.csv`, every dependency and the
  exact version the Windows binary was built, checked and test-installed with.

Assets are published only after the Windows and Linux build and check jobs
succeed, and after the Windows ZIP has been installed into an empty library
with `install-phase.R`, loaded, and used for a check calculation, on R 4.4.1
and on the current R release.

`islandbrand` is released separately, at
[its own Releases page](https://github.com/temuulene/islandbrand/releases).
The two are approved as a pair: the maintainer records which `islandbrand`
version goes with which `islandepi` version.

## Installing from release files

1. Confirm the approved `islandepi` and `islandbrand` versions with the
   maintainer.
2. Download from the `islandepi` release its ZIP, `install-phase.R` and the
   dependency manifest. Download the `islandbrand` ZIP from the paired
   `islandbrand` release.
3. Restart R, so no package is loaded, then in that fresh interactive session
   `source()` the installer and run it for each package with the actual paths
   and approved versions:

   ```r
   source("install-phase.R")
   install_phase_zip(file.choose(), package = "islandbrand", version = "x.y.z")
   install_phase_zip(file.choose(), package = "islandepi", version = "x.y.z")
   ```

   The installer checks that the ZIP's DESCRIPTION matches the package and
   version, and stops if the package or one of its dependencies is already
   loaded, since Windows cannot replace a loaded package. It installs the
   dependencies as CRAN Windows binaries, checks they meet the versions the
   package requires, installs the package, then loads it and runs a small
   check calculation. It stops if any step fails or warns.
4. Restart R. Save `sessionInfo()`, `packageVersion("islandepi")` and
   `packageVersion("islandbrand")` with the report.

## Reproducible and offline installs

Installing dependencies from CRAN gives the current CRAN versions, not the ones
in the dependency manifest. For a deployment that must match the release
exactly, or a laptop without internet access, the maintainer prepares a bundle
of Windows binary ZIPs at the versions in the manifest. Install those ZIPs
first, then run the installer with `install_dependencies = FALSE`.

A package ZIP does not contain its dependencies.

## Before a pilot

Compare a communicable-disease, toxic-drug and heat or IMRC output with the
established team workflow. Agree on indicator inclusion rules, week
definitions, completeness, suppression relationships and standard population
vintage. These analytical policy decisions remain explicit inputs.
