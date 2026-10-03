# PHASE installation and pilot release

Staff target: Windows with R 4.4.1. Use a writable user library and the matching
approved Windows ZIP, obtained from the team's release or GitHub Releases.
Do not install packages while knitting or rendering. Do not use pak on managed
staff laptops. Offline installation also requires a locally available set of
matching binary dependencies; a package ZIP does not contain its dependencies.

1. Confirm the approved package version with the team lead. Release assets
   are published only after the Windows and Linux build/check jobs succeed.
2. Download both package ZIPs and `scripts/install-phase.R` from that release.
3. In an interactive R session, source the script. Run, using the actual paths
   and approved versions:

   `install_phase_zip(file.choose(), package = "islandepi", version = approved_epi_version)`

   Repeat for `islandbrand`. The helper installs CRAN dependencies as Windows
   binaries, validates the ZIP's DESCRIPTION, and verifies the installed version.
   For an offline dependency bundle, install those ZIPs first and set
   `install_dependencies = FALSE`.
4. Restart R, run the paired example, and save `sessionInfo()` plus
   `packageVersion("islandepi")` and `packageVersion("islandbrand")` with the
   report.

Before approving a pilot, compare a communicable-disease, toxic-drug and heat
or IMRC output with the established team workflow. Agree on indicator inclusion
rules, week definitions, completeness, suppression relationships and standard
population vintage. These analytical policy decisions remain explicit inputs.
