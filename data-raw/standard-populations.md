# Standard populations

`R/standard-population.R` holds five standard populations in the 19 age
groups published by the SEER Program:

<https://seer.cancer.gov/stdpopulations/stdpop.19ages.html>

Retrieved 2026-09-30. The values were extracted from the page's HTML tables
by script, not typed, and each column was checked to sum exactly to the total
SEER prints (34,342,780 for Canada 2011, 38,239,864 for Canada 2021 and
1,000,000 for the others). The Canada 2011 total also matches Statistics
Canada's age-standardization page, <https://www.statcan.gc.ca/en/dai/btd/asr>.

To add a standard, take it from a primary or official source, check its total
against the published one, and add a test in `test-standard-population.R`.
Statistics Canada also publishes the Canadian standards with 85-89 and 90+
bands; those are not included because they were not verified against the
source for this release.
