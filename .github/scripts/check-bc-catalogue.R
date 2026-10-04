# Live check of the pinned BC Data Catalogue sources.
#
# Ordinary builds never download from the catalogue. This script does, so the
# scheduled workflow can say when a pinned resource disappears or its schema
# changes. It stops with an error on the first problem.

pkgload::load_all(".", quiet = TRUE)

island_lha <- c(
  "411", "412", "413", "414", "421", "422", "423", "424", "425", "426",
  "431", "432", "433", "434"
)
island_hsda <- c("41", "42", "43")

check <- function(label, expr) {
  result <- tryCatch(
    {
      force(expr)
      "ok"
    },
    error = function(e) conditionMessage(e)
  )
  cat(sprintf("%-40s %s\n", label, result))
  result == "ok"
}

results <- c(
  check(
    "LHA population, estimates",
    islh_bc_population(
      "lha",
      estimate_type = "Estimate",
      expected_codes = island_lha
    )
  ),
  check(
    "HSDA population, estimates",
    islh_bc_population(
      "hsda",
      estimate_type = "Estimate",
      expected_codes = island_hsda
    )
  ),
  check(
    "LHA boundaries, Vancouver Island",
    islh_bc_geography("lha", expected_codes = island_lha)
  ),
  check(
    "HSDA boundaries, Vancouver Island",
    islh_bc_geography("hsda", expected_codes = island_hsda)
  )
)

if (!all(results)) {
  stop("At least one pinned BC catalogue source failed its check.")
}
