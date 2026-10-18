# Installs the way the README tells staff to: a clean library, no compiler
# tools and no GitHub token. Pass repositories to install from public GitHub,
# or a directory to install that checkout by the same route, so a pull request
# is tested before it is merged.
#
# Usage:
#   Rscript data-raw/check-github-install.R temuulene/islandepi temuulene/islandbrand
#   Rscript data-raw/check-github-install.R .
check_github_install <- function(sources) {
  if (.Platform$OS.type != "windows") {
    stop("Run this check on Windows.")
  }
  library <- tempfile("staff-library-")
  dir.create(library)
  .libPaths(c(library, .Library), include.site = FALSE)
  Sys.setenv(R_LIBS_USER = library, R_LIBS_SITE = "")
  # Keep only Windows and R on PATH. A preinstalled compiler must not make a
  # source dependency appear to work on a staff laptop without build tools.
  Sys.setenv(
    PATH = paste(
      c(
        R.home("bin"),
        Sys.getenv("SystemRoot"),
        file.path(Sys.getenv("SystemRoot"), "System32")
      ),
      collapse = ";"
    )
  )
  Sys.unsetenv(c(
    "RTOOLS_HOME",
    paste0("RTOOLS", c("40", "42", "43", "44", "45", "46"), "_HOME"),
    "BINPREF"
  ))
  tools <- c("make", "gcc", "g++", "gfortran")
  stopifnot(all(Sys.which(tools) == ""))
  # Leave R's default package type alone: the README's `type = "binary"` must
  # be what keeps every dependency a prebuilt binary.
  options(repos = c(CRAN = "https://cloud.r-project.org"))

  utils::install.packages(
    "remotes",
    lib = library,
    type = "binary",
    quiet = TRUE
  )
  candidate <- length(sources) == 1L && dir.exists(sources)
  if (candidate) {
    remotes::install_local(
      sources,
      type = "binary",
      upgrade = "always",
      build = FALSE,
      lib = library,
      force = TRUE,
      quiet = TRUE
    )
    packages <- "islandepi"
  } else {
    remotes::install_github(
      sources,
      type = "binary",
      upgrade = "always",
      build = FALSE,
      lib = library,
      auth_token = NULL,
      quiet = TRUE
    )
    packages <- basename(sources)
  }

  for (package in packages) {
    stopifnot(requireNamespace(package, lib.loc = library, quietly = TRUE))
  }
  rate <- islandepi::islh_crude_rate(12, 50000)
  stopifnot(isTRUE(all.equal(rate$rate, 24)))
  stopifnot(all(Sys.which(tools) == ""))
  message(
    "Installed ",
    paste(packages, collapse = " and "),
    " without compiler tools; a check calculation ran."
  )
  invisible(library)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0L) {
  stop("Supply GitHub repositories or a package directory.")
}
check_github_install(args)
