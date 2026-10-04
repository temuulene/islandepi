# Run interactively before rendering; base R only, no pak or compiler.
install_phase_zip <- function(zip, package, version, install_dependencies = TRUE,
    lib = path.expand(Sys.getenv("R_LIBS_USER")),
    repos = c(CRAN = "https://cran.r-project.org")) {
  stopifnot(.Platform$OS.type == "windows",
    package %in% c("islandbrand", "islandepi"),
    length(version) == 1L, !is.na(version), file.exists(zip))
  if (length(lib) != 1L || !nzchar(lib)) stop("Supply a writable user library.")
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  if (file.access(lib, 2) != 0) stop("The user library is not writable.")
  .libPaths(c(lib, .libPaths()))
  listing <- utils::unzip(zip, list = TRUE)$Name
  description <- paste0(package, "/DESCRIPTION")
  if (sum(listing == description) != 1L) stop("Unexpected ZIP package structure.")
  dest <- tempfile("phase-description-")
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  utils::unzip(zip, files = description, exdir = dest)
  d <- read.dcf(file.path(dest, description))
  if (d[1, "Package"] != package || d[1, "Version"] != version) {
    stop("The ZIP does not match the requested package/version.")
  }
  if (install_dependencies) {
    fields <- intersect(c("Depends", "Imports"), colnames(d))
    deps <- trimws(unlist(strsplit(paste(d[1, fields], collapse = ","), ",")))
    deps <- unique(trimws(sub("\\s*\\(.*", "", deps)))
    base <- rownames(utils::installed.packages(priority = c("base", "recommended")))
    deps <- setdiff(deps, c("R", "", base))
    utils::install.packages(deps, lib = lib, repos = repos, type = "binary")
  }
  utils::install.packages(zip, lib = lib, repos = NULL, type = "win.binary")
  if (as.character(utils::packageVersion(package, lib.loc = lib)) != version) {
    stop("Installed version does not match the approved version.")
  }
  # Loading proves the dependencies resolve; a small calculation proves the
  # package runs, not just that its files are in place.
  loadNamespace(package, lib.loc = c(lib, .libPaths()))
  run <- function(name, ...) getExportedValue(package, name)(...)
  works <- switch(package,
    islandepi = isTRUE(all.equal(run("islh_crude_rate", 5, 1000)$rate, 500)),
    islandbrand = grepl("^#[0-9A-Fa-f]{6}$", run("islh_brand", "primary")))
  if (!isTRUE(works)) stop("The package installed but its check calculation failed.")
  message(package, " ", version, " installed, loaded and checked.")
  invisible(utils::packageDescription(package, lib.loc = lib))
}
