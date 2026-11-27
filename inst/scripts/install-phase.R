# Run interactively before rendering; base R only, no pak or compiler.
# Start a fresh R session first: a package that is already loaded cannot be
# replaced on Windows, and loading it again would test the old copy. Both
# packages can then be installed one after the other in that session.
install_phase_zip <- function(zip, package, version, install_dependencies = TRUE,
    lib = path.expand(Sys.getenv("R_LIBS_USER")),
    repos = c(CRAN = "https://cran.r-project.org")) {
  stopifnot(.Platform$OS.type == "windows",
    is.character(package), length(package) == 1L,
    package %in% c("islandbrand", "islandepi"),
    is.character(zip), length(zip) == 1L, !is.na(zip), file.exists(zip),
    length(version) == 1L, !is.na(version))
  if (!is.character(lib) || length(lib) != 1L || is.na(lib) || !nzchar(lib)) {
    stop("Supply one writable user library folder in `lib`.")
  }
  if (grepl(.Platform$path.sep, lib, fixed = TRUE)) {
    stop("`lib` names more than one folder. Supply one library folder.")
  }
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  # file.access() is unreliable on Windows network and OneDrive folders, so
  # try writing a file instead.
  probe <- tempfile("phase-write-check-", tmpdir = lib)
  if (!isTRUE(suppressWarnings(file.create(probe)))) {
    stop("The user library is not writable: ", lib)
  }
  unlink(probe)

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
  fields <- intersect(c("Depends", "Imports"), colnames(d))
  requirements <- .phase_requirements(d[1, fields])
  base <- rownames(
    utils::installed.packages(priority = c("base", "recommended")))
  deps <- setdiff(requirements$package, c("R", base))

  if (package %in% loadedNamespaces()) {
    stop("Restart R before installing ", package, ": it is already loaded.",
      call. = FALSE)
  }
  # A dependency loaded by an earlier install in this session, such as the
  # islandbrand install before islandepi, is kept if it is recent enough.
  # Reinstalling it would fail on Windows while it is loaded.
  loaded_deps <- intersect(deps, loadedNamespaces())
  .phase_check_versions(
    requirements[requirements$package %in% loaded_deps, , drop = FALSE],
    .libPaths(), restart = TRUE)
  deps <- setdiff(deps, loaded_deps)

  old_paths <- .libPaths()
  finished <- FALSE
  on.exit(if (!finished) .libPaths(old_paths), add = TRUE)
  .libPaths(c(lib, .libPaths()))

  # install.packages() only warns when a download fails or a file is locked,
  # and the old copy stays in place. Treat any warning as a failure.
  strict <- function(expr, what) {
    withCallingHandlers(expr, warning = function(w) {
      stop("Installing ", what, " failed: ", conditionMessage(w), call. = FALSE)
    })
  }
  if (install_dependencies && length(deps) > 0L) {
    strict(utils::install.packages(deps, lib = lib, repos = repos,
      type = "binary"), "the dependencies")
  }
  .phase_check_versions(requirements, c(lib, .libPaths()))
  strict(utils::install.packages(zip, lib = lib, repos = NULL,
    type = "win.binary"), package)
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
  finished <- TRUE
  message(package, " ", version, " installed, loaded and checked.")
  invisible(utils::packageDescription(package, lib.loc = lib))
}

# "dplyr (>= 1.1.4), rlang" as one row per package with any version rule.
.phase_requirements <- function(fields) {
  entries <- trimws(unlist(strsplit(paste(fields, collapse = ","), ",")))
  entries <- entries[nzchar(entries)]
  pattern <- paste0("^([A-Za-z0-9.]+)\\s*",
    "(\\(\\s*(>=|<=|==|>|<)\\s*([^) ]+)\\s*\\))?$")
  rule <- regmatches(entries, regexec(pattern, entries))
  bad <- lengths(rule) == 0L
  if (any(bad)) stop("Cannot read the dependency: ", entries[bad][1])
  data.frame(
    package = vapply(rule, `[`, "", 2L),
    operator = vapply(rule, `[`, "", 4L),
    version = vapply(rule, `[`, "", 5L),
    stringsAsFactors = FALSE
  )
}

# Every dependency must be installed at a version DESCRIPTION accepts, so an
# old or offline copy cannot pass for the right one. A loaded dependency is
# judged by the version in use, not the copy on disk.
.phase_check_versions <- function(requirements, lib_paths, restart = FALSE) {
  for (i in seq_len(nrow(requirements))) {
    name <- requirements$package[i]
    operator <- requirements$operator[i]
    installed <- if (name == "R") {
      getRversion()
    } else if (name %in% loadedNamespaces()) {
      package_version(getNamespaceVersion(name))
    } else {
      tryCatch(utils::packageVersion(name, lib.loc = lib_paths),
        error = function(e) NULL)
    }
    if (is.null(installed)) stop("Dependency ", name, " is not installed.")
    if (nzchar(operator)) {
      needed <- package_version(requirements$version[i])
      if (!isTRUE(do.call(operator, list(installed, needed)))) {
        stop(name, " ", format(installed), " is ",
          if (restart) "loaded" else "installed", ", but ", operator, " ",
          format(needed), " is required.",
          if (restart) " Restart R and run the installer first.",
          call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}
