.read.profile <- function(pkgname = .packageName) {
  cfg <- tools::R_user_dir(pkgname, "config") |> file.path("profile.rds")
  if (file.exists(cfg)) tryCatch(readRDS(cfg), error = function(e) {
    warning("User configuration file is corrupt, ignoring")
    list()
  }) else list()
}

.set.profile <- function(..., pkgname = .packageName) {
  prof <- .read.profile(pkgname)
  l <- rlang::list2(...)
  for (key in names(l)) prof[[key]] <- as.character(l[[key]])
  cfg.dir <- tools::R_user_dir(pkgname, "config")
  if (!dir.exists(cfg.dir) && !dir.create(cfg.dir, TRUE, TRUE, "0700"))
    cli::cli_abort("Cannot create configuration directory {.path {cfg.dir}} ")
  saveRDS(prof, fn <- file.path(cfg.dir, "profile.rds.new"))
  Sys.chmod(fn, "0600")
  file.rename(fn, file.path(cfg.dir, "profile.rds"))
  do.call(Sys.setenv, prof)
  invisible(prof)
}

.onLoad <- function(libname, pkgname) {
  prof <- .read.profile()
  ## do not override already present env vars
  prof <- prof[is.na(sapply(names(prof), Sys.getenv, NA))]
  if (length(prof)) do.call(Sys.setenv, prof)
}
