#' @keywords internal
.onAttach <- function(libname, pkgname) {
  assign("status_dir", tempdir(), envir = .GlobalEnv)
  dir.create(status_dir)
  assign("status_file", paste0(status_dir, "/status.Rdata"), envir = .GlobalEnv)
  ## assign("log_file", paste0(status_dir, "/log.Rdata"), envir = .GlobalEnv)
}

## .onUnLoad()
