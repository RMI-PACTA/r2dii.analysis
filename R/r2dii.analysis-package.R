#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # CRAN OMP THREAD LIMIT
  # logic for setting OMP_THREAD_LIMIT form https://stackoverflow.com/a/77323812
  # check modified from testthat's on_cran function
  if (!interactive() && isFALSE(as.logical(Sys.getenv("NOT_CRAN", "false")))) {
    Sys.setenv("OMP_THREAD_LIMIT" = 2)
  }
}

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL
