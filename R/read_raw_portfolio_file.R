#' Read raw portfolio data
#'
#' @param path A character string giving the path to a raw portfolio file.
#' @param delim Passed to [readr::read_delim].
#' @param ... Arguments passed to [readr::read_delim].
#'
#' @seealso [find_project_input_files()]
#'
#' @return A [tibble::tibble].
#' @export
#'
#' @examples
#' "raw_portfolio.csv" %>%
#'   path_example("r2dii.analysis") %>%
#'   read_raw_portfolio()
read_raw_portfolio <- function(path, delim = ",", ...) {
  abort_if_multiple_paths(path)

  if (identical(length(path), 1L)) {
    out <- readr::read_delim(path, delim = delim, ...)
  }

  if (!isTRUE(ncol(out) > 1L)) {
    warn(
      glue(
        "The portfolio should have multiple columns but it doesn't.
        Do you need a different {ui_code('delim')}?"
      )
    )
  }

  out
}

#' Find input files in a project
#'
#' @inheritParams path_project_dirs
#' @param regexp A regular expression for match files in `project`.
#' @inheritDotParams fs::dir_ls
#'
#' @return A character vector
#' @export
#'
#' @examples
#' find_project_input_files("TEST")
#'
#' match_csv_files <- "TEST_Input[.]csv"
#' find_project_input_files("TEST", regexp = match_csv_files)
find_project_input_files <- function(project,
                                     regexp = glue("{project}_Input[.]csv"),
                                     ...) {
  check_dots_used()
  dir_ls(path(path_project(project, "20_Raw_Inputs")), regexp = regexp, ...)
}

abort_if_multiple_paths <- function(path) {
  if (length(path) >= 2L) {
    abort(glue("Can't read more than one file but found {length(path)}."))
  }
}
