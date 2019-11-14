#' Read raw portfolio data
#'
#' @inheritParams path_project_dirs
#'
#' @return A [tibble::tibble].
#' @export
#'
#' @examples
#' if (r2dii.utils::dropbox_exists()) {
#'   read_raw_portfolio("TEST")
#' }
read_raw_portfolio <- function(project) {
  out <- NA
  inputs_path <- with_path_in_10_projects(project)("20_Raw_Inputs")

  csv_path <- dir_ls(inputs_path, regexp = glue("{project}_Input.csv"))
  abort_if_multiple_paths(csv_path)

  if (identical(length(csv_path), 1L)) {
    out <- readr::read_csv(csv_path)
    if (has_expected_structure(out)) {
      return(out)
    }

    out <- readr::read_delim(csv_path, delim = ";")
    if (has_expected_structure(out)) {
      return(out)
    }
  }

  txt_path <- dir_ls(inputs_path, regexp = glue("{project}_Input.txt"))
  abort_if_multiple_paths(txt_path)

  if (identical(length(txt_path), 1L)) {
    enc <- readr::guess_encoding(txt_path)$encoding[1]
    out <- read.table(txt_path, sep = ",", header = TRUE, fileEncoding = enc)
    if (has_expected_structure(out)) {
      return(out)
    }

    out <- read.table(txt_path, sep = "\t", header = TRUE, fileEncoding = enc)
    if (has_expected_structure(out)) {
      return(out)
    }

    out <- read.table(txt_path, sep = ";", header = TRUE, fileEncoding = enc)
    if (has_expected_structure(out)) {
      return(out)
    }
  }

  abort("Can't read porfolio input file")
}

has_expected_structure <- function(data) {
  isTRUE(ncol(data) > 1L)
}

abort_if_multiple_paths <- function(path) {
  if (length(path) >= 2L) {
    abort(glue("Can't read more than one file but found {length(path)}."))
  }
}
