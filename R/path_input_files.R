#' Create paths to common input and output files
#'
#' @param parent String. Path to the directory containing `file_names`
#' @param file_names Character vector giving the name of the files in
#'   `parent`.
#' @inheritParams path_project_dirs
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' path_input_files()
#'
#' path_output_files("a-project")
#'
#' # Use fs::file_copy() to copy all files in one step
#' \dontrun{
#' fs::file_copy(
#'   path_input_files(),
#'   path_output_files("a-project"),
#'   overwrite = TRUE
#' )
#' }
#'
#' # Use `file_names` to create pahts to new files
#' path_input_files(file_names = c("new_file_1", "new_file_2"))
#' path_output_files("a-project", file_names = c("new_file_1", "new_file_2"))
#'
#' # Use `parent` to change the parent directory
#' path_input_files(parent = tempdir())
#' path_output_files("a-project", parent = tempdir())
path_input_files <- function(parent = r2dii.utils::path_dropbox_2dii(
                               "Portcheck_v2",
                               "00_Administration",
                               "20_Input_Files"
                             ),
                             file_names = get_input_file_names()) {
  fs::path(parent = parent, file_names = file_names)
}

#' @rdname path_input_files
#' @export
path_output_files <- function(project, parent = NULL, file_names = NULL) {
  path_dir <- path_project(project = project, parent = parent)
  file_names <- file_names %||% get_input_file_names()
  fs::path(path_dir, file_names)
}

#' Name of common input files
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' get_input_file_names()
get_input_file_names <- function() {
  c(
    "ProjectName_Input.csv",
    "ReportParameterFile.yml",
    "AnalysisParameters.yml"
  )
}
