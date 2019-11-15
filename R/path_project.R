#' Create paths in 2dii's dropbox under "PortCheck_v2/10_Projects"
#'
#' This function output a path inside 2dii's dropbox folder, under
#' "PortCheck_v2/10_Projects/<a directory>", where "<a directory>" is usually a
#' directory within which you may need to access multiple subdirectories. This
#' function replaces the legacy global variable `project_location`.
#'
#' @inheritParams path_project_dirs
#' @inheritParams fs::path
#'
#' @return A "character", subclass "fs_path".
#' @export
#'
#' @examples
#' path_project("a-project")
#'
#' path_project("a-project", "a", "sub", "directory")
#'
#' # Useful for tests and examples
#' path_project("a-project", "a", "sub", "directory", parent = tempdir())
#'
#' # Common paths for analyses --------------------------------------------
#' path_project("a-project", "00_Log_Files")
#' path_project("a-project", "10_Parameter_File")
#' path_project("a-project", "20_Raw_Inputs")
#' path_project("a-project", "30_Processed_Inputs")
#' path_project("a-project", "40_Results")
#' path_project("a-project", "50_Outputs")
path_project <- function(project, ..., parent = NULL) {
  ellipsis::check_dots_used()

  if (is.null(parent)) {
    return(
      r2dii.utils::dbox_port2_10proj(project, ...)
    )
  }

  fs::path(parent, project, ...)
}
