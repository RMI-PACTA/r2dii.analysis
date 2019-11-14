#' Facilitate creating paths in 2dii's dropbox under "PortCheck_v2/10_Projects"
#'
#' This is a function factory to create functions which output a path inside
#' 2dii's dropbox folder, under "PortCheck_v2/10_Projects/<a directory>", where
#' "<a directory>" is usually a directory within which you may need to access
#' multiple subdirectories. Use this function to access those multiple
#' subdirectories on the fly, which avoids storing variables in the global
#' environment and reduces duplication. This function replaces the legacy
#' global variable `project_location`.
#'
#' @param directory The name
#'
#' @return A function.
#' @export
#'
#' @examples
#' path_analysis <- with_path_in_10_projects("my-project")
#' path_analysis("00_Log_Files")
#' path_analysis("10_Parameter_File")
#' path_analysis("20_Raw_Inputs")
#' path_analysis("30_Processed_Inputs")
#' path_analysis("40_Results")
#' path_analysis("50_Outputs")
with_path_in_10_projects <- function(directory) {
  force(directory)

  function(...) {
    r2dii.utils::dbox_port2_10proj(directory, ...)
  }
}
