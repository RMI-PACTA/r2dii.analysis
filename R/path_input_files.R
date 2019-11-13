#' Create paths to common input files
#'
#' @param path_dir String. Path to the directory containing `file_names`
#' @param file_names Character vector giving the name of the files in
#'   `path_dir`.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' path_input_files()
#'
#' # The argument defaults should be okay but you can change them if needed
#' # This is useful if the name of a directory changes
#' path_input_files(file_names = c("new_file_1", "new_file_2"))
#' # This is useful for tests and examples
#' path_input_files(path_dir = tempdir())
path_input_files <- function(path_dir = r2dii.utils::path_dropbox_2dii(
                               "Portcheck_v2",
                               "00_Administration",
                               "20_Input_Files"),
                             file_names = c(
                               "ProjectName_Input.csv",
                               "ReportParameterFile.yml",
                               "AnalysisParameters.yml"
                             )) {
  fs::path(path_dir = path_dir, file_names = file_names)
}



path_project_files <- function(project_name) {
  folder_location

  path_project <- with_path_in_10_projects(project_name)

  input_file <- path_project("20_Raw_Inputs", glue("{project_name}_Input.csv"))
  if (!file.exists(input_file)) {
    file.copy(paste0(folder_location, "ProjectName_Input.csv"), input_file, overwrite = F)
  }

  par_file <- path_project("20_Raw_Inputs", "ReportParameterFile.yml")
  if (!file.exists(par_file)) {
    file.copy(paste0(folder_location, "ReportParameterFile.yml"), par_file, overwrite = F)
  }

  yml_file <- path_project("20_Raw_Inputs", "AnalysisParameters.yml")
  if (!file.exists(yml_file)) {
    file.copy(paste0(folder_location, "AnalysisParameters.yml"), yml_file, overwrite = F)
  }
}
