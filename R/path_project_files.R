# add_intial_project_files <- function(project_name) {
path_project_files <- function(project_name,
                               folder_location = r2dii.utils::path_dropbox_2dii(
                                 "Portcheck_v2",
                                 "00_Administration",
                                 "20_Input_Files")) {
  folder_location
  # # FIXME: Instead of `project_location` reuse r2dii.utils
  # input_file <- paste0(project_location, "20_Raw_Inputs/", project_name, "_Input.csv")
  # par_file <- paste0(project_location, "20_Raw_Inputs/", "ReportParameterFile.yml")
  # yml_file <- paste0(project_location, "20_Raw_Inputs/", "AnalysisParameters.yml")
  #
  # if (!file.exists(input_file)) {
  #   file.copy(paste0(folder_location, "ProjectName_Input.csv"), input_file, overwrite = F)
  # }
  #
  # if (!file.exists(par_file)) {
  #   file.copy(paste0(folder_location, "ReportParameterFile.yml"), par_file, overwrite = F)
  # }
  #
  # if (!file.exists(yml_file)) {
  #   file.copy(paste0(folder_location, "AnalysisParameters.yml"), yml_file, overwrite = F)
  # }
}
