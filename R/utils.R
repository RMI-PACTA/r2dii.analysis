# Use parameters from the configuration file ------------------------------

project_meta_investor_name <- function(inc_project_metaportfolio,
                                       file = r2dii.utils::get_config()) {
  if (is.logical(inc_project_metaportfolio) && inc_project_metaportfolio) {
    paste0("Project ", meta_investor_name(file = file))
  } else {
    NULL
  }
}

project_meta_portfolio_name <- function(inc_project_metaportfolio,
                                        file = r2dii.utils::get_config()) {
  if (is.logical(inc_project_metaportfolio) && inc_project_metaportfolio) {
    paste0("Project ", meta_portfolio_name(file = file))
  } else {
    NULL
  }
}

# Other stuff -------------------------------------------------------------

set_project_paths <- function(project_name) {
  portcheck_v2_path <<- paste0(path_dropbox_2dii(), "/PortCheck_v2")
  project_location <<- paste0(portcheck_v2_path, "/10_Projects/", project_name, "/")

  log_path <<- paste0(project_location, "00_Log_Files/")
  par_file_path <<- paste0(project_location, "10_Parameter_File/")
  raw_input_path <<- paste0(project_location, "20_Raw_Inputs/")
  proc_input_path <<- paste0(project_location, "30_Processed_Inputs/")
  results_path <<- paste0(project_location, "40_Results/")
  outputs_path <<- paste0(project_location, "50_Outputs/")
}

set_git_path <- function() {
  if (rstudioapi::isAvailable()) {
    git_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
  } else {
    git_path <- getwd()
  }

  git_path <- gsub("\u00C2", "", git_path)
  git_path <- paste0(git_path, "/")

  git_path
}

data_path <- function(...) {
  r2dii.utils::path_dropbox_2dii("PortCheck", "00_Data", ...)
}

set_general_paths <- function() {
  git_path <- set_git_path()

  analysis_inputs_path <<- paste0(DROPBOX.PATH(), "/PortCheck/00_Data/07_AnalysisInputs/", financial_timestamp(), "_", dataprep_timestamp(), "/")
  data_store_path <<- paste0(DROPBOX.PATH(), "/PortCheck/00_Data/06_DataStore/F", financial_timestamp(), "_A", ald_timestamp(), "_export_", datastore_timestamp(), "/")
}

create_project_folder <- function(project_name) {
  folder_location <- paste0(portcheck_v2_path, "/00_Administration/10_Folder_Structures/StartFolders")

  # Create the new project folder
  if (dir.exists(project_location)) {
    print("Project Folder Already Exists")
  } else {
    dir.create(project_location)
    a <- list.dirs(folder_location)
    b <- basename(a)[-1]
    c <- paste0(project_location, b)
    lapply(c, function(x) dir.create(x))
  }
}

add_intial_project_files <- function(project_name) {
  folder_location <- paste0(DROPBOX.PATH(), "Portcheck_v2/00_Administration/20_Input_Files/")

  input_file <- paste0(project_location, "20_Raw_Inputs/", project_name, "_Input.csv")
  par_file <- paste0(project_location, "20_Raw_Inputs/", "ReportParameterFile.yml")
  yml_file <- paste0(project_location, "20_Raw_Inputs/", "AnalysisParameters.yml")

  if (!file.exists(input_file)) {
    file.copy(paste0(folder_location, "ProjectName_Input.csv"), input_file, overwrite = F)
  }

  if (!file.exists(par_file)) {
    file.copy(paste0(folder_location, "ReportParameterFile.yml"), par_file, overwrite = F)
  }

  if (!file.exists(yml_file)) {
    file.copy(paste0(folder_location, "AnalysisParameters.yml"), yml_file, overwrite = F)
  }
}


first_char_up <- function(x) {
  x <- paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
  x
}

clean_punctuation <- function(x) {
  x <- gsub("\u00F3", "o", x)
  x <- gsub("&", " and ", x)
  x <- gsub("\u00E1", "a", x)
  x <- gsub("/", " ", x)
  x <- gsub("\u00E4", "ae", x)
  x <- gsub("\u00F6", "oe", x)
  x <- gsub("\u00FC", "ue", x)
  x <- gsub("\u00C4", "Ae", x)
  x <- gsub("\u00D6", "Oe", x)
  x <- gsub("\u00DC", "Ue", x)

  x
}

# Checks whether a variable is a dataframe. Considers also logicals and null values.
data_check <- function(df) {
  if (is.data.frame(df)) {
    if (nrow(df) > 0) {
      check <- TRUE
    } else {
      check <- FALSE
    }
  } else {
    check <- FALSE
  }

  return(check)
}

# Checks whether a value is null or blank
is_blank_na <- function(x) {
  if (is.na(x) | x == "") {
    flag <- TRUE
  } else {
    flag <- FALSE
  }
  flag
}
