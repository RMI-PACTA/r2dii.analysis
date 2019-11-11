# FIXME: These function should implemente deprecation and live in r2dii.utils
financial_timestamp <- r2dii.utils::FINANCIAL.TIMESTAMP
ald_timestamp <- r2dii.utils::ALD.TIMESTAMP
datastore_timestamp <- r2dii.utils::DATASTORE.TIMESTAMP
# FIXME: Now `if_null = NULL`. Do we need `if_null = stop`? (ASK @Clare2D)
dataprep_timestamp <- r2dii.utils::DATAPREP.TIMESTAMP

set_global_parameters <- function(file_path) {
  cfg <- config::get(file = file_path)

  start_year <<- cfg$AnalysisPeriod$Years.Startyear
  time_horizon <<- cfg$AnalysisPeriod$Years.Horizon
  risk_year <<- cfg$AnalysisPeriod$Years.Riskyear
  additional_year <<- cfg$AnalysisPeriod$Years.Additional

  tech_list <<- cfg$Lists$Technology.List
  tech_exclude <<- cfg$Lists$Technology.Exclusion.List
  sector_list <<- cfg$Lists$TechnologyRoadmap.Sector.List
  other_sector_list <<- cfg$Lists$CO2Intensity.Sector.List

  scenario_sources_list <<- cfg$Lists$Scenario.Sources.List
  iea_scenario_list <<- cfg$Lists$IEA.Scenarios.List
  web_region_list <<- cfg$Lists$WebToolRegions
  scenario_geographies_list <<- cfg$Lists$Scenario.Geography.List

  equity_market_list <<- cfg$Lists$Equity.Market.List

  allowable_asset_list <<- cfg$Lists$AssetTypes
  if (is.null(allowable_asset_list)) {
    allowable_asset_list <<- c("Funds", "Equity", "Bonds", "Others")
  }

  global_aggregate_sector_list <<- cfg$Lists$Global.Aggregate.Sector.List
  global_aggregate_scenario_sources_list <<- cfg$Lists$Global.Aggregate.Scenario.Sources.List


  meta_investor_name <<- cfg$ComparisonBenchmarks$MetaInvestorName
  meta_portfolio_name <<- cfg$ComparisonBenchmarks$MetaPortfolioName
  inc_metaportfolio <<- cfg$ComparisonBenchmarks$CreateMetaPortfolio
  if (is.null(inc_metaportfolio)) {
    inc_metaportfolio <<- FALSE
  }


  inc_project_metaportfolio <<- cfg$ComparisonBenchmarks$CreateProjectMetaPortfolio

  if (is.null(inc_project_metaportfolio)) {
    inc_project_metaportfolio <<- FALSE
  }
  if (inc_project_metaportfolio) {
    project_meta_investor_name <<- paste0("Project ", meta_investor_name)
    project_meta_portfolio_name <<- paste0("Project ", meta_portfolio_name)
  }


  has_bv <<- cfg$Methodology$HasBookValue
  if (is.null(has_bv)) {
    has_bv <<- FALSE
    print("Warning: has_bv set to standard value (FALSE) as not defined in the parameter file")
  }

  has_risk <<- cfg$Methodology$HasRISK
  if (is.null(has_risk)) {
    has_risk <<- TRUE
    print("Warning: has_risk set to standard value (TRUE) as not defined in the parameter file")
  }

  has_map <<- cfg$Methodology$HasMAP
  if (is.null(has_map)) {
    has_map <<- TRUE
    print("Warning: has_map set to standard value (TRUE) as not defined in the parameter file")
  }

  has_sb <<- cfg$Methodology$HasSB
  if (is.null(has_sb)) {
    has_sb <<- FALSE
    print("Warning: has_sb set to standard value (FALSE) as not defined in the parameter file")
  }
}

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

  analysis_inputs_path <<- paste0(DROPBOX.PATH(), "/PortCheck/00_Data/07_AnalysisInputs/", financial_timestamp, "_", dataprep_timestamp, "/")
  data_store_path <<- paste0(DROPBOX.PATH(), "/PortCheck/00_Data/06_DataStore/F", financial_timestamp, "_A", ald_timestamp, "_export_", datastore_timestamp, "/")
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
