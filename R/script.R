rm(list=ls())

library(r2dii)
library(readr)
library(tidyverse)

source("R/util.R")
source("R/prep_portfolio_input_check.R")

project_name <- "TEST"

set_project_paths(project_name)

set_global_parameters(file_path = paste0(par_file_path,"AnalysisParameters.yml"))

set_general_paths()

create_project_folder(project_name = project_name)

add_intial_project_files(project_name = project_name)





### Process Input Files

#### Some of these functions only need to be run once and are not portfolio specific.
#### Move them to data prep?
#### Only the fin data override piece is more "project specific" as in is updated without updates from DataStore

# Currency Data
currencies <- readRDS("data/Currencies.rda")

currencies <<- set_currency_timestamp(currencies)

# Fund Data
fund_data <- readRDS(paste0(analysis_inputs_path,"FundsData",financial_timestamp,".rda"))

fund_data <- clean_fund_data(fund_data)

fund_data <- normalise_fund_data(fund_data)



# Financial Data
fin_data_raw <- readRDS(paste0(analysis_inputs_path,"FinancialData_DataStore.rda"))

overrides <- read_csv("data/Fin-Sector-Overrides.csv")

fin_data <<- clean_fin_data(fin_data_raw, overrides)

### TEST
if (nrow(fin_data) != nrow(fin_data_raw)){stop("Additional rows added to fin data")}

# updates csv file with missing bloomberg data re funds
check_funds_wo_bbg(fund_data,fin_data)

portfolio_input_check()
