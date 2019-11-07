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

###




portfolio_input_check <- function(){

  portfolio <- read_raw_portfolio_file(project_name)

  portfolio <- clean_colnames_portfolio_input_file(portfolio)

  portfolio <- rename_portfolio_columns(portfolio)

  portfolio <- clear_portfolio_input_blanks(portfolio)

  portfolio <- add_meta_portfolio(inc_metaportfolio, inc_project_metaportfolio)

  portfolio <- add_holding_id(portfolio)

  portfolio <- check_missing_cols(portfolio)

  portfolio <- clean_portfolio_col_types(portfolio)

  portfolio <- convert_currencies(portfolio, currencies)

  cols_portfolio_no_bbg <- colnames(portfolio)
  cols_funds <- c("direct_holding", "fund_isin","original_value_usd")

  # Add financial data
  # Merges in the clean data and calculates the marketvalue and number of shares
  portfolio <- add_fin_data(portfolio, fin_data)

  portfolio <- calculate_value_usd_with_fin_data(portfolio)

  original_value_usd = sum(portfolio$value_usd, na.rm = T)

  # identify funds in the portfolio
  fund_portfolio <- identify_fund_portfolio(portfolio)

  # Creates the fund_portfolio to match the original portfolio
  fund_portfolio <- calculate_fund_portfolio(fund_portfolio, fund_data)

  # Merges in the bbg data to the fund portfolio
  fund_portfolio <- add_fin_data(fund_portfolio, fin_data)

  # add fund_portfolio and check that the total value is the same
  portfolio_total <- add_fund_portfolio(portfolio, fund_portfolio)

  if(round(sum(portfolio_total$value_usd, na.rm = T),1) != round(original_value_usd,1)){stop("Fund Portfolio introducing errors in total value")}


  ### TO DO
  # summarise fund results
  # create summary of results
  #   use summary that already should exist
  summarise_fund_info <- function(fund_data){




  }
  #   use summary data frame that's already created
  #   check for funds with no bbg info
  identify_missing_funds <- function(portfolio_total, fund_data){}
  ###

  # Keep going with portfolio_total

  ### FLAGS/Exclusions

  portfolio_total <- check_isin_format(portfolio_total)
  portfolio_total <- check_missing_currency(portfolio_total)
  portfolio_total <- check_valid_input_value(portfolio_total)
  portfolio_total <- check_bloomberg_data(portfolio_total)

  portfolio_total <- add_flags(portfolio_total)
  portfolio_total <- overall_validity_flag(portfolio_total)


  portfolio_overview <- portfolio_summary(portfolio_total)
  # Missing is still flags about whether an isin is linked to ald or data or not.
  # these tend to confuse, without more specific information about what sector the asset level data is in
  # option to add in technology information from datastore?


  ### Create portfolios to print.

  ### Revenue Data
  # This needs to be an optional






}


