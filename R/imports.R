#' @importFrom tibble as_tibble
#' @importFrom dplyr case_when distinct filter group_by if_else left_join
#' @importFrom dplyr mutate select
#' @importFrom rlang %||%
#' @importFrom glue glue
NULL

globalVariables(".data")

# FIXME: The lowercase names should be defined in r2dii.utils (r2dii.utils#6)
financial_timestamp <- r2dii.utils::FINANCIAL.TIMESTAMP
ald_timestamp <- r2dii.utils::ALD.TIMESTAMP
datastore_timestamp <- r2dii.utils::DATASTORE.TIMESTAMP
dataprep_timestamp <- r2dii.utils::DATAPREP.TIMESTAMP
start_year <- r2dii.utils::START.YEAR
time_horizon <- r2dii.utils::TIME.HORIZON
risk_year <- r2dii.utils::RISK.YEAR
additional_year <- r2dii.utils::ADDITIONAL.YEAR
tech_list <- r2dii.utils::TECH.LIST
tech_exclude <- r2dii.utils::TECH.EXCLUDE
sector_list <- r2dii.utils::SECTOR.LIST
other_sector_list <- r2dii.utils::OTHER.SECTOR.LIST
scenario_sources_list <- r2dii.utils::SCENARIO.SOURCES.LIST
iea_scenario_list <- r2dii.utils::IEA.SCENARIO.LIST
web_region_list <- r2dii.utils::WEB.REGION.LIST
scenario_geographies_list <- r2dii.utils::SCENARIO.GEOGRAPHIES.LIST
equity_market_list <- r2dii.utils::EQUITY.MARKET.LIST
global_aggregate_sector_list <- r2dii.utils::GLOBAL.AGGREGATE.SECTOR.LIST
global_aggregate_scenario_sources_list <-
  r2dii.utils::GLOBAL.AGGREGATE.SCENARIO.SOURCES.LIST
meta_investor_name <- r2dii.utils::META.INVESTOR.NAME
meta_portfolio_name <- r2dii.utils::META.PORTFOLIO.NAME
has_risk <- r2dii.utils::HasRISK
has_bv <- r2dii.utils::HasBV
has_map <- r2dii.utils::HasMAP
has_sb <- r2dii.utils::HasSB
# FIXME: The version by @Clare2D seems different.
datastore_path <- r2dii.utils::DATA.STORE.PATH
analysis_inputs_path <- r2dii.utils::ANALYSIS.INPUTS.PATH
data_path <- r2dii.utils::dbox_port_00
