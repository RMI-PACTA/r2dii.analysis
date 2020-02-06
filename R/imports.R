#' @importFrom dplyr filter bind_rows case_when distinct group_by group_vars
#' @importFrom dplyr inner_join mutate rename select summarise ungroup
#' @importFrom dplyr distinct filter if_else inner_join mutate rename right_join
#' @importFrom rlang %||% syms
NULL

# FIXME: This is a hack. Best practice is to use e.g. .data$Allocation
# FIXME: Some items of this list may not be column names but functions or
# variables. Search for them and fix as appropriate.
globalVariables(
  c(
    "Allocation",
    "Asset.Type",
    "Investor.Name",
    "Plan.Alloc.WtTechProd",
    "Portfolio.Name",
    "Scen.Alloc.WtTechProd",
    "Scenario",
    "ScenarioGeography",
    "Sector",
    "Technology",
    "ValueUSD",
    "ValueUSD_port",
    "Year",
    "climate_rel_cat",
    "exposure_climate_sectors",
    "financial_instument_value_weight",
    "interval",
    "mapped_sector",
    "metric",
    "metric_Asset.Type",
    "metric_port",
    "metric_sector",
    "plan_tech_prod",
    "relation",
    "scen_plan_prod_diff",
    "scen_tech_prod",
    "scen_tech_prod_lower",
    "scen_tech_prod_lower_bound",
    "scen_tech_prod_reference",
    "scen_tech_prod_upper",
    "scen_tech_prod_upper_bound",
    "sector_value_weight",
    "sector_weight",
    "sector_weightings",
    "technology_weight",
    "temp_lower",
    "temp_lower_range",
    "temp_reference",
    "temp_upper",
    "temp_upper_range",
    "temperature",
    "temperature_range",
    "value_usd_Asset.Type",
    "value_usd_sector"
  )
)
