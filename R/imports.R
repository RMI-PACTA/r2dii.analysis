#' @importFrom dplyr filter bind_rows case_when distinct group_by group_vars
#' @importFrom dplyr inner_join mutate rename select summarise ungroup
#' @importFrom dplyr distinct filter if_else inner_join mutate rename right_join
#' @importFrom rlang %||%
NULL

# FIXME: This is a hack. Best practice is to use e.g. .data$Allocation
# FIXME: Some items of this list may not be column names but functions or
# variables. Search for them and fix as appropriate.
globalVariables(
  c(
    "Allocation",
    "climate_rel_cat",
    "exposure_climate_sectors",
    "group_vars_weight",
    "interval",
    "Investor.Name",
    "mapped_sector",
    "metric",
    "metric_group_vars",
    "metric_port",
    "metric_sector",
    "Plan.Alloc.WtTechProd",
    "plan_tech_prod",
    "Portfolio.Name",
    "relation",
    "Scen.Alloc.WtTechProd",
    "scen_plan_prod_diff",
    "scen_tech_prod",
    "scen_tech_prod_lower",
    "scen_tech_prod_lower_bound",
    "scen_tech_prod_reference",
    "scen_tech_prod_upper",
    "scen_tech_prod_upper_bound",
    "Scenario",
    "scenario_relationships",
    "ScenarioGeography",
    "Sector",
    "sector_value_weight",
    "sector_weight",
    "tech_sector_weighting",
    "Technology",
    "technology_weight",
    "temp_lower",
    "temp_lower_range",
    "temp_reference",
    "temp_upper",
    "temp_upper_range",
    "temperature_range",
    "value_usd_sector",
    "ValueUSD",
    "ValueUSD_port",
    "Year"
  )
)
