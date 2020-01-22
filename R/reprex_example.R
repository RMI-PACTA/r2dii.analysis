library(tidyverse)

temp <- calculate_temperature_indicator(
  input_results = sample_results,
  upper_temp_threshold = 6,
  lower_temp_threshold = 1.5,
  start_year = 2019,
  time_horizon = 5,
  allocation = "PortfolioWeight",
  production_type = "absolute",
  group_vars = c(
    "Investor.Name",
    "Portfolio.Name",
    "Asset.Type"
  ),
  scenario_relationships = scenario_relationships
)

temp_rolled_up <- apply_influencemap_portfolio_weighting(
  input_results = temp,
  input_audit = sample_audit,
  metric_name = "temperature",
  group_vars = c(
    "Investor.Name",
    "Portfolio.Name",
    "Asset.Type"),
  sector_weightings = tech_sector_weighting
)
