fake_portfolio <- function(year = NULL,
                           ScenarioGeography = NULL,
                           Scenario = NULL,
                           Sector = NULL,
                           investor_name = NULL,
                           Portfolio.Name = NULL,
                           Allocation = NULL,
                           plan_sec_emissions_factor = NULL,
                           scen_sec_emissions_factor = NULL,
                           ...) {
  tibble::tibble(
    ScenarioGeography = ScenarioGeography %||% "Global",
    Scenario = Scenario %||% "B2DS",
    Sector = Sector %||% "Steel",
    year = year %||% 2021L,
    investor_name = investor_name %||% "Market",
    Portfolio.Name = Portfolio.Name %||% "GlobalMarket",
    Allocation = Allocation %||% "PortfolioWeight",
    plan_sec_emissions_factor = plan_sec_emissions_factor %||% 1.0,
    scen_sec_emissions_factor = scen_sec_emissions_factor %||% 1.0,
    ...
  )
}

expect_error_free <- function(object, ...) {
  expect_error(object, regexp = NA)
}
