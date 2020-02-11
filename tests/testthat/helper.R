fake_portfolio <- function(Year = NULL,
                           ScenarioGeography = NULL,
                           Scenario = NULL,
                           Sector = NULL,
                           Investor.Name = NULL,
                           Portfolio.Name = NULL,
                           Allocation = NULL,
                           plan_sec_emissions_factor = NULL,
                           Scen.Sec.EmissionsFactor = NULL,
                           ...) {
  tibble::tibble(
    ScenarioGeography = ScenarioGeography %||% "Global",
    Scenario = Scenario %||% "B2DS",
    Sector = Sector %||% "Steel",
    Year = Year %||% 2021L,
    Investor.Name = Investor.Name %||% "Market",
    Portfolio.Name = Portfolio.Name %||% "GlobalMarket",
    Allocation = Allocation %||% "PortfolioWeight",
    plan_sec_emissions_factor = plan_sec_emissions_factor %||% 1.0,
    Scen.Sec.EmissionsFactor = Scen.Sec.EmissionsFactor %||% 1.0,
    ...
  )
}

expect_error_free <- function(object, ...) {
  expect_error(object, regexp = NA)
}
