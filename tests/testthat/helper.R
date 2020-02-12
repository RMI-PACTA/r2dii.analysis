fake_portfolio <- function(year = NULL,
                           scenario_geography = NULL,
                           scenario = NULL,
                           sector = NULL,
                           investor_name = NULL,
                           portfolio_name = NULL,
                           allocation = NULL,
                           plan_sec_emissions_factor = NULL,
                           scen_sec_emissions_factor = NULL,
                           ...) {
  tibble::tibble(
    scenario_geography = scenario_geography %||% "Global",
    scenario = scenario %||% "B2DS",
    sector = sector %||% "Steel",
    year = year %||% 2021L,
    investor_name = investor_name %||% "Market",
    portfolio_name = portfolio_name %||% "GlobalMarket",
    allocation = allocation %||% "PortfolioWeight",
    plan_sec_emissions_factor = plan_sec_emissions_factor %||% 1.0,
    scen_sec_emissions_factor = scen_sec_emissions_factor %||% 1.0,
    ...
  )
}

expect_error_free <- function(object, ...) {
  expect_error(object, regexp = NA)
}
