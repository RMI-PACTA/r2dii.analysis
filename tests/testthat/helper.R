fake_portfolio <- function(ScenarioGeography = NULL,
                           Scenario = NULL,
                           Sector = NULL,
                           Year = NULL,
                           Investor.Name = NULL,
                           Portfolio.Name = NULL,
                           Allocation = NULL,
                           Plan.Sec.EmissionsFactor = NULL,
                           Scen.Sec.EmissionsFactor = NULL,
                           ...) {
  tibble::tibble(
    ScenarioGeography = ScenarioGeography %||% "Global",
    Scenario = Scenario %||% "B2DS",
    Sector = Sector %||% "Steel",
    Year = Year %||% 2021L:2022,
    Investor.Name = Investor.Name %||% "Market",
    Portfolio.Name = Portfolio.Name %||% "GlobalMarket",
    Allocation = Allocation %||% "PortfolioWeight",
    Plan.Sec.EmissionsFactor = Plan.Sec.EmissionsFactor %||% 1.0,
    Scen.Sec.EmissionsFactor = Scen.Sec.EmissionsFactor %||% 1.0:2.0,
    ...
  )
}

