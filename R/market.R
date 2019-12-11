#' TODO \@vintented: Add title
#'
#' TODO \@vintented: Add description
#'
#' @family demo datasets
#' @source \@vintented via <https://github.com/2DegreesInvesting/
#' r2dii.analysis/issues/6#issuecomment-561543097>.
"market"

#' @rdname market
#' @export
sample_market <- tibble::tribble(
  ~Investor.Name, ~Portfolio.Name, ~Scenario, ~ScenarioGeography,       ~Allocation, ~Year, ~Sector, ~Plan.Sec.EmissionsFactor, ~Scen.Sec.EmissionsFactor,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2019, "Steel",                       1.11,                  1.1063532,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2020, "Steel",                       1.11,                  1.0608054,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2021, "Steel",                       1.11,                  1.0152576,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2022, "Steel",                       1.11,                  0.9697098,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2023, "Steel",                       1.11,                   0.924162,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2024, "Steel",                       1.11,                  0.8786143,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2025, "Steel",                       1.11,                  0.8330665,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2026, "Steel",                         NA,                  0.7955433,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2027, "Steel",                         NA,                  0.7580202,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2028, "Steel",                         NA,                  0.7204971,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2029, "Steel",                         NA,                   0.682974,
  "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2040, "Steel",                         NA,                  0.3663936
)
