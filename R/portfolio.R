#' TODO \@vintented: Add title
#'
#' TODO \@vintented: Add description
#'
#' @family demo datasets
#' @source \@vintented via <https://github.com/2DegreesInvesting/
#' r2dii.analysis/issues/6#issuecomment-561543097>.
"portfolio"

#' @rdname portfolio
#' @export
sample_portfolio <- tibble::tribble(
  ~Investor.Name, ~Portfolio.Name, ~Scenario, ~ScenarioGeography,       ~Allocation, ~Year, ~Sector, ~Plan.Sec.EmissionsFactor, ~Scen.Sec.EmissionsFactor,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2019, "Steel",                        1.8,                  1.1063532,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2020, "Steel",                        1.8,                  1.0608054,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2021, "Steel",                        1.8,                  1.0152576,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2022, "Steel",                        1.8,                  0.9697098,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2023, "Steel",                        1.8,                   0.924162,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2024, "Steel",                        1.8,                  0.8786143,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2025, "Steel",                        1.8,                  0.8330665,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2026, "Steel",                         NA,                  0.7955433,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2027, "Steel",                         NA,                  0.7580202,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2028, "Steel",                         NA,                  0.7204971,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2029, "Steel",                         NA,                   0.682974,
  "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2040, "Steel",                         NA,                  0.3663936
)
