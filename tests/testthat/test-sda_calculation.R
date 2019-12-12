test_that("sda_calculation with `market` and `portfolio` returns a tibble", {
  expect_is(sda_calculation(market, portfolio), "tbl")
})

test_that("sda_calculation outputs a known value", {
  expect_known_value(
    sda_calculation(market, portfolio), "ref-sda_calculation"

  )
})

sample_market <- dplyr::tribble(
  ~Investor.Name, ~Portfolio.Name, ~Scenario, ~ScenarioGeography, ~Allocation, ~Year, ~Sector, ~Plan.Sec.EmissionsFactor, ~Scen.Sec.EmissionsFactor,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2019, "Steel", 1.11, 1.1063532,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2020, "Steel", 1.11, 1.0608054,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2021, "Steel", 1.11, 1.0152576,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2022, "Steel", 1.11, 0.9697098,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2023, "Steel", 1.11, 0.924162,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2024, "Steel", 1.11, 0.8786143,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2025, "Steel", 1.11, 0.8330665,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2026, "Steel", NA, 0.7955433,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2027, "Steel", NA, 0.7580202,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2028, "Steel", NA, 0.7204971,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2029, "Steel", NA, 0.682974,
  "Market", "GlobalMarket", "B2DS", "Global", "PortfolioWeight", 2040, "Steel", NA, 0.3663936
)

sample_portfolio <- dplyr::tribble(
  ~Investor.Name, ~Portfolio.Name, ~Scenario, ~ScenarioGeography, ~Allocation, ~Year, ~Sector, ~Plan.Sec.EmissionsFactor, ~Scen.Sec.EmissionsFactor,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2019, "Steel", 1.8, 1.1063532,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2020, "Steel", 1.8, 1.0608054,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2021, "Steel", 1.8, 1.0152576,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2022, "Steel", 1.8, 0.9697098,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2023, "Steel", 1.8, 0.924162,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2024, "Steel", 1.8, 0.8786143,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2025, "Steel", 1.8, 0.8330665,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2026, "Steel", NA, 0.7955433,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2027, "Steel", NA, 0.7580202,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2028, "Steel", NA, 0.7204971,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2029, "Steel", NA, 0.682974,
  "Investor1", "Portfolio1", "B2DS", "Global", "PortfolioWeight", 2040, "Steel", NA, 0.3663936
)

test_that("sda_calculation with another dataset returns a tibble", {
  expect_is(sda_calculation(sample_market, sample_portfolio), "tbl")
})
