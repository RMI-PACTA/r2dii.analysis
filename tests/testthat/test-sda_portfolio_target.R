library(dplyr)
library(r2dii.utils)

# Avoid warning for using a toy configuration file
setup({
  op <- options(r2dii_config = example_config("config_demo.yml"))
  on.exit(options(op))
})

test_that("sda_portfolio_target errors gracefully with obviously wrong data", {
  expect_error(
    sda_portfolio_target(1, portfolio), "data.frame.* is not TRUE"
  )
  expect_error(
    sda_portfolio_target(market, 1), "data.frame.* is not TRUE"
  )

  bad_market <- rename(market, bad = .data$Year)
  expect_error(
    sda_portfolio_target(bad_market, portfolio, target_year = "2040"),
    "must have.*Year"
  )

  bad_portfolio <- rename(portfolio, bad = .data$Plan.Sec.EmissionsFactor)
  expect_error(
    sda_portfolio_target(market, bad_portfolio, target_year = "2040"),
    "must have.*Plan.Sec.EmissionsFactor"
  )
})

test_that("sda_portfolio_target errors gracefully with wrong ref_scenario", {
  expect_error(
    sda_portfolio_target(
      market, portfolio, target_year = "2040",
      ref_scenario = "bad"
    ),
    "bad.*Must be one of"
  )

  expect_error(
    sda_portfolio_target(
      market, portfolio, target_year = "2040",
      ref_scenario = c("B2DS", "other")
    ),
    "length_1.*not TRUE"
  )
})

test_that("sda_portfolio_target errors gracefully with wrong ref_geography", {
  expect_error(
    sda_portfolio_target(
      market, portfolio, target_year = "2040",
      ref_geography = "bad"
    ),
    "bad.*Must be one of"
  )

  expect_error(
    sda_portfolio_target(
      market, portfolio, target_year = "2040",
      ref_geography = c("B2DS", "other"),
    ),
    "length_1.*not TRUE"
  )
})

test_that("sda_portfolio_target errors with bad ref_sector", {
  expect_error(
    sda_portfolio_target(
      market, portfolio, target_year = "2040",
      ref_sector = "bad"
    ), "is not TRUE"
  )
})

test_that("sda_portfolio_target errors with bad 'year' arguments", {
  sda_portfolio_target_partial <- purrr::partial(
    .f = sda_portfolio_target,
    market_data = market,
    port_data = portfolio,
    ref_sector = "Steel"
  )

  expect_error(
    sda_portfolio_target_partial(start_year = "bad", target_year = "2040"),
    "is not TRUE"
  )
  expect_error(sda_portfolio_target_partial(target_year = "bad"), "is not TRUE")
})

test_that("sda_portfolio_target takes 'year' arguments of length-1", {
  sda_portfolio_target_partial <- purrr::partial(
    .f = sda_portfolio_target,
    market_data = market,
    port_data = portfolio,
    ref_sector = "Steel"
  )

  expect_error(
    sda_portfolio_target_partial(start_year = 2019:2020, target_year = "2040"),
    "is not TRUE"
  )
  expect_error(
    sda_portfolio_target_partial(target_year = 2040:2041), "is not TRUE"
  )
})

test_that("sda_portfolio_target takes chr, num, or int 'year' arguments", {
  sda_portfolio_target_partial <- purrr::partial(
    .f = sda_portfolio_target,
    market_data = market,
    port_data = portfolio,
    ref_sector = "Steel",
    target_year = "2040"
  )
  lgl <- TRUE
  expect_error(
    sda_portfolio_target_partial(start_year = lgl),
    "character.*numeric.*is not TRUE"
  )
  expect_equal(
    sda_portfolio_target_partial(start_year = "2019"),
    sda_portfolio_target_partial(start_year = 2019),
  )
  expect_equal(
    sda_portfolio_target_partial(start_year = "2019"),
    sda_portfolio_target_partial(start_year = 2019L),
  )

  sda_portfolio_target_partial <- purrr::partial(
    .f = sda_portfolio_target,
    market_data = market,
    port_data = portfolio,
    ref_sector = "Steel"
  )
  lgl <- TRUE
  expect_error(
    sda_portfolio_target_partial(target_year = lgl),
    "character.*numeric.*is not TRUE"
  )
  expect_equal(
    sda_portfolio_target_partial(target_year = 2040),
    sda_portfolio_target_partial(target_year = 2040L)
  )
  expect_equal(
    sda_portfolio_target_partial(target_year = 2040),
    sda_portfolio_target_partial(target_year = "2040")
  )
})

test_that("sda_portfolio_target warns `ref_sector`s missing in `port_data`", {
  expect_warning(
    sda_portfolio_target(
      market,
      port_data = filter(portfolio, Sector == "Steel"),
      ref_sector = c("Steel", "Power")
    ),
    "missing.*Power"
  )
})

test_that("sda_portfolio_target errors clearly if config has null start_year", {
  config_with_some_start_year <- example_config("config_demo.yml")
  expect_false(is.null(START.YEAR(file = config_with_some_start_year)))
  expect_error(
    withr::with_options(
      list(r2dii_config = config_with_some_start_year),
      sda_portfolio_target(
        market, portfolio, ref_sector = "Steel", target_year = "2040"
      )
    ),
    NA
  )

  config_with_null_start_year <- example_config("config-toy.yml")
  expect_true(is.null(START.YEAR(file = config_with_null_start_year)))
  expect_error(
    withr::with_options(
      list(r2dii_config = config_with_null_start_year),
      sda_portfolio_target(market, portfolio, target_year = "2040")
    ),
    "start_year.*can't be NULL"
  )
})

test_that("sda_portfolio_target w/ `market` and `portfolio` returns a tibble", {
  expect_is(
    sda_portfolio_target(
      market, portfolio, ref_sector = "Steel", target_year = "2040"
    ),
    "tbl"
  )
})

test_that("sda_portfolio_target outputs a known value", {
  expect_known_value(
    sda_portfolio_target(
      market, portfolio,
      ref_sector = "Steel",
      target_year = "2040"
    ),
    "ref-sda_portfolio_target",
    update = FALSE
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

test_that("sda_portfolio_target with another dataset returns a tibble", {
  expect_is(
    sda_portfolio_target(
      sample_market, sample_portfolio,
      ref_sector = "Steel",
      target_year = "2040"
    ),
    "tbl"
  )
})
