library(dplyr)
library(r2dii.utils)

fake_market <- fake_portfolio <- fake_portfolio(2021:2022)

if (hasName(market, "Plan.Sec.EmissionsFactor")) {
  market$plan_sec_emissions_factor <- market$Plan.Sec.EmissionsFactor
  market$Plan.Sec.EmissionsFactor <- NULL
}
if (hasName(portfolio, "Plan.Sec.EmissionsFactor")) {
  portfolio$plan_sec_emissions_factor <- portfolio$Plan.Sec.EmissionsFactor
  portfolio$Plan.Sec.EmissionsFactor <- NULL
}

if (hasName(market, "Scen.Sec.EmissionsFactor")) {
  market$scen_sec_emissions_factor <- market$Scen.Sec.EmissionsFactor
  market$Scen.Sec.EmissionsFactor <- NULL
}
if (hasName(portfolio, "Scen.Sec.EmissionsFactor")) {
  portfolio$scen_sec_emissions_factor <- portfolio$Scen.Sec.EmissionsFactor
  portfolio$Scen.Sec.EmissionsFactor <- NULL
}

market <- dplyr::rename(market, year = .data$Year)
portfolio <- dplyr::rename(portfolio, year = .data$Year)

market <- dplyr::rename(market, investor_name = .data$Investor.Name)
portfolio <- dplyr::rename(portfolio, investor_name = .data$Investor.Name)

market <- dplyr::rename(market, portfolio_name = .data$Portfolio.Name)
portfolio <- dplyr::rename(portfolio, portfolio_name = .data$Portfolio.Name)

market <- dplyr::rename(market, allocation = .data$Allocation)
portfolio <- dplyr::rename(portfolio, allocation = .data$Allocation)

market <- dplyr::rename(market,       sector = .data$Sector)
portfolio <- dplyr::rename(portfolio, sector = .data$Sector)

market <- dplyr::rename(market,       scenario = .data$Scenario)
portfolio <- dplyr::rename(portfolio, scenario = .data$Scenario)

market <- dplyr::rename(market,       scenario_geography = .data$ScenarioGeography)
portfolio <- dplyr::rename(portfolio, scenario_geography = .data$ScenarioGeography)



test_that("errors gracefully with obviously wrong data", {
  expect_error(sda_portfolio_target(1, portfolio), "data.frame.* is not TRUE")
  expect_error(sda_portfolio_target(fake_market, 1), "data.frame.* is not TRUE")

  bad_market <- rename(fake_market, bad = .data$year)
  expect_error(
    class = "missing_names",
    sda_portfolio_target(bad_market, portfolio)
  )

  bad_portfolio <- rename(portfolio, bad = .data$plan_sec_emissions_factor)
  expect_error(
    class = "missing_names",
    sda_portfolio_target(fake_market, bad_portfolio)
  )
})

test_that("errors gracefully with bad scenario", {
  expect_error(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      scenario = "bad"
    ),
    "bad.*Must be one of"
  )

  expect_error(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      scenario = c("B2DS", "other")
    ),
    "length_1.*not TRUE"
  )
})

test_that("errors gracefully with bad geography", {
  expect_error(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      geography = "bad"
    ),
    "bad.*Must be one of"
  )

  expect_error(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      geography = c("B2DS", "other")
    ),
    "length_1.*not TRUE"
  )
})

test_that("errors with bad sector", {
  expect_error(
    sda_portfolio_target(fake_market, fake_portfolio, sector = "bad"),
    "is not TRUE"
  )
})

test_that("errors with bad 'year' arguments", {
  expect_error(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = "bad"
    ),
    "is not TRUE"
  )

  expect_error(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = "2021",
      target_year = "bad"
    ),
    "is not TRUE"
  )
})

test_that("errors if 'year' arguments are not of length-1", {
  expect_error(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = 2021:2022
    ),
    "is not TRUE"
  )
  expect_error(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = 2021,
      target_year = 2040:2041
    ),
    "is not TRUE"
  )
})

test_that("takes chr, num, or int 'year' arguments", {
  expected <- sda_portfolio_target(
    fake_market, fake_portfolio,
    sector = "Steel",
    start_year = 2021L,
    target_year = 2022L,
  )

  expect_equal(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = "2021",
      target_year = "2022",
    ),
    expected
  )

  expect_equal(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = 2021,
      target_year = 2022,
    ),
    expected
  )
})

test_that("uses start_year from configuration file", {
  start_year <- r2dii.utils::START.YEAR(example_config("config_demo.yml"))
  market <- portfolio <- fake_portfolio(year = c(start_year, start_year + 1))

  expect_error_free(
    sda_portfolio_target(
      market, portfolio,
      start_year = start_year,
      sector = "Steel"
    )
  )
})

test_that("warns `sector`s not in both datasets", {
  expect_warning(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      start_year = 2021,
      target_year = 2022,
      sector = c("Steel", "Power", "Other")
    ),
    "Skipping.*Power, Other."
  )
})

test_that("errors clearly if config has null start_year", {
  config_with_some_start_year <- example_config("config_demo.yml")

  expect_false(is.null(START.YEAR(file = config_with_some_start_year)))

  expect_error_free(
    withr::with_options(
      list(r2dii_config = config_with_some_start_year),
      sda_portfolio_target(market, portfolio, sector = "Steel")
    )
  )

  config_with_null_start_year <- example_config("config-toy.yml")

  expect_true(is.null(START.YEAR(file = config_with_null_start_year)))

  expect_error(
    withr::with_options(
      list(r2dii_config = config_with_null_start_year),
      sda_portfolio_target(market, portfolio, sector = "Steel")
    ),
    "start_year.*can't be NULL"
  )
})

test_that("outputs a known value", {
  expect_known_value(
    sda_portfolio_target(market, portfolio, sector = "Steel", start_year = 2019),
    "ref-sda_portfolio_target",
    update = FALSE
  )
})

test_that("uses max target_year in all market-sector (#13)", {
  # https://github.com/2DegreesInvesting/r2dii.analysis/issues/13
  market <- portfolio <- fake_portfolio(
    sector = c("Steel", "Steel", "Power"),
    year = c(2019, 2020, 2019)
  )

  expect_error(
    sda_portfolio_target(
      market, portfolio,
      sector = c("Steel", "Power"),
      start_year = 2019,
      target_year = 2020 # market data has no Power data for 2020
    ),
    "is_target_year_shared_across_sectors is not TRUE"
  )

  expect_error_free(
    sda_portfolio_target(
      market, portfolio,
      sector = c("Steel", "Power"),
      start_year = 2019,
      target_year = 2019 # market has both Steel and Power data for 2019
    )
  )

  expect_error_free(
    sda_portfolio_target(
      market, portfolio,
      sector = "Steel",
      start_year = 2019,
      target_year = 2020 # market has Steel data for 2019, Power is unused
    )
  )
})

test_that("errors if sector is missing from `portfolio`", {
  fake_portfolio$sector <- NULL

  expect_error(
    class = "missing_names",
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = 2020,
      target_year = 2021
    )
  )
})

test_that("errors if sector is missing from market", {
  fake_market$sector <- NULL

  expect_error(
    class = "missing_names",
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = 2020,
      target_year = 2021
    )
  )
})

test_that("passes w/ bad scenario in `portfolio`", {
  fake_portfolio$scenario <- "bad"

  expect_error_free(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = 2021,
      target_year = 2022,
      geography = "Global",
    )
  )
})

test_that("passes w/ bad scenario in `market`", {
  fake_market$scenario <- "bad"

  expect_error_free(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = 2021,
      target_year = 2022,
      geography = "Global",
    )
  )
})

test_that("passes w/ bad geography in `market`", {
  fake_market$scenario_geography <- "bad"

  expect_error_free(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = 2021,
      target_year = 2022,
      geography = "Global",
    )
  )
})

test_that("passes w/ bad geography in `portfolio`", {
  fake_portfolio$scenario_geography <- "bad"

  expect_error_free(
    sda_portfolio_target(
      fake_market, fake_portfolio,
      sector = "Steel",
      start_year = 2021,
      target_year = 2022,
      geography = "Global",
    )
  )
})

test_that("outputs plan_sec_emissions_factor = NA after target_year (#19)", {
  portfolio <- market <- fake_portfolio(
    year = 2021:2023,
    scen_sec_emissions_factor = 1:3
  )

  out <- sda_portfolio_target(
    market, portfolio,
    sector = "Steel",
    geography = "Global",
    start_year = 2021,
    target_year = 2022
  )

  expect_equal(out$year, c(2021, 2022, 2023))
  expect_equal(out$scen_sec_emissions_factor[[3]], NA_real_)
})
