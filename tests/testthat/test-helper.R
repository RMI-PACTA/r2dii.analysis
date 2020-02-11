test_that("fake_portfolio adds a row", {
  expected <- tibble::tibble(
    scenario_geography = "Global",
    scenario = "B2DS",
    sector = "Steel",
    year = 1:3L,
    investor_name = "Market",
    portfolio_name = "GlobalMarket",
    allocation = "PortfolioWeight",
    plan_sec_emissions_factor = 1,
    scen_sec_emissions_factor = 1:3,
  )
  expect_equal(
    fake_portfolio(year = 1L:3L, scen_sec_emissions_factor = 1:3),
    expected
  )
})

test_that("fake_portfolio adds a new column", {
  expect_true(rlang::has_name(fake_portfolio(new = "any"), "new"))
})
