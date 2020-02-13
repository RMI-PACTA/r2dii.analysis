test_that("w/ scenario with missing names errors gracefully", {
  bad <- function(data, x) dplyr::rename(data, bad = x)

  expect_error_missing_names <- function(scenario = fake_scenario()) {
    expect_error(
      class = "missing_names",
      add_scenario_fair_shares(scenario, startyear = 2020)
    )
  }

  expect_error_missing_names(scenario = bad(fake_scenario(), "scenario"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "sector"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "technology"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "region"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "value"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "units"))
})
