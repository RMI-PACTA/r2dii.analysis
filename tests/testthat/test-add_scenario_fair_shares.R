test_that("with fake_scenario and start_year = 2020 passes with no error", {
  expect_no_error(add_scenario_fair_shares(scenario, start_year = 2020))
})

test_that("w/ scenario with missing names errors gracefully", {
  skip("Broken test. Skip until good data passes with no error")
  bad <- function(data, x) dplyr::rename(data, bad = x)

  expect_error_missing_names <- function(scenario = fake_scenario()) {
    expect_error(
      class = "missing_names",
      add_scenario_fair_shares(scenario, start_year = 2020)
    )
  }

  expect_error_missing_names(scenario = bad(fake_scenario(), "scenario"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "sector"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "technology"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "region"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "value"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "units"))
})
