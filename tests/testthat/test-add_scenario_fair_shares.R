test_that("with fake_scenario and start_year = 2020 passes with no error", {
  expect_error_free(
    add_scenario_fair_shares(r2dii.dataraw::scenario_demo, start_year = 2020)
  )
})

test_that("outputs known value (temporary regression test for refactoring)", {
  expect_known_value(
    add_scenario_fair_shares(r2dii.dataraw::scenario_demo, start_year = 2020),
    "ref-add_scenario_fair_shares",
    update = FALSE
  )
})

test_that("w/ scenario with missing names errors gracefully", {
  bad <- function(data, x) dplyr::rename(data, bad = x)

  expect_error_missing_names <- function(scenario) {
    expect_error(
      class = "missing_names",
      add_scenario_fair_shares(scenario, start_year = 2020)
    )
  }

  expect_error_missing_names(bad(r2dii.dataraw::scenario_demo, "scenario"))
  expect_error_missing_names(bad(r2dii.dataraw::scenario_demo, "sector"))
  expect_error_missing_names(bad(r2dii.dataraw::scenario_demo, "technology"))
  expect_error_missing_names(bad(r2dii.dataraw::scenario_demo, "region"))
  expect_error_missing_names(bad(r2dii.dataraw::scenario_demo, "value"))
  expect_error_missing_names(bad(r2dii.dataraw::scenario_demo, "units"))
})
