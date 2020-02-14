get_scenario <- function() r2dii.dataraw::scenario_demo

test_that("w/ fake_scenario and start_year = 2020 passes with no error", {
  expect_error_free(
    add_scenario_fair_shares(get_scenario(), start_year = 2020)
  )
})

test_that("outputs known value (temporary regression test for refactoring)", {
  expect_known_value(
    add_scenario_fair_shares(get_scenario(), start_year = 2020),
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

  expect_error_missing_names(bad(get_scenario(), "scenario"))
  expect_error_missing_names(bad(get_scenario(), "sector"))
  expect_error_missing_names(bad(get_scenario(), "technology"))
  expect_error_missing_names(bad(get_scenario(), "region"))
  expect_error_missing_names(bad(get_scenario(), "value"))
  expect_error_missing_names(bad(get_scenario(), "units"))
})

test_that("w/ scenario with inconsistent units errors gracefully", {
  bad_scenario <- fake_scenario(scenario = "sds",
                                sector = "automotive",
                                region = "global",
                                technology = "ice",
                                units = c("aa", "bb"))
  expect_error(
    class = "inconsistent units",
    add_scenario_fair_shares(bad_scenario)
  )

})
