test_that("w/ fake_scenario and start_year = 2020 passes with no error", {
  expect_error_free(
    add_scenario_fair_shares(fake_scenario(), start_year = 2020)
  )
})

test_that("outputs known value", {
  expect_known_value(
    add_scenario_fair_shares(fake_scenario(), start_year = 2020),
    "ref-add_scenario_fair_shares",
    update = FALSE
  )
})

test_that("w/ scenario with missing names errors gracefully", {
  expect_error_missing_names <- function(scenario) {
    expect_error(
      class = "missing_names",
      add_scenario_fair_shares(scenario, start_year = 2020)
    )
  }

  bad <- function(data, x) dplyr::rename(data, bad = x)

  expect_error_missing_names(bad(fake_scenario(), "scenario"))
  expect_error_missing_names(bad(fake_scenario(), "sector"))
  expect_error_missing_names(bad(fake_scenario(), "technology"))
  expect_error_missing_names(bad(fake_scenario(), "region"))
  expect_error_missing_names(bad(fake_scenario(), "value"))
  expect_error_missing_names(bad(fake_scenario(), "units"))
})

test_that("w/ scenario with inconsistent units errors gracefully", {
  bad_scenario <- fake_scenario(
    scenario = "sds",
    sector = "automotive",
    region = "global",
    technology = "ice",
    units = c("aa", "bb")
  )

  expect_error(
    class = "inconsistent_units",
    add_scenario_fair_shares(bad_scenario)
  )

  verify_output(
    test_path("output", "inconsistent_units.txt"),
    add_scenario_fair_shares(bad_scenario)
  )
})

test_that("tfsr is calculated as expected", {
  scenario <- fake_scenario(
    year = c(2020, 2021, 2022),
    value = c(1500, 300, 12)
  )

  expected_tfsr <- c(1, 0.2, 0.008)

  output <- add_scenario_fair_shares(scenario, 2020)

  expect_equal(output$tfsr, expected_tfsr)
})

test_that("mfsp is calculated as expected", {
  scenario <- fake_scenario(
    year = c(2020, 2021, 2022, 2020, 2021, 2022),
    technology = c("ice", "ice", "ice", "electric", "electric", "electric"),
    value = c(8000, 3000, 120, 2000, 3000, 6000)
  )

  expected_mfsp_ice <- c(0, -0.5, -0.788)

  expected_mfsp_electric <- c(0, 0.1, 0.4)

  output <- add_scenario_fair_shares(scenario, 2020)

  output_ice <- dplyr::filter(output, technology == "ice") %>%
    dplyr::arrange(year)

  output_electric <- dplyr::filter(output, technology == "electric") %>%
    dplyr::arrange(year)

  expect_equal(output_ice$mfsp, expected_mfsp_ice)

  expect_equal(output_electric$mfsp, expected_mfsp_electric)
})
