library(r2dii.data)
library(r2dii.match)

test_that("with bad `data` errors with informative message", {
  expect_error(add_sda_target("bad", fake_ald(), fake_co2_scenario()), "data.frame.*not.*TRUE")
  expect_error(add_sda_target(fake_matched(), "bad", fake_co2_scenario()), "data.frame.*not.*TRUE")
  expect_error(add_sda_target(fake_matched(), fake_ald(), "bad"), "data.frame.*not.*TRUE")
  expect_error(add_sda_target(fake_matched(),
                              ald = fake_ald(),
                              co2_intensity_scenario = fake_co2_scenario(),
                              use_credit_limit = "bad"), "logical.*not.*TRUE")
  })

test_that("w/ match_result, ald or scenario with missing names errors gracefully", {
  bad <- function(data, x) dplyr::rename(data, bad = x)

  expect_error_missing_names <- function(match_result = fake_matched(),
                                         ald = fake_ald(),
                                         scenario = fake_co2_scenario()) {
    expect_error(
      class = "missing_names",
      add_sda_target(match_result, ald, scenario)
    )
  }

  expect_error_missing_names(match_result = bad(fake_matched(), "loan_size_outstanding"))
  expect_error_missing_names(match_result = bad(fake_matched(), "loan_size_credit_limit"))
  expect_error_missing_names(match_result = bad(fake_matched(), "name_ald"))
  expect_error_missing_names(match_result = bad(fake_matched(), "sector_ald"))

  expect_error_missing_names(ald = bad(fake_ald(), "name_company"))
  expect_error_missing_names(ald = bad(fake_ald(), "sector"))
  expect_error_missing_names(ald = bad(fake_ald(), "year"))
  expect_error_missing_names(ald = bad(fake_ald(), "emission_factor"))
  expect_error_missing_names(ald = bad(fake_ald(), "production"))

  expect_error_missing_names(scenario = bad(fake_co2_scenario(), "sector"))
  expect_error_missing_names(scenario = bad(fake_co2_scenario(), "year"))
  expect_error_missing_names(scenario = bad(fake_co2_scenario(), "emission_factor"))
})

test_that("with fake data outputs known value", {
  out <- add_sda_target(
    fake_matched(
      sector = "cement",
      sector_ald = "cement"
    ),
    ald = fake_ald(
      sector = "cement",
      technology = "cement",
      year = c(2020, 2021, 2022),
      emission_factor = c(1, 2, 3)
    ),
    co2_intensity_scenario = fake_co2_scenario(year = c(2020,2050),
                                               emission_factor = c(0.6, 0.2))
  )

  expect_known_value(out, "ref-add_sda_target", update = FALSE)
})

test_that("with known input outputs as expected", {
  valid_matches <- fake_matched(
    id_loan = c(1, 2),
    id_2dii = c(1, 2),
    sector = c("cement", "steel"),
    sector_ald = c("cement", "steel"),
    name_ald = c("cement_company", "steel_company")
  )
  ald <- fake_ald(
    name_company = c("cement_company", "steel_company"),
    sector = c("cement", "steel"),
    technology = NA,
    year = 2020,
    emission_factor = c(0.6, 1.6)
  )

  co2_intensity_scenario <- fake_co2_scenario(
    scenario = "demo_2020",
    sector = c(
      "cement",
      "cement",
      "cement",
      "steel",
      "steel",
      "steel"
    ),
    region = "global",
    technology = NA,
    tmsr = NA,
    smsp = NA,
    year = c(
      2020,
      2030,
      2050,
      2020,
      2030,
      2050
    ),
    emission_factor = c(
      0.53835,
      0.43039,
      0.16897,
      1.43731,
      0.87454,
      0.26055
    ),
    emission_factor_unit = NA
  )

  out <- valid_matches %>%
    add_sda_target(
      ald,
      co2_intensity_scenario
    ) %>%
    dplyr::filter(
      emission_factor_name == "portfolio_target_emission_factor",
      year == 2030
    )

  out_cement <- out %>%
    dplyr::filter(sector == "cement")

  out_steel <- out %>%
    dplyr::filter(sector == "steel")

  expect_equal(
    round(out_cement$emission_factor_value, 3),
    0.460
  )


  expect_equal(
    round(out_steel$emission_factor_value, 3),
    0.944
  )
})
