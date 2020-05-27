library(r2dii.data)
library(r2dii.match)
library(dplyr)
library(glue)

test_that("with fake data outputs known value", {
  out <- add_sda_target(
    fake_matched(
      sector_ald = "cement"
    ),
    ald = fake_ald(
      sector = "cement",
      technology = "cement",
      year = c(2020, 2021, 2022),
      emission_factor = c(1, 2, 3)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050),
      emission_factor = c(0.6, 0.2)
    )
  )

  expect_known_value(out, "ref-add_sda_target", update = FALSE)
  expect_known_output(
    out, "ref-add_sda_target-output", print = TRUE, update = FALSE
  )
})

test_that("with bad `data` errors with informative message", {
  expect_error(
    add_sda_target("bad", fake_ald(), fake_co2_scenario()),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    add_sda_target(fake_matched(), "bad", fake_co2_scenario()),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    add_sda_target(fake_matched(), fake_ald(), "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    add_sda_target(
      fake_matched(),
      ald = fake_ald(),
      co2_intensity_scenario = fake_co2_scenario(),
      use_credit_limit = "bad"
    ),
    "logical.*not.*TRUE",
  )
})

test_that("w/ missing crucial names errors gracefully", {
  bad <- function(data, x) rename(data, bad = one_of(x))

  expect_error_missing_names <- function(match_result = fake_matched(),
                                         ald = fake_ald(),
                                         scenario = fake_co2_scenario()) {
    expect_error(
      class = "missing_names",
      add_sda_target(match_result, ald, scenario)
    )
  }

  mch <- fake_matched()
  expect_error_missing_names(match_result = bad(mch, "loan_size_outstanding"))
  expect_error_missing_names(match_result = bad(mch, "loan_size_credit_limit"))
  expect_error_missing_names(match_result = bad(mch, "name_ald"))
  expect_error_missing_names(match_result = bad(mch, "sector_ald"))

  expect_error_missing_names(ald = bad(fake_ald(), "name_company"))
  expect_error_missing_names(ald = bad(fake_ald(), "sector"))
  expect_error_missing_names(ald = bad(fake_ald(), "year"))
  expect_error_missing_names(ald = bad(fake_ald(), "emission_factor"))
  expect_error_missing_names(ald = bad(fake_ald(), "production"))

  scen <- fake_co2_scenario()
  expect_error_missing_names(scenario = bad(scen, "sector"))
  expect_error_missing_names(scenario = bad(scen, "year"))
  expect_error_missing_names(scenario = bad(scen, "emission_factor"))
})

test_that("with known input outputs as expected", {
  sectors <- c("cement", "steel")

  valid_matches <- fake_matched(
    sector_ald = sectors,
    name_ald = glue("{sectors}_company")
  )

  ald <- fake_ald(
    name_company = glue("{sectors}_company"),
    sector = sectors,
    year = 2020,
    emission_factor = c(0.6, 1.6)
  )

  co2_intensity_scenario <- fake_co2_scenario(
    sector = rep(sectors, each = 3),
    year = rep(c(2020, 2030, 2050), 2),
    emission_factor = c(0.53835, 0.43039, 0.16897, 1.43731, 0.87454, 0.26055),
  )

  out <- add_sda_target(valid_matches, ald, co2_intensity_scenario) %>%
    filter(
      emission_factor_name == "portfolio_target_emission_factor",
      year == 2030
    )

  out_cement <- filter(out, sector == "cement")
  expect_equal(round(out_cement$emission_factor_value, 3), 0.460)

  out_steel <- filter(out, sector == "steel")
  expect_equal(round(out_steel$emission_factor_value, 3), 0.944)
})

test_that("without `sector` throws no error", {
  # 2DegreesInvesting/r2dii.analysis/pull/62#issuecomment-634651157
  without_sector <- dplyr::select(fake_matched(), -sector)
  expect_error_free(
    add_sda_target(
      without_sector,
      ald = fake_ald(),
      co2_intensity_scenario = fake_co2_scenario()
    )
  )
})

test_that("properly weights emissions factors", {
  companies <- c("a","b")

  out <- add_sda_target(
    fake_matched(
      id_loan = c(1,2),
      name_ald = companies,
      sector_ald = "cement"
    ),
    ald = fake_ald(
      name_company = companies,
      sector = "cement",
      technology = "cement",
      year = 2020,
      emission_factor = c(1, 2)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050),
      emission_factor = c(0.6, 0.2)
    )
  )

  initial_data <- out %>%
    dplyr::filter(
      year == 2020,
      emission_factor_name == "portfolio_weighted_emission_factor"
    )

  expect_equal(initial_data$emission_factor_value, 1.5)
})

test_that("outputs 3 metrics of CO2 emissions", {
  out <- add_sda_target(
    fake_matched(sector_ald = "cement"),
    ald = fake_ald(
      sector = "cement",
      technology = "cement",
      year = c(2020, 2021, 2022),
      emission_factor = c(1, 2, 3)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050), emission_factor = c(0.6, 0.2)
    )
  )

  expect_length(unique(out$emission_factor_name), 3L)
})

test_that("outputs expected names", {
  out <- add_sda_target(
    fake_matched(sector_ald = "cement"),
    ald = fake_ald(
      sector = "cement",
      technology = "cement",
      year = c(2020, 2021, 2022),
      emission_factor = c(1, 2, 3)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050), emission_factor = c(0.6, 0.2)
    )
  )

  exp <- c("sector", "year", "emission_factor_name", "emission_factor_value")
  expect_named(out, exp)
})

