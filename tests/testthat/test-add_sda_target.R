library(r2dii.data)
library(r2dii.match)

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
      emission_factor = c(0.6, 0.7, 0.8)
    ),
    co2_intensity_scenario = r2dii.analysis::co2_intensity_scenario
  )

  expect_known_value(out, "ref-add_sda_target", update = FALSE)
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
      emission_factor = c(0.6, 0.7, 0.8)
    ),
    co2_intensity_scenario = r2dii.analysis::co2_intensity_scenario
  )

  expect_known_value(out, "ref-add_sda_target", update = FALSE)
})


test_that("with known input outputs as expected", {
  valid_matches <- fake_matched(id_loan = c(1,2),
                                id_2dii = c(1,2),
                                sector = c("cement", "steel"),
                                sector_ald = c("cement", "steel"),
                                name_ald = c("cement_company", "steel_company"))
  ald <- fake_ald(name_company = c("cement_company", "steel_company"),
                  sector = c("cement", "steel"),
                  technology = NA,
                  year = 2020,
                  emission_factor = c(0.6, 1.6))

  co2_intensity_scenario = fake_scenario(scenario = "demo_2020",
                                         sector = c("cement",
                                                    "cement",
                                                    "cement",
                                                    "steel",
                                                    "steel",
                                                    "steel"),
                                         region = "global",
                                         technology = NA,
                                         tmsr = NA,
                                         smsp = NA,
                                         year = c(2020,
                                                  2030,
                                                  2050,
                                                  2020,
                                                  2030,
                                                  2050),
                                         emission_factor = c(0.53835,
                                                             0.43039,
                                                             0.16897,
                                                             1.43731,
                                                             0.87454,
                                                             0.26055),
                                         emission_factor_unit = NA)

out <- valid_matches %>%
  add_sda_target(ald,
                 co2_intensity_scenario) %>%
  dplyr::filter(emission_factor_name == "portfolio_target_emission_factor",
                year == 2030)

out_cement <- out %>%
  dplyr::filter(sector == "cement")

out_steel <- out %>%
  dplyr::filter(sector == "steel")

expect_equal(round(out_cement$emission_factor_value,3),
             0.460)


expect_equal(round(out_steel$emission_factor_value,3),
             0.944)

})
