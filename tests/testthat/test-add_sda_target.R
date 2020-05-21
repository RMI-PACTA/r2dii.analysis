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

test_that("with example throws no error", {
  valid_matches <- r2dii.match::match_name(
    r2dii.data::loanbook_demo, r2dii.data::ald_demo
  ) %>%
    r2dii.match::prioritize()

  expect_error_free(
    add_sda_target(
      valid_matches,
      ald = ald_demo,
      co2_intensity_scenario = r2dii.analysis::co2_intensity_scenario
    )
  )
})
