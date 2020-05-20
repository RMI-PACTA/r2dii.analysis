test_that("with fake data outputs known value", {
  out <- add_sda_target(
    fake_matched(sector = "cement",
                 sector_ald = "cement"),
    ald = fake_ald(sector = "cement",
                   technology = "cement",
                   year = c(2020, 2021,2022),
                   emission_factor = c(0.6, 0.7, 0.8)),
    co2_intensity_scenario = r2dii.analysis::co2_intensity_scenario
  )

  expect_known_value(out, "ref-add_sda_target", update = FALSE)
})
