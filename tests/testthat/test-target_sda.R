library(r2dii.data)
library(r2dii.match)
library(dplyr)

test_that("with fake data outputs known value", {
  out <- target_sda(
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

  expect_known_value(out, "ref-target_sda", update = FALSE)
})

test_that("outputs is ungrouped", {
  out <- target_sda(
    fake_matched(
      sector_ald = "cement"
    ),
    fake_ald(
      sector = "cement",
      year = c(2020, 2050)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      emission_factor = c(1, 2),
      year = c(2020, 2050)
    )
  )
  expect_false(dplyr::is_grouped_df(out))
})

test_that("warns when input data is grouped", {
  grouped_data <- group_by(fake_matched(sector_ald = "cement"), id_loan)

  out <- function() {
    target_sda(
      grouped_data,
      ald = fake_ald(
        sector = "cement",
        year = c(2020, 2050)
      ),
      co2_intensity_scenario = fake_co2_scenario(
        emission_factor = c(1, 2),
        year = c(2020, 2050)
      )
    )
  }

  expect_warning(out(), "Ungrouping")
})

test_that("with bad `data` errors with informative message", {
  expect_error(
    target_sda("bad", fake_ald(), fake_co2_scenario()),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    target_sda(fake_matched(), "bad", fake_co2_scenario()),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    target_sda(fake_matched(), fake_ald(), "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    target_sda(
      fake_matched(),
      ald = fake_ald(),
      co2_intensity_scenario = fake_co2_scenario(),
      use_credit_limit = "bad"
    ),
    "logical.*not.*TRUE",
  )
})

test_that("w/ missing crucial names errors gracefully", {
  bad <- function(data, x) rename(data, bad = dplyr::one_of(x))

  expect_error_missing_names <- function(match_result = fake_matched(),
                                         ald = fake_ald(),
                                         scenario = fake_co2_scenario()) {
    expect_error(
      class = "missing_names",
      target_sda(match_result, ald, scenario)
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

test_that("without `sector` throws no error", {
  # 2DegreesInvesting/r2dii.analysis/pull/62#issuecomment-634651157
  without_sector <- select(fake_matched(sector_ald = "cement"), -sector)
  expect_error_free(
    target_sda(
      without_sector,
      ald = fake_ald(sector = "cement"),
      co2_intensity_scenario = fake_co2_scenario()
    )
  )
})

test_that("properly weights emissions factors", {
  companies <- c("a", "b")

  out <- target_sda(
    fake_matched(
      id_loan = c(1, 2),
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
    filter(
      year == 2020,
      emission_factor_metric == "projected"
    )

  expect_equal(initial_data$emission_factor_value, 1.5)
})

test_that("outputs expected names", {
  out <- target_sda(
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

  exp <- c("sector", "year", "emission_factor_metric", "emission_factor_value")
  expect_named(out, exp)
})

test_that("with known input outputs as expected", {
  # TODO: Re-factor this test into smaller isolated expected output tests
  matched <- fake_matched(sector_ald = "cement")

  ald <- fake_ald(
    sector = "cement",
    technology = "cement",
    name_company = c(rep("shaanxi auto", 4), "company 2"),
    year = c(2020, 2021, 2022, 2025, 2020),
    emission_factor = c(0.9, 0.9, 0.8, 0.5, 12)
  )

  co2_intensity_scenario <- fake_co2_scenario(
    scenario = c(rep("b2ds", 2), rep("sds", 2)),
    year = rep(c(2020, 2025), 2),
    emission_factor = c(0.5, 0.1, 0.5, 0.4)
  )

  out <- target_sda(matched, ald, co2_intensity_scenario) %>%
    arrange(.data$year) %>%
    split(.$emission_factor_metric)

  expect_equal(out$projected$emission_factor_value, c(0.9, 0.9, 0.8, 0.5))
  expect_equal(
    out$corporate_economy$emission_factor_value, c(06.45, 0.9, 0.8, 0.5)
  )
  expect_equal(out$adjusted_scenario_b2ds$emission_factor_value, c(6.45, 1.29))
  expect_equal(out$adjusted_scenario_sds$emission_factor_value, c(6.45, 5.16))
  expect_equal(out$target_b2ds$emission_factor_value, c(0.9, 1.29))
  expect_equal(out$target_sds$emission_factor_value, c(0.9, 5.16))
})

test_that("with known input outputs as expected, at company level (#155)", {
  matched <- fake_matched(
    name_ald = c("shaanxi auto", "company 2"),
    sector_ald = "cement"
  )

  ald <- fake_ald(
    sector = "cement",
    technology = "cement",
    name_company = c(rep("shaanxi auto", 4), "company 2"),
    year = c(2020, 2021, 2022, 2025, 2020),
    emission_factor = c(0.9, 0.9, 0.8, 0.5, 12)
  )

  co2_intensity_scenario <- fake_co2_scenario(
    scenario = c(rep("b2ds", 2), rep("sds", 2)),
    year = rep(c(2020, 2025), 2),
    emission_factor = c(0.5, 0.1, 0.5, 0.4)
  )

  out <- target_sda(
    matched,
    ald,
    co2_intensity_scenario,
    by_company = TRUE
  ) %>%
    arrange(.data$year)

  out_shaanxi <- filter(out, name_ald == "shaanxi auto") %>%
    split(.$emission_factor_metric)

  expect_equal(out_shaanxi$projected$emission_factor_value, c(0.9, 0.9, 0.8, 0.5))
  expect_equal(out_shaanxi$target_b2ds$emission_factor_value, c(0.9, 1.29))
  expect_equal(out_shaanxi$target_sds$emission_factor_value, c(0.9, 5.16))

  out_company_2 <- filter(out, name_ald == "company 2") %>%
    split(.$emission_factor_metric)

  expect_equal(out_company_2$projected$emission_factor_value, 12)
  expect_equal(out_company_2$target_b2ds$emission_factor_value, 12)
  expect_equal(out_company_2$target_sds$emission_factor_value, 12)
})

test_that("with no matching data warns", {
  no_matches <- fake_matched(sector_ald = "bad")
  expect_warning(
    target_sda(no_matches, fake_ald(), fake_co2_scenario()), "no match"
  )

  bad_scenario <- fake_co2_scenario(sector = "bad")
  expect_warning(
    target_sda(fake_matched(), fake_ald(), bad_scenario), "no scenario"
  )
})
