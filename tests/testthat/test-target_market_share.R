test_that("with bad `data` errors with informative message", {
  expect_error(target_market_share(
    "bad",
    fake_ald(),
    fake_scenario()
  ), "data.frame.*not.*TRUE")
})

test_that("outputs a tibble", {
  out <- target_market_share(
    fake_matched(),
    fake_ald(),
    fake_scenario(),
    region_isos_demo
  )
  expect_is(out, "tbl_df")
})

test_that("outputs is ungrouped", {
  out <- target_market_share(
    fake_matched(),
    fake_ald(),
    fake_scenario(),
    region_isos_demo
  )
  expect_false(dplyr::is_grouped_df(out))
})

test_that("warns when input data is grouped", {
  grouped_data <- group_by(fake_matched(), id_loan)

  expect_warning(
    target_market_share(
      grouped_data,
      fake_ald(),
      fake_scenario(),
      region_isos_demo
    ),
    "Ungrouping"
  )
})

test_that("with fake data outputs known value", {
  out <- target_market_share(
    fake_matched(),
    fake_ald(),
    fake_scenario(),
    region_isos_demo
  )

  expect_known_value(out, "ref-target_market_share", update = FALSE)
})

test_that("with data lacking crucial columns errors with informative message", {
  expect_error_missing_names <- function(name) {
    bad_scenario <- rename(
      fake_scenario(),
      bad = name
    )

    expect_error(
      class = "missing_names",
      target_market_share(
        fake_matched(),
        fake_ald(),
        bad_scenario
      )
    )
  }

  expect_error_missing_names("tmsr")
  expect_error_missing_names("smsp")
})

test_that("with NAs in crucial columns errors with informative message", {
  expect_error_crucial_NAs_portfolio <- function(name) {
    data <- fake_matched(
      id_loan = c("i1", "i2", "i1", "i2"),
      loan_size_outstanding = c(40, 10, 40, 10)
    )

    data[1, name] <- NA
    expect_error(
      class = "some_value_is_missing",
      target_market_share(
        data,
        fake_ald(),
        fake_scenario(),
        region_isos_demo
      )
    )
  }

  expect_error_crucial_NAs_ald <- function(name) {
    data <- fake_ald(
      technology = c("ice", "ice", "ice", "ice"),
      production = c(10, 30, 20, 40)
    )

    data[1, name] <- NA
    expect_error(
      class = "some_value_is_missing",
      target_market_share(
        fake_matched(),
        data,
        fake_scenario(),
        region_isos_demo
      )
    )
  }

  expect_error_crucial_NAs_scenario <- function(name) {
    data <- fake_scenario()

    data[1, name] <- NA

    expect_error(
      class = "some_value_is_missing",
      target_market_share(
        fake_matched(),
        fake_ald(),
        data,
        region_isos_demo
      )
    )
  }

  expect_error_crucial_NAs_portfolio("name_ald")

  expect_error_crucial_NAs_ald("production")
  expect_error_crucial_NAs_ald("sector")
  expect_error_crucial_NAs_ald("year")

  expect_error_crucial_NAs_scenario("scenario")
  expect_error_crucial_NAs_scenario("tmsr")
  expect_error_crucial_NAs_scenario("smsp")
})

test_that("outputs expected names", {
  out <- target_market_share(
    fake_matched(),
    fake_ald(),
    fake_scenario(),
    region_isos_demo
  )

  expect_named(
    out,
    c(
      "sector",
      "technology",
      "year",
      "region",
      "scenario_source",
      "weighted_production_metric",
      "weighted_production_value"
    )
  )
})

test_that("with known input outputs as expected", {
  portfolio <- fake_matched(
    name_ald = "comp1"
  )

  ald <- fake_ald(
    technology = c("electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2021, 2021),
    name_company = paste0("comp", rep(1, 4)),
    production = c(200, 250, 240, 240)
  )

  scenario <- fake_scenario(
    technology = c("electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2021, 2021),
    tmsr = c(1, 1, 1.85, 0.6),
    smsp = c(0, 0, 0.34, -0.2)
  )

  out <- target_market_share(portfolio, ald, scenario, region_isos_demo)
  out_target <- out %>%
    filter(weighted_production_metric == "target_sds") %>%
    arrange(.data$technology, .data$year)

  expect_equal(out_target$weighted_production_value, c(200, 353, 250, 150))
})

test_that("with known input outputs as expected, at company level", {
  portfolio <- fake_matched(
    name_ald = c("comp1", "comp2")
  )

  ald <- fake_ald(
    technology = c("electric", "ice", "electric", "ice", "electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2021, 2021, 2020, 2020, 2021, 2021),
    name_company = paste0("comp", c(rep(1, 4), rep(2, 4))),
    production = c(10, 30, 20, 20, 90, 95, 100, 100)
  )

  scenario <- fake_scenario(
    technology = c("electric", "ice", "electric", "ice", "electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2021, 2021, 2020, 2020, 2021, 2021),
    tmsr = c(1, 1, 1.85, 0.6, 1, 1, 1.85, 0.6),
    smsp = c(0, 0, 0.34, -0.2, 0, 0, 0.34, -0.2)
  )

  out <- target_market_share(
    portfolio,
    ald,
    scenario,
    region_isos_demo,
    by_company = TRUE
  )
  out_target <- out %>%
    filter(weighted_production_metric == "target_sds") %>%
    arrange(.data$technology, .data$year, .data$name_ald)

  expect_equal(
    out_target$weighted_production_value,
    c(20, 180, 47.2, 305.8, 60, 190, 36, 114)
  )
})

test_that("with known input outputs as expected, ald benchmark", {
  portfolio <- fake_matched()

  ald <- fake_ald(
    name_company = c("shaanxi auto", "unmatched company", "shaanxi auto", "unmatched company"),
    technology = c("electric", "electric", "electric", "electric"),
    production = c(10, 20, 20, 70),
    plant_location = c("de", "fr", "de", "fr"),
    year = c(2020, 2020, 2025, 2025)
  )

  scenario <- fake_scenario(
    region = c("global", "global", "europe", "europe"),
    technology = c("electric", "electric", "ice", "ice"),
    year = c(2020, 2025, 2020, 2025)
  )

  out <- target_market_share(
    portfolio,
    ald,
    scenario,
    region_isos_demo,
    by_company = TRUE
  )

  out_benchmark <- out %>%
    filter(weighted_production_metric == "normalized_corporate_economy") %>%
    arrange(.data$technology, .data$year)

  expect_equal(
    out_benchmark$weighted_production_value,
    c(10, 30)
  )
})

test_that("outputs identical values at start year (#47, #87)", {
  matched <- fake_matched(
    sector = c("power", "automotive"),
    sector_ald = c("power", "automotive")
  )

  ald <- fake_ald(
    sector = rep(c("automotive", "power"), times = 2, each = 2),
    technology = rep(c("electric", "ice", "renewablescap", "coalcap"), 2),
    year = 2020,
    plant_location = rep(c("us", "de"), each = 4),
    production = rep(c(200, 250, 100, 150), 2)
  )

  scenario <- fake_scenario(
    sector = rep(c("automotive", "power"), times = 2, each = 2),
    technology = rep(c("electric", "ice", "renewablescap", "coalcap"), 2),
    year = 2020,
    region = rep(c("global", "europe"), each = 4),
    tmsr = 1,
    smsp = 0
  )

  out <- target_market_share(
    matched,
    ald,
    scenario,
    region_isos_demo
  ) %>%
    filter(year == min(year)) %>%
    group_by(sector, technology, region) %>%
    summarize(distinct_intial_values = dplyr::n_distinct(weighted_production_value)) %>%
    mutate(initial_values_are_equal = (.data$distinct_intial_values == 1))

  expect_true(all(out$initial_values_are_equal))
})

test_that("corporate economy benchmark only aggregates ultimate owners (#103)", {
  out <- target_market_share(
    fake_matched(name_ald = c("company a", "company b")),
    fake_ald(
      name = c("company a", "company b", "company a", "company b"),
      is_ultimate_owner = c(T, F, T, F),
      production = c(50, 100, 100, 50),
      year = c(2020, 2020, 2021, 2021)
    ),
    fake_scenario(year = c(2020, 2021)),
    region_isos_demo
  )

  corporate_economy_value <- out %>%
    filter(weighted_production_metric == "normalized_corporate_economy")

  expect_equal(corporate_economy_value$weighted_production_value, c(150, 300))
})
