library(dplyr)
library(r2dii.data)

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

  out_company <- target_sda(
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
    ),
    by_company = TRUE
  )

  expect_known_value(out_company, "ref-target_sda_company", update = TRUE)
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
    out$corporate_economy$emission_factor_value, c(6.45, 0.9, 0.8, 0.5)
  )
  expect_equal(
    round(out$adjusted_scenario_b2ds$emission_factor_value, 2),
    c(6.45, 5.42, 4.39, 3.35, 2.32, 1.29)
  )
  expect_equal(
    round(out$adjusted_scenario_sds$emission_factor_value, 2),
    c(6.45, 6.19, 5.93, 5.68, 5.42, 5.16)
  )
  expect_equal(
    round(out$target_b2ds$emission_factor_value, 2),
    c(0.9, 0.98, 1.06, 1.13, 1.21, 1.29)
  )
  expect_equal(
    round(out$target_sds$emission_factor_value, 2),
    c(0.9, 1.75, 2.60, 3.46, 4.31, 5.16)
  )
})

test_that("with no matching data warns", {
  no_matches <- fake_matched(sector_ald = "bad")

  if (packageVersion("testthat") >= "2.99.0.9000") {
    expect_warning(
      class = "no_match",
      target_sda(no_matches, fake_ald(), fake_co2_scenario())
    )
  } else {
    expect_warning(
      target_sda(no_matches, fake_ald(), fake_co2_scenario()), "no match"
    )
  }

  bad_scenario <- fake_co2_scenario(sector = "bad")
  expect_warning(
    target_sda(fake_matched(), fake_ald(), bad_scenario), "no scenario"
  )
})

test_that("with duplicated id_loan weights emission_factor as expected (#160)", {
  match_result <- fake_matched(
    id_loan = c(1, 1),
    name_ald = rep("large company", 2),
    sector_ald = "cement"
  )

  ald <- fake_ald(
    sector = "cement",
    name_company = "large company",
    emission_factor = 2,
    year = c(2020, 2025)
  )

  scen <- fake_co2_scenario(
    year = c(2020, 2025),
    emission_factor = c(1, 0.5)
  )

  expect_error(
    target_sda(
      match_result,
      ald,
      scen
    ) %>%
      filter(year == min(year)),
    class = "unique_ids"
  )
})

test_that("with NAs in crucial columns errors with informative message (#146)", {
  expect_error_crucial_NAs_portfolio <- function(name) {
    data <- fake_matched(sector_ald = "cement")

    ald <- fake_ald(
      sector = "cement",
      year = c(2020, 2050)
    )

    scen <- fake_co2_scenario(
      emission_factor = c(1, 2),
      year = c(2020, 2050)
    )

    data[1, name] <- NA
    expect_error(
      class = "some_value_is_missing",
      target_sda(
        data,
        ald,
        scen
      )
    )
  }

  expect_error_crucial_NAs_ald <- function(name) {
    match_result <- fake_matched(sector_ald = "cement")

    data <- fake_ald(
      sector = "cement",
      year = c(2020, 2050)
    )

    scen <- fake_co2_scenario(
      emission_factor = c(1, 2),
      year = c(2020, 2050)
    )


    data[1, name] <- NA
    expect_error(
      class = "some_value_is_missing",
      target_sda(
        match_result,
        data,
        scen
      )
    )
  }

  expect_error_crucial_NAs_scenario <- function(name) {
    match_result <- fake_matched(sector_ald = "cement")

    ald <- fake_ald(
      sector = "cement",
      year = c(2020, 2050)
    )

    data <- fake_co2_scenario(
      emission_factor = c(1, 2),
      year = c(2020, 2050)
    )

    data[1, name] <- NA

    expect_error(
      class = "some_value_is_missing",
      target_sda(
        match_result,
        ald,
        data
      )
    )
  }

  expect_error_crucial_NAs_portfolio("name_ald")
  expect_error_crucial_NAs_portfolio("sector_ald")

  expect_error_crucial_NAs_ald("name_company")
  expect_error_crucial_NAs_ald("production")
  expect_error_crucial_NAs_ald("sector")
  expect_error_crucial_NAs_ald("year")

  expect_error_crucial_NAs_scenario("sector")
  expect_error_crucial_NAs_scenario("year")
  expect_error_crucial_NAs_scenario("emission_factor")
})

test_that("with multiple technologies weights emission_factor as expected (#160)", {
  match_result <- fake_matched(
    id_loan = c(1, 2),
    name_ald = rep("large company", 2),
    sector_ald = "cement"
  )

  ald <- fake_ald(
    sector = "cement",
    name_company = "large company",
    technology = rep(c("a", "b"), 2),
    emission_factor = 2,
    year = c(rep(2020, 2), rep(2025, 2))
  )

  scen <- fake_co2_scenario(
    year = c(2020, 2025),
    emission_factor = c(1, 0.5)
  )

  out <- target_sda(
    match_result,
    ald,
    scen
  ) %>%
    filter(year == min(year)) %>%
    split(.$emission_factor_metric)

  expect_equal(out$projected$emission_factor_value, 2)
  expect_equal(out$target_b2ds$emission_factor_value, 2)
})

test_that("with multiple technologies, aggregates production-weighted emission_factor (#160)", {
  out <- target_sda(
    fake_matched(sector_ald = "cement"),
    ald = fake_ald(
      sector = "cement",
      technology = c("cement 1", "cement 2"),
      year = 2020,
      production = c(1, 3),
      emission_factor = c(4, 132)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050), emission_factor = c(0.6, 0.2)
    )
  ) %>%
    split(.$emission_factor_metric)

  expect_equal(out$corporate_economy$emission_factor_value, 100)
})

test_that("with multiple plant_location, aggregates production-weighted emission_factor (#160)", {
  out <- target_sda(
    fake_matched(sector_ald = "cement"),
    ald = fake_ald(
      sector = "cement",
      plant_location = c("de", "fr"),
      year = 2020,
      production = c(1, 3),
      emission_factor = c(4, 132)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050), emission_factor = c(0.6, 0.2)
    )
  ) %>%
    split(.$emission_factor_metric)

  expect_equal(out$corporate_economy$emission_factor_value, 100)
})

test_that("filters and warns when input-data has NAs", {
  # Work around: in testthat v2, the `class` argument seems to now work
  # https://gist.github.com/maurolepore/c04388c6d4795561fb168172e75154c0
  .object <- rlang::expr(
    out <- target_sda(
      fake_matched(sector_ald = "cement"),
      ald = fake_ald(
        sector = "cement",
        technology = rep(c("cement", "bad"), 2),
        year = rep(c(2020, 2050), 2),
        emission_factor = c(1, 2, rep(NA, 2))
      ),
      co2_intensity_scenario = fake_co2_scenario(
        year = c(2020, 2050), emission_factor = c(0.6, 0.2)
      )
    )
  )
  if (packageVersion("testthat") >= "2.99.0.9000") {
    args <- list(object = .object, class = "na_emission_factor")
  } else {
    args <- list(object = .object, regexp = "emission_factor.*NA")
  }
  do.call(expect_warning, args)

  out <- split(out, out$emission_factor_metric)
  expect_equal(out$corporate_economy$emission_factor_value, c(1, 2))
})

test_that(
  "`sector` column is not used from data (should only use `sector_ald`) (#178)",
  {
    expect_error_free(
      target_sda(
        fake_matched(
          sector_ald = "cement"
        ) %>% select(-sector),
        fake_ald(
          sector = "cement",
          year = c(2020, 2050)
        ),
        co2_intensity_scenario = fake_co2_scenario(
          emission_factor = c(1, 2),
          year = c(2020, 2050)
        )
      )
    )
  }
)

test_that("with multiple values of `country_of_domicile` outputs the expected
          `emission_factor_value` (#171)", {
  this_company <- "company"
  this_sector <- "steel"
  ald <- fake_ald(
    country_of_domicile = c("a", "b"),
    emission_factor = 0.5,
    year = 2020,
    name_company = this_company,
    sector = this_sector
  )

  matched <- fake_matched(
    name_ald = this_company,
    sector = this_sector,
    sector_ald = this_sector
  )

  out <- matched %>%
    target_sda(ald, co2_intensity_scenario_demo) %>%
    split(.$emission_factor_metric)

  expect_equal(out$projected$emission_factor_value, 0.5)
})

test_that("outputs same target regardless of years present in ald", {
  matched <- fake_matched(
    name_ald = "company a",
    sector_ald = "steel"
  )


  ald_ten_year <- fake_ald(
    sector = "steel",
    technology = "steel",
    name_company = c(rep("company a", 3), rep("company b", 3)),
    emission_factor = c(rep(1.5, 3), rep(2.5, 3)),
    year = rep(c(2020, 2025, 2030), 2),
    plant_location = "DE"
  )

  ald_thirty_year <- fake_ald(
    sector = "steel",
    technology = "steel",
    name_company = c(rep("company a", 4), rep("company b", 4)),
    emission_factor = c(rep(1.5, 4), rep(2.5, 4)),
    year = rep(c(2020, 2025, 2030, 2050), 2),
    plant_location = "DE"
  )

  co2_scenario <- fake_co2_scenario(
    sector = "steel",
    year = c(2020, 2025, 2030, 2050),
    emission_factor = c(2, 1.9, 1.8, 0.25)
  )

  out_ten_year <- target_sda(matched, ald_ten_year, co2_scenario) %>%
    filter(
      year == 2030,
      emission_factor_metric == "target_b2ds"
    )

  out_thirty_year <- target_sda(matched, ald_thirty_year, co2_scenario) %>%
    filter(
      year == 2030,
      emission_factor_metric == "target_b2ds"
    )

  expect_equal(
    out_ten_year$emission_factor_value,
    out_thirty_year$emission_factor_value
  )
})

test_that("outputs only sectors present in `co2_intensity_scenario` (#308)", {
  matched <- fake_matched(
    id_loan = c("L1", "L2"),
    sector_ald = c("cement", "power")
  )

  ald <- fake_ald(
    sector = c("cement", "power"),
  )

  co2_scenario <- fake_co2_scenario(
    sector = "cement",
    emission_factor = c(1, 0.6),
    year = c(2025, 2026)
  )

  out <- target_sda(matched, ald, co2_scenario)

  out_sectors <- unique(out$sector)
  scenario_sectors <- unique(co2_scenario$sector)

  expect_equal(
    setdiff(out_sectors, scenario_sectors),
    character(0)
  )
})

test_that("doesn't output NAs if ald and scenario years are misaligned (#307,
          #346)", {
  matched <- fake_matched(
    sector_ald = "cement"
  )

  ald <- fake_ald(
    sector = "cement",
    year = c(2024, 2025)
  )

  co2_scenario <- fake_co2_scenario(
    emission_factor = c(1, 0.6, 0.4),
    year = c(2023, 2025, 2026)
  )

  out <- target_sda(matched, ald, co2_scenario)

  expect_false(
    any(is.na(out$emission_factor_value))
  )
})
