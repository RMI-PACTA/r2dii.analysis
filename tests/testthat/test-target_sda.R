library(dplyr)
library(r2dii.data)

test_that("with fake data outputs known value", {
  out <- target_sda(
    fake_matched(
      sector_abcd = "cement"
    ),
    abcd = fake_abcd(
      sector = "cement",
      technology = "cement",
      year = c(2020, 2021, 2022),
      emission_factor = c(1, 2, 3)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050),
      emission_factor = c(0.6, 0.2)
    ),
    region_isos = region_isos_stable
  )

  expect_snapshot(out)

  out_company <- target_sda(
    fake_matched(
      sector_abcd = "cement"
    ),
    abcd = fake_abcd(
      sector = "cement",
      technology = "cement",
      year = c(2020, 2021, 2022),
      emission_factor = c(1, 2, 3)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050),
      emission_factor = c(0.6, 0.2)
    ),
    by_company = TRUE,
    region_isos = region_isos_stable
  )

  expect_snapshot(out_company)
})

test_that("outputs is ungrouped", {
  out <- target_sda(
    fake_matched(
      sector_abcd = "cement"
    ),
    fake_abcd(
      sector = "cement",
      year = c(2020, 2050)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      emission_factor = c(1, 2),
      year = c(2020, 2050)
    ),
    region_isos = region_isos_stable
  )
  expect_false(dplyr::is_grouped_df(out))
})

test_that("warns when input data is grouped", {
  grouped_data <- group_by(fake_matched(sector_abcd = "cement"), id_loan)

  out <- function() {
    target_sda(
      grouped_data,
      abcd = fake_abcd(
        sector = "cement",
        year = c(2020, 2050)
      ),
      co2_intensity_scenario = fake_co2_scenario(
        emission_factor = c(1, 2),
        year = c(2020, 2050)
      ),
      region_isos = region_isos_stable
    )
  }

  expect_warning(out(), "Ungrouping")
})

test_that("with bad `data` errors with informative message", {
  expect_error(
    target_sda("bad", fake_abcd(), fake_co2_scenario()),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    target_sda(fake_matched(), "bad", fake_co2_scenario()),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    target_sda(fake_matched(), fake_abcd(), "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    target_sda(
      fake_matched(),
      abcd = fake_abcd(),
      co2_intensity_scenario = fake_co2_scenario(),
      use_credit_limit = "bad"
    ),
    "logical.*not.*TRUE",
  )
})

test_that("w/ missing crucial names errors gracefully", {
  bad <- function(data, x) rename(data, bad = dplyr::one_of(x))

  expect_error_missing_names <- function(match_result = fake_matched(),
                                         abcd = fake_abcd(),
                                         scenario = fake_co2_scenario()) {
    expect_error(
      class = "missing_names",
      target_sda(match_result, abcd, scenario)
    )
  }

  mch <- fake_matched()
  expect_error_missing_names(match_result = bad(mch, "loan_size_outstanding"))
  expect_error_missing_names(match_result = bad(mch, "loan_size_credit_limit"))
  expect_error_missing_names(match_result = bad(mch, "name_abcd"))
  expect_error_missing_names(match_result = bad(mch, "sector_abcd"))

  expect_error_missing_names(abcd = bad(fake_abcd(), "name_company"))
  expect_error_missing_names(abcd = bad(fake_abcd(), "sector"))
  expect_error_missing_names(abcd = bad(fake_abcd(), "year"))
  expect_error_missing_names(abcd = bad(fake_abcd(), "emission_factor"))
  expect_error_missing_names(abcd = bad(fake_abcd(), "production"))

  scen <- fake_co2_scenario()
  expect_error_missing_names(scenario = bad(scen, "sector"))
  expect_error_missing_names(scenario = bad(scen, "year"))
  expect_error_missing_names(scenario = bad(scen, "emission_factor"))
})

test_that("without `sector` throws no error", {
  # RMI-PACTA/r2dii.analysis/pull/62#issuecomment-634651157
  without_sector <- select(fake_matched(sector_abcd = "cement"), -sector)
  expect_error_free(
    target_sda(
      without_sector,
      abcd = fake_abcd(sector = "cement"),
      co2_intensity_scenario = fake_co2_scenario(),
      region_isos = region_isos_stable
    )
  )
})

test_that("properly weights emissions factors", {
  companies <- c("a", "b")

  out <- target_sda(
    fake_matched(
      id_loan = c(1, 2),
      name_abcd = companies,
      sector_abcd = "cement"
    ),
    abcd = fake_abcd(
      name_company = companies,
      sector = "cement",
      technology = "cement",
      year = 2020,
      emission_factor = c(1, 2)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050),
      emission_factor = c(0.6, 0.2)
    ),
    region_isos = region_isos_stable
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
    fake_matched(sector_abcd = "cement"),
    abcd = fake_abcd(
      sector = "cement",
      technology = "cement",
      year = c(2020, 2021, 2022),
      emission_factor = c(1, 2, 3)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050), emission_factor = c(0.6, 0.2)
    ),
    region_isos = region_isos_stable
  )

  expected_names <- c(
    "sector",
    "year",
    "region",
    "scenario_source",
    "emission_factor_metric",
    "emission_factor_value"
  )
  expect_named(out, expected_names)
})

test_that("with known input outputs as expected", {
  # TODO: Re-factor this test into smaller isolated expected output tests
  matched <- fake_matched(sector_abcd = "cement")

  abcd <- fake_abcd(
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
    abcd,
    co2_intensity_scenario,
    region_isos = region_isos_stable
  ) %>%
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
  no_matches <- fake_matched(sector_abcd = "bad")

  if (packageVersion("testthat") >= "2.99.0.9000") {
    expect_warning(
      class = "no_match",
      target_sda(no_matches, fake_abcd(), fake_co2_scenario())
    )
  } else {
    expect_warning(
      target_sda(no_matches, fake_abcd(), fake_co2_scenario()), "no match"
    )
  }

  bad_scenario <- fake_co2_scenario(sector = "bad")
  expect_warning(
    target_sda(
      fake_matched(),
      fake_abcd(),
      bad_scenario,
      region_isos = region_isos_stable
      ),
    class = "no_match"
  )
})

test_that("with duplicated id_loan weights emission_factor as expected (#160)", {
  match_result <- fake_matched(
    id_loan = c(1, 1),
    name_abcd = rep("large company", 2),
    sector_abcd = "cement"
  )

  abcd <- fake_abcd(
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
      abcd,
      scen,
      region_isos = region_isos_stable
    ) %>%
      filter(year == min(year)),
    class = "unique_ids"
  )
})

test_that("with NAs in crucial columns errors with informative message (#146)", {
  expect_error_crucial_NAs_portfolio <- function(name) {
    data <- fake_matched(sector_abcd = "cement")

    abcd <- fake_abcd(
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
        abcd,
        scen,
        region_isos = region_isos_stable
      )
    )
  }

  expect_error_crucial_NAs_abcd <- function(name) {
    match_result <- fake_matched(sector_abcd = "cement")

    data <- fake_abcd(
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
        scen,
        region_isos = region_isos_stable
      )
    )
  }

  expect_error_crucial_NAs_scenario <- function(name) {
    match_result <- fake_matched(sector_abcd = "cement")

    abcd <- fake_abcd(
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
        abcd,
        data,
        region_isos = region_isos_stable
      )
    )
  }

  expect_error_crucial_NAs_portfolio("name_abcd")
  expect_error_crucial_NAs_portfolio("sector_abcd")

  expect_error_crucial_NAs_abcd("name_company")
  expect_error_crucial_NAs_abcd("sector")
  expect_error_crucial_NAs_abcd("year")

  expect_error_crucial_NAs_scenario("sector")
  expect_error_crucial_NAs_scenario("year")
  expect_error_crucial_NAs_scenario("emission_factor")
})

test_that("with multiple technologies weights emission_factor as expected (#160)", {
  match_result <- fake_matched(
    id_loan = c(1, 2),
    name_abcd = rep("large company", 2),
    sector_abcd = "cement"
  )

  abcd <- fake_abcd(
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
    abcd,
    scen,
    region_isos = region_isos_stable
  ) %>%
    filter(year == min(year)) %>%
    split(.$emission_factor_metric)

  expect_equal(out$projected$emission_factor_value, 2)
  expect_equal(out$target_b2ds$emission_factor_value, 2)
})

test_that("with multiple technologies, aggregates production-weighted emission_factor (#160)", {
  out <- target_sda(
    fake_matched(sector_abcd = "cement"),
    abcd = fake_abcd(
      sector = "cement",
      technology = c("cement 1", "cement 2"),
      year = 2020,
      production = c(1, 3),
      emission_factor = c(4, 132)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050), emission_factor = c(0.6, 0.2)
    ),
    region_isos = region_isos_stable
  ) %>%
    split(.$emission_factor_metric)

  expect_equal(out$corporate_economy$emission_factor_value, 100)
})

test_that("with multiple plant_location, aggregates production-weighted emission_factor (#160)", {
  out <- target_sda(
    fake_matched(sector_abcd = "cement"),
    abcd = fake_abcd(
      sector = "cement",
      plant_location = c("de", "fr"),
      year = 2020,
      production = c(1, 3),
      emission_factor = c(4, 132)
    ),
    co2_intensity_scenario = fake_co2_scenario(
      year = c(2020, 2050), emission_factor = c(0.6, 0.2)
    ),
    region_isos = region_isos_stable
  ) %>%
    filter(region == "global") %>%
    split(.$emission_factor_metric)

  expect_equal(out$corporate_economy$emission_factor_value, 100)
})

test_that("filters and warns when input-data has NAs", {
  # Work around: in testthat v2, the `class` argument seems to now work
  # https://gist.github.com/maurolepore/c04388c6d4795561fb168172e75154c0
  .object <- rlang::expr(
    out <- target_sda(
      fake_matched(sector_abcd = "cement"),
      abcd = fake_abcd(
        sector = "cement",
        technology = rep(c("cement", "bad"), 2),
        year = rep(c(2020, 2050), 2),
        emission_factor = c(1, 2, rep(NA, 2))
      ),
      co2_intensity_scenario = fake_co2_scenario(
        year = c(2020, 2050), emission_factor = c(0.6, 0.2)
      ),
      region_isos = region_isos_stable
    )
  )
  if (packageVersion("testthat") >= "2.99.0.9000") {
    args <- list(object = .object, class = "na_crucial_economic_input")
  } else {
    args <- list(object = .object, regexp = "emission_factor.*NA")
  }
  do.call(expect_warning, args)

  out <- split(out, out$emission_factor_metric)
  expect_equal(out$corporate_economy$emission_factor_value, c(1, 2))
})

test_that("filters and warns when input-data (production in abcd) has NAs #304", {
  matched <- fake_matched(
    id_loan = c("L1", "L2"),
    sector_abcd = c("cement", "power")
  )

  abcd <- fake_abcd(
    sector = c("cement", "power"),
    production = c(1,NA)
  )

  co2_scenario <- fake_co2_scenario(
    sector = "cement",
    emission_factor = c(1, 0.6),
    year = c(2025, 2026)
  )

  expect_warning(
    target_sda(
      matched,
      abcd,
      co2_scenario,
      region_isos = region_isos_stable),
    class = "na_crucial_economic_input"
    )
})


test_that(
  "`sector` column is not used from data (should only use `sector_abcd`) (#178)",
  {
    expect_error_free(
      target_sda(
        fake_matched(
          sector_abcd = "cement"
        ) %>% select(-sector),
        fake_abcd(
          sector = "cement",
          year = c(2020, 2050)
        ),
        co2_intensity_scenario = fake_co2_scenario(
          emission_factor = c(1, 2),
          year = c(2020, 2050)
        ),
        region_isos = region_isos_stable
      )
    )
  }
)

test_that("with multiple values of `country_of_domicile` outputs the expected
          `emission_factor_value` (#171)", {
  this_company <- "company"
  this_sector <- "steel"
  abcd <- fake_abcd(
    country_of_domicile = c("a", "b"),
    emission_factor = 0.5,
    year = 2020,
    name_company = this_company,
    sector = this_sector
  )

  matched <- fake_matched(
    name_abcd = this_company,
    sector = this_sector,
    sector_abcd = this_sector
  )

  out <- matched %>%
    target_sda(
      abcd,
      co2_intensity_scenario_demo,
      region_isos = region_isos_stable
    ) %>%
    split(.$emission_factor_metric)

  expect_equal(out$projected$emission_factor_value, 0.5)
})

test_that("outputs same target regardless of years present in abcd", {
  matched <- fake_matched(
    name_abcd = "company a",
    sector_abcd = "steel"
  )


  abcd_ten_year <- fake_abcd(
    sector = "steel",
    technology = "steel",
    name_company = c(rep("company a", 3), rep("company b", 3)),
    emission_factor = c(rep(1.5, 3), rep(2.5, 3)),
    year = rep(c(2020, 2025, 2030), 2),
    plant_location = "DE"
  )

  abcd_thirty_year <- fake_abcd(
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

  out_ten_year <- target_sda(
    matched,
    abcd_ten_year,
    co2_scenario,
    region_isos = region_isos_stable
  ) %>%
    filter(
      year == 2030,
      emission_factor_metric == "target_b2ds"
    )

  out_thirty_year <- target_sda(
    matched,
    abcd_thirty_year,
    co2_scenario,
    region_isos = region_isos_stable
  ) %>%
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
    sector_abcd = c("cement", "power")
  )

  abcd <- fake_abcd(
    sector = c("cement", "power"),
  )

  co2_scenario <- fake_co2_scenario(
    sector = "cement",
    emission_factor = c(1, 0.6),
    year = c(2025, 2026)
  )

  out <- target_sda(
    matched,
    abcd,
    co2_scenario,
    region_isos = region_isos_stable
  )

  out_sectors <- unique(out$sector)
  scenario_sectors <- unique(co2_scenario$sector)

  expect_equal(
    setdiff(out_sectors, scenario_sectors),
    character(0)
  )
})

test_that("doesn't output NAs if abcd and scenario years are misaligned (#307,
          #346)", {
  matched <- fake_matched(
    sector_abcd = "cement"
  )

  abcd <- fake_abcd(
    sector = "cement",
    year = c(2024, 2025)
  )

  co2_scenario <- fake_co2_scenario(
    emission_factor = c(1, 0.6, 0.4),
    year = c(2023, 2025, 2026)
  )

  out <- target_sda(
    matched,
    abcd,
    co2_scenario,
    region_isos = region_isos_stable
  )

  expect_false(
    any(is.na(out$emission_factor_value))
  )
})

test_that("output useful error message when emission_factor is not of type double (#224)", {
  matched <- fake_matched()
  abcd <- fake_abcd()
  co2_intensity_scenario <- fake_co2_scenario()

  bad_abcd <- abcd %>%
    mutate(
      emission_factor = as.character(emission_factor)
    )

  expect_error(
    class = "crucial_column_wrong_type",
    target_sda(
      matched,
      bad_abcd,
      co2_intensity_scenario,
      region_isos = region_isos_stable
    )
  )
})

test_that("argument `weight_emission_factor` outputs correctly with known input (#376)", {
  matched <- fake_matched(
    id_loan = c("L1", "L2"),
    name_abcd = c("american cement", "boral cement"),
    sector = "cement",
    sector_abcd = "cement"
  )

  abcd <- fake_abcd(
    name_company = rep(c("american cement", "boral cement"), 2),
    sector = "cement",
    technology = "cement integrated facility",
    year = rep(c(2020, 2021), each = 2),
    emission_factor = rep(c(0.7, 0.5), 2)
  )

  out <- matched %>%
    target_sda(
      abcd,
      fake_co2_scenario(year = c(2020, 2021), emission_factor = c(1, 0.7)),
      by_company = TRUE,
      region_isos = region_isos_stable
    ) %>%
    filter(year == 2020, emission_factor_metric == "target_b2ds") %>%
    split(.$name_abcd)


  abcd <- abcd %>%
    filter(!is.na(emission_factor), year == 2020) %>%
    select(name_company, year, emission_factor) %>%
    split(.$name_company)

  expect_equal(
    out$`american cement`$emission_factor_value,
    abcd$`american cement`$emission_factor
  )

  expect_equal(
    out$`boral cement`$emission_factor_value,
    abcd$`boral cement`$emission_factor
  )
})

test_that("outputs empty tibble for sectors in `scenario` and `abcd` but not
          `data` (#390)", {

  abcd <- fake_abcd(
    sector = c("cement", "steel")
  )

  scenario <- fake_co2_scenario(
    sector = rep(c("cement", "steel"), each = 2),
    year = rep(c(2025, 2026), 2),
    emission_factor = rep(c(1, 2), 2)
  )

  out <- target_sda(
    fake_matched(sector_abcd = "cement"),
    abcd,
    scenario,
    region_isos = region_isos_stable
  )

  out_steel <- filter(out, sector == "steel")

  expect_equal(nrow(out_steel), 0L)
})

test_that("region_isos only has lowercase isos #398", {

  bad_region_isos <- mutate(region_isos_demo, isos = toupper(isos))

  expect_warning(
    class = "column_not_in_lowercase",
    target_sda(
      fake_matched(sector_abcd = "cement", "steel"),
      fake_abcd(sector = c("cement", "steel")),
      fake_co2_scenario(),
      region_isos = bad_region_isos
    )
  )
})
