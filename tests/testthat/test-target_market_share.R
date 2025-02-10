library(r2dii.data)

test_that("w/ bad `data`, errors with informative message", {
  expect_error(target_market_share(
    "bad",
    fake_abcd(),
    fake_scenario()
  ), "data.frame.*not.*TRUE")
})

test_that("outputs a tibble", {
  out <- target_market_share(
    fake_matched(),
    fake_abcd(),
    fake_scenario(),
    region_isos_stable
  )
  expect_s3_class(out, "tbl_df")
})

test_that("outputs is ungrouped", {
  out <- target_market_share(
    fake_matched(),
    fake_abcd(),
    fake_scenario(),
    region_isos_stable
  )
  expect_false(dplyr::is_grouped_df(out))
})

test_that("warns when input data is grouped", {
  grouped_data <- group_by(fake_matched(), id_loan)

  expect_warning(
    target_market_share(
      grouped_data,
      fake_abcd(),
      fake_scenario(),
      region_isos_stable
    ),
    "Ungrouping"
  )
})

test_that("w/ fake data, outputs known value", {
  out <- target_market_share(
    fake_matched(),
    fake_abcd(),
    fake_scenario(),
    region_isos_stable
  )
  expect_snapshot(out)
})

test_that("w/ abcd lacking crucial columns, errors with informative message", {
  expect_error_abcd_missing_names <- function(name) {
    bad_abcd <- rename(
      fake_abcd(),
      bad = all_of(name)
    )

    expect_error(
      class = "missing_names",
      target_market_share(
        fake_matched(),
        bad_abcd,
        fake_scenario()
      )
    )
  }

  expect_error_abcd_missing_names("sector")
  expect_error_abcd_missing_names("technology")
  expect_error_abcd_missing_names("year")
  expect_error_abcd_missing_names("name_company")
  expect_error_abcd_missing_names("production")
  expect_error_abcd_missing_names("plant_location")
  expect_error_abcd_missing_names("is_ultimate_owner")
})

test_that("w/ scenario lacking crucial columns, errors with informative message", {
  expect_error_scenario_missing_names <- function(name) {
    bad_scenario <- rename(
      fake_scenario(),
      bad = all_of(name)
    )

    expect_error(
      class = "missing_names",
      target_market_share(
        fake_matched(),
        fake_abcd(),
        bad_scenario
      )
    )
  }

  expect_error_scenario_missing_names("sector")
  expect_error_scenario_missing_names("technology")
  expect_error_scenario_missing_names("year")
  expect_error_scenario_missing_names("scenario")
  expect_error_scenario_missing_names("region")
  expect_error_scenario_missing_names("tmsr")
  expect_error_scenario_missing_names("smsp")
  expect_error_scenario_missing_names("scenario_source")
})

test_that("w/ NAs in crucial columns, errors with informative message", {
  expect_error_crucial_NAs_portfolio <- function(name) {
    data <- fake_matched(
      id_loan = c("i1", "i2"),
      loan_size_outstanding = c(40, 10)
    )

    data[1, name] <- NA
    expect_error(
      class = "some_value_is_missing",
      target_market_share(
        data,
        fake_abcd(),
        fake_scenario(),
        region_isos_stable
      )
    )
  }

  expect_error_crucial_NAs_abcd <- function(name) {
    data <- fake_abcd(
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
        region_isos_stable
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
        fake_abcd(),
        data,
        region_isos_stable
      )
    )
  }

  expect_error_crucial_NAs_portfolio("name_abcd")
  expect_error_crucial_NAs_portfolio("sector_abcd")

  expect_error_crucial_NAs_abcd("sector")
  expect_error_crucial_NAs_abcd("year")

  expect_error_crucial_NAs_scenario("scenario")
  expect_error_crucial_NAs_scenario("tmsr")
  expect_error_crucial_NAs_scenario("smsp")
})

test_that("fills and warns when input-data has NAs", {
  matched <- fake_matched()
  abcd <- fake_abcd(production = c(1, NA))
  scenario <- fake_scenario()

  expect_warning(
    target_market_share(
      matched,
      abcd,
      scenario,
      region_isos_stable),
      class = "fill_nas_crucial_economic_input")
})

test_that("outputs expected names", {
  expected_output_names <- c(
    "sector",
    "technology",
    "year",
    "region",
    "scenario_source",
    "metric",
    "production",
    "technology_share",
    "scope",
    "percentage_of_initial_production_by_scope"
  )

  out <- target_market_share(
    fake_matched(),
    fake_abcd(),
    fake_scenario(),
    region_isos_stable
  )

  expect_named(out, expected_output_names)
})

test_that("w/ known input, outputs target production as expected", {
  portfolio <- fake_matched(
    name_abcd = "comp1"
  )

  abcd <- fake_abcd(
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

  out <- target_market_share(portfolio, abcd, scenario, region_isos_stable)
  out_target <- out %>%
    filter(metric == "target_sds") %>%
    arrange(.data$technology, .data$year)

  expect_equal(out_target$production, c(200, 353, 250, 150))
})

test_that("w/ known input, outputs target production as expected, at company level", {
  portfolio <- fake_matched(
    id_loan = c("i1", "i2"),
    name_abcd = c("comp1", "comp2")
  )

  abcd <- fake_abcd(
    technology = c("electric", "ice", "electric", "ice", "electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2021, 2021, 2020, 2020, 2021, 2021),
    name_company = paste0("comp", c(rep(1, 4), rep(2, 4))),
    production = c(10, 30, 20, 20, 90, 95, 100, 100)
  )

  scenario <- fake_scenario(
    technology = c("electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2021, 2021),
    tmsr = c(1, 1, 1.85, 0.6),
    smsp = c(0, 0, 0.34, -0.2)
  )

  out <- target_market_share(
    portfolio,
    abcd,
    scenario,
    region_isos_stable,
    by_company = TRUE,
    weight_production = FALSE
  )
  out_target <- out %>%
    filter(metric == "target_sds") %>%
    arrange(.data$technology, .data$year, .data$name_abcd)

  expect_equal(
    out_target$production,
    c(10, 90, 23.6, 152.9, 30, 95, 18, 57)
  )
})

test_that("w/ known input, outputs corporate_economy production as expected", {
  portfolio <- fake_matched()

  abcd <- fake_abcd(
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
    abcd,
    scenario,
    region_isos_stable,
    by_company = TRUE,
    weight_production = FALSE
  )

  out_corporate_economy <- out %>%
    filter(metric == "corporate_economy") %>%
    arrange(.data$technology, .data$year)

  expect_equal(
    out_corporate_economy$production,
    c(rep(30, 3), rep(90, 3))
  )
})

test_that("outputs identical values at start year (#47, #87)", {
  matched <- fake_matched(
    id_loan = c("i1", "i2"),
    sector = c("power", "automotive"),
    sector_abcd = c("power", "automotive")
  )

  abcd <- fake_abcd(
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
    abcd,
    scenario,
    region_isos_stable
  ) %>%
    filter(year == min(year)) %>%
    group_by(sector, technology, region) %>%
    summarize(
      distinct_intial_values = n_distinct(production), .groups = "drop"
    ) %>%
    mutate(initial_values_are_equal = (.data$distinct_intial_values == 1))

  expect_true(all(out$initial_values_are_equal))
})

test_that("corporate economy only aggregates ultimate owners (#103)", {
  out <- target_market_share(
    fake_matched(
      id_loan = c("i1", "i2"),
      name_abcd = c("company a", "company b")
    ),
    fake_abcd(
      name_company = c("company a", "company b", "company a", "company b"),
      is_ultimate_owner = c(T, F, T, F),
      production = c(50, 100, 100, 50),
      year = c(2020, 2020, 2021, 2021)
    ),
    fake_scenario(year = c(2020, 2021)),
    region_isos_stable
  )

  corporate_economy_value <- out %>%
    filter(metric == "corporate_economy")

  expect_equal(corporate_economy_value$production, c(50, 100))
})

test_that("`data$sector` is not used (should only use `data$sector_abcd`) (#178)", {
  expect_error_free(
    target_market_share(
      fake_matched() %>% select(-sector),
      fake_abcd(),
      fake_scenario(),
      region_isos_stable
    )
  )
})

test_that("outputs known value with `weight_production` (#131)", {
  matched <- fake_matched(
    id_loan = c(1, 2),
    loan_size_outstanding = c(1, 9),
    name_abcd = c("a", "b")
  )

  abcd <- fake_abcd(
    name_company = c("a", "b"),
    production = c(1, 2)
  )

  out_weighted <- target_market_share(
    matched,
    abcd = abcd,
    scenario = fake_scenario(),
    region_isos = region_isos_stable,
    weight_production = TRUE
  ) %>%
    split(.$metric)

  expect_equal(out_weighted$projected$production, 1.9)

  out_unweighted <- target_market_share(
    matched,
    abcd = abcd,
    scenario = fake_scenario(),
    region_isos = region_isos_stable,
    weight_production = FALSE
  ) %>%
    split(.$metric)

  expect_equal(out_unweighted$projected$production, 3)
})

test_that("warns if `by_company` & `weight_production` are both TRUE (#165)", {
  expect_warning(
    target_market_share(
      fake_matched(),
      abcd = fake_abcd(),
      scenario = fake_scenario(),
      region_isos = region_isos_stable,
      by_company = TRUE,
      weight_production = TRUE
    ),
    "`by_company = TRUE` and `weight_production = TRUE`"
  )
})

test_that("w/ `by_company = TRUE` outputs additional column `name_abcd` (#291)", {
  by_company_false <- target_market_share(
    data = fake_matched(),
    abcd = fake_abcd(),
    scenario = fake_scenario(),
    region_isos = region_isos_stable,
    by_company = FALSE
  )

  by_company_true <- suppressWarnings(
    target_market_share(
      data = fake_matched(),
      abcd = fake_abcd(),
      scenario = fake_scenario(),
      region_isos = region_isos_stable,
      by_company = TRUE
    )
  )

  additional_column <- setdiff(names(by_company_true), names(by_company_false))
  expect_equal(additional_column, "name_abcd")
})

test_that("outputs same names regardless of the value of `weight_production` (#186)", {
  out_weighted <- target_market_share(
    fake_matched(),
    fake_abcd(),
    fake_scenario(),
    region_isos_stable,
    weight_production = TRUE
  )

  out_unweighted <- target_market_share(
    fake_matched(),
    fake_abcd(),
    fake_scenario(),
    region_isos_stable,
    weight_production = FALSE
  )

  diff_names <- setdiff(names(out_unweighted), names(out_weighted))

  expect_equal(diff_names, character(0))
})

test_that("w/ known input, outputs `technology_share` as expected (#184, #262)", {
  matched <- fake_matched(
    id_loan = c("L1", "L2"),
    loan_size_outstanding = c(1, 3),
    name_abcd = c("a", "b")
  )

  abcd <- fake_abcd(
    name_company = rep(c("a", "b"), each = 2),
    technology = rep(c("ice", "electric"), 2),
    production = c(1, 1, 1, 3)
  )

  scenario <- fake_scenario(
    technology = c("ice", "electric"),
    smsp = 1
  )

  out <- target_market_share(
    matched,
    abcd,
    scenario,
    region_isos_stable
  ) %>%
    split(.$metric)

  expect_equal(
    out$projected$technology_share,
    c(0.6875, 0.3125)
  )

  FIXME <- 1e-2 # Do we need 1e-3?
  expect_equal(
    out$corporate_economy$technology_share,
    c(0.666, 0.333),
    tolerance = FIXME
  )

  expect_equal(
    out$target_sds$technology_share,
    c(0.914, 0.086),
    tolerance = 1e-3
  )

  ################ More extensive test (#262)

  matched <- fake_matched(
    id_loan = c("L1", "L2", "L3"),
    loan_size_outstanding = c(1000, 15000, 1200),
    name_abcd = c("a", "b", "a"),
    id_2dii = c("DL1", "DL2", "DL1"),
    sector = "power",
    sector_abcd = "power"
  )

  abcd <- fake_abcd(
    name_company = rep(c("a", "b"), each = 6),
    sector = "power",
    technology = rep(
      c(
        "coalcap",
        "gascap",
        "hydrocap",
        "nuclearcap",
        "oilcap",
        "renewablescap"
      ),
      2
    ),
    production = c(100, 200, 300, 100, 100, 200, 500, 0, 0, 300, 100, 100),
    year = 2020
  )

  scenario <- fake_scenario(
    sector = "power",
    technology = c(
      "coalcap",
      "gascap",
      "hydrocap",
      "nuclearcap",
      "oilcap",
      "renewablescap"
    ),
    year = 2020,
    tmsr = 1,
    smsp = 0
  )

  out <- target_market_share(
    matched,
    abcd,
    scenario,
    region_isos_stable
  ) %>%
    split(.$metric)

  expect_equal(
    out$projected$technology_share,
    c(0.4488, 0.0255, 0.0383, 0.2744, 0.1, 0.1128),
    tolerance = 1e-3
  )

  expect_equal(
    out$target_sds$technology_share,
    c(0.4488, 0.0255, 0.0383, 0.2744, 0.1, 0.1128),
    tolerance = 1e-3
  )

  expect_equal(
    out$corporate_economy$technology_share,
    c(0.3, 0.1, 0.15, 0.2, 0.1, 0.15),
    tolerance = 1e-3
  )
})

test_that("w/ some region missing some scenario, outputs expected `production`
          values (#203)", {
  scenario <- fake_scenario(
    scenario = c("cps", "sds", "sds"),
    region = c("global", "global", "non opec"),
    scenario_source = "weo_2019"
  )

  out <- target_market_share(fake_matched(), fake_abcd(), scenario)

  target_cps <- out %>%
    filter(metric == "target_cps") %>%
    split(.$region)
  target_sds <- out %>%
    filter(metric == "target_sds") %>%
    split(.$region)

  expect_equal(target_cps$global$production, 0.5)
  expect_equal(target_sds$global$production, 0.5)
  expect_equal(target_sds$`non opec`$production, 0.5)
})

test_that("w/ no matching regions, outputs empty named tibble", {
  out <- suppressWarnings(
    target_market_share(
      fake_matched(),
      fake_abcd(),
      fake_scenario()
    )
  )

  expect_equal(nrow(out), 0L)

  good_names <- names(
    target_market_share(
      fake_matched(),
      fake_abcd(),
      fake_scenario(),
      region_isos_stable
    )
  )

  expect_equal(setdiff(names(out), good_names), character(0))
})

test_that("w/ technology in abcd but not loanbook, outputs all techs (#235)", {
  matched <- fake_matched(
    name_abcd = "company a"
  )

  abcd <- fake_abcd(
    name_company = c("company a", "company b"),
    technology = c("ice", "electric"),
    production = c(1, 3)
  )

  scenario <- fake_scenario(
    technology = c("electric", "ice")
  )

  out <- target_market_share(
    matched,
    abcd,
    scenario,
    region_isos_stable
  )

  corporate_economy <- out %>%
    filter(metric == "corporate_economy") %>%
    split(.$technology)

  expect_equal(corporate_economy$ice$technology_share, 0.25)
  expect_equal(corporate_economy$electric$technology_share, 0.75)
})

test_that("w/ unweighted company flags & multi loans, outputs correctly (#239)", {
  matched <- fake_matched(id_loan = c("L1", "L2"))
  abcd <- fake_abcd()
  scenario <- fake_scenario()

  out <- target_market_share(
    matched,
    abcd,
    scenario,
    region_isos_stable,
    by_company = TRUE,
    weight_production = FALSE
  )

  projected <- filter(out, metric == "projected")

  expect_equal(projected$production, abcd$production)
})

test_that("w/ multiple loans to same company, `technology_share` sums to one (#218)", {
  shares_sum_to_one <- function(data) {
    out <- data %>%
      group_by(sector, metric) %>%
      summarize(sum_of_shares = sum(technology_share), .groups = "drop")

    all(out$sum_of_shares == 1)
  }

  # multiple loans to same company
  out <- target_market_share(
    fake_matched(id_loan = c("L1", "L2")),
    fake_abcd(),
    fake_scenario(),
    region_isos_stable
  )

  expect_true(shares_sum_to_one(out))
})

test_that("w/ multiple match `level`, unweighted production is equal to abcd production (#249)", {
  matched <- fake_matched(
    id_loan = c(1, 2),
    level = c("direct_loantaker", "ultimate_parent"),
  )

  out <- target_market_share(
    matched,
    fake_abcd(),
    fake_scenario(),
    r2dii.data::region_isos_demo,
    by_company = TRUE,
    weight_production = FALSE
  )

  abcd_production <- fake_abcd() %>%
    dplyr::pull(production)

  out_production <- filter(out, metric == "projected") %>%
    dplyr::pull(production)

  expect_equal(abcd_production, out_production)
})

test_that("for one company with multiple loans of different size, unweighted
          production by company equals abcd-production (#239)", {
  different_loan_size <- 1:2
  matched <- fake_matched(
    id_loan = c("L1", "L2"),
    loan_size_credit_limit = different_loan_size,
    loan_size_outstanding = different_loan_size
  )

  projected <- target_market_share(
    matched,
    fake_abcd(),
    scenario = fake_scenario(),
    region_isos = region_isos_stable,
    by_company = TRUE,
    weight_production = FALSE
  ) %>%
    filter(metric == "projected")

  expect_equal(projected$production, fake_abcd()$production)
})

test_that("w/ bad column, errors with informative message (#267)", {
  bad_matched <- fake_matched(
    bad_column = "bad"
  )

  expect_error(
    class = "invalid_columns",
    target_market_share(
      bad_matched,
      fake_abcd(),
      fake_scenario()
    )
  )
})

test_that("`technology_share` outputs consistently when multiple
          direct_loantakers match to a single company (#265)", {
  matched <- fake_matched(
    id_loan = c("L1", "L2", "L3", "L4", "L5"),
    name_abcd = c(rep("company a", 4), "company b")
  )

  matched_split_dl <- matched %>%
    mutate(name_direct_loantaker = c("company a1", "company a2", "company a3", "company a4", "company b"))

  abcd <- fake_abcd(
    name_company = rep(c("company a", "company b"), each = 2),
    technology = rep(c("ice", "electric"), 2),
    production = c(8, 2, 15, 5)
  )

  scenario <- fake_scenario(
    technology = c("ice", "electric")
  )

  out <- target_market_share(
    matched,
    abcd,
    scenario,
    region_isos_stable
  ) %>%
    filter(
      metric == "projected",
      year == 2025,
      technology == "ice"
    )

  out_split_dl <- target_market_share(
    matched_split_dl,
    abcd,
    scenario,
    region_isos_stable
  ) %>%
    filter(
      metric == "projected",
      year == 2025,
      technology == "ice"
    )

  expect_equal(out$technology_share, out_split_dl$technology_share)
})

test_that("`technology_share` outputs consistently when multiple
          loans at different levels match to a single company (#265)", {
  matched_same_level <- fake_matched(
    id_loan = c("L1", "L2", "L3"),
    name_direct_loantaker = c("company a1", "company a2", "company b"),
    name_abcd = c("company a", "company a", "company b")
  )

  matched_diff_level <- matched_same_level %>%
    mutate(
      level = c("ultimate_parent", "direct_loantaker", "ultimate_parent")
    )

  abcd <- fake_abcd(
    name_company = rep(c("company a", "company b"), each = 2),
    technology = rep(c("ice", "electric"), 2),
    production = c(8, 2, 15, 5)
  )

  scenario <- fake_scenario(
    technology = c("ice", "electric")
  )

  out_same_level <- target_market_share(
    matched_same_level,
    abcd,
    scenario,
    region_isos_stable
  ) %>%
    filter(
      metric == "projected",
      year == 2025,
      technology == "ice"
    )

  out_diff_level <- target_market_share(
    matched_diff_level,
    abcd,
    scenario,
    region_isos_stable
  ) %>%
    filter(
      metric == "projected",
      year == 2025,
      technology == "ice"
    )

  expect_equal(
    out_same_level$technology_share,
    out_diff_level$technology_share
  )
})

test_that("projects technology share as 'production / total production' when
          computing by company, unweighted by relative loan size (#288)", {
  .production <- c(1, 10)
  .year <- 2022
  .company <- "toyota motor corp"
  .sector <- "automotive"
  .technology <- c("hybrid", "ice")

  abcd <- tibble(
    production = .production,
    name_company = .company,
    technology = .technology,
    sector = .sector,
    year = .year,
    plant_location = c("US"),
    emission_factor = 1,
    is_ultimate_owner = TRUE
  )

  matched <- tibble(
    sector = .sector,
    sector_abcd = .sector,
    name_abcd = .company,
    id_loan = "L1",
    loan_size_outstanding = 1,
    loan_size_outstanding_currency = "XYZ",
    loan_size_credit_limit = 1,
    loan_size_credit_limit_currency = "XYZ",
    id_2dii = "DL1",
    level = "direct_loantaker",
    score = 1
  )

  scenario <- tibble(
    sector = .sector,
    scenario = "cps",
    technology = .technology,
    region = "global",
    year = .year,
    tmsr = 1,
    smsp = c(0.100, 0.101),
    scenario_source = "demo_2020"
  )

  region <- tibble(
    region = "global",
    isos = "us",
    source = "demo_2020"
  )

  out <- matched %>%
    target_market_share(
      abcd = abcd,
      scenario = scenario,
      region_isos = region,
      by_company = TRUE,
      weight_production = FALSE
    ) %>%
    filter(metric == "projected")

  expect_equal(
    out$technology_share,
    out$production / sum(out$production)
  )

  expect_equal(
    out$production,
    .production
  )
})

test_that("initial value of technology_share consistent between `projected` and
          `target_*` (#277)", {
  matched <- fake_matched(
    id_loan = c("L1", "L2"),
    name_abcd = c("company a", "company b")
  )

  abcd <- fake_abcd(
    name_company = c("company a", "company b", "company a", "company b"),
    technology = c("ice", "ice", "electric", "electric"),
    production = c(100, 1, 100, 3),
    year = 2020
  )

  scenario <- fake_scenario(
    technology = c("ice", "electric"),
    year = 2020,
    tmsr = 1,
    smsp = 0
  )

  out <- target_market_share(
    matched,
    abcd,
    scenario,
    region_isos_stable
  ) %>%
    filter(
      metric %in% c("projected", "target_sds"),
      year == 2020
    ) %>%
    select(technology, metric, technology_share)

  out_ice <- out %>%
    filter(technology == "ice") %>%
    split(.$metric)

  out_electric <- out %>%
    filter(technology == "electric") %>%
    split(.$metric)

  expect_equal(
    out_ice$projected$technology_share,
    out_ice$target_sds$technology_share
  )
  expect_equal(
    out_electric$projected$technology_share,
    out_electric$target_sds$technology_share
  )
})

test_that("w/ different currencies in input, errors with informative message (#279)", {
  matched <- fake_matched(
    id_loan = c("i1", "i2"),
    loan_size_outstanding_currency = c("USD", "EUR"),
    loan_size_credit_limit_currency = c("USD", "EUR")
  )

  # test outstanding
  expect_error(
    target_market_share(
      matched,
      fake_abcd(),
      fake_scenario(),
      region_isos_stable
    ),
    class = "multiple_currencies"
  )

  # test credit limit
  expect_error(
    target_market_share(
      matched,
      fake_abcd(),
      fake_scenario(),
      region_isos_stable,
      use_credit_limit = TRUE
    ),
    class = "multiple_currencies"
  )
})

test_that("input with only green technologies, outputs only green technologies
          (#318)", {
  scenario <- fake_scenario(
    year = rep(c(2020, 2025), 2),
    tmsr = c(1, 0.5, 1, 2),
    smsp = c(0, -0.5, 0, 0.5),
    technology = rep(c("ice", "electric"), each = 2),
  )

  abcd_green <- fake_abcd(
    technology = "electric"
  )

  # testing that input with only green techs outputs only green techs
  out <- target_market_share(
    fake_matched(),
    abcd_green,
    scenario,
    region_isos_stable
  )

  expect_equal(
    setdiff(
      c("electric"),
      unique(out$technology)
    ),
    character(0)
  )
})

test_that("technology_share is calculated per region (#315)", {
  matched <- fake_matched(
    id_loan = c("L1", "L2"),
    name_abcd = c("increasing", "decreasing")
  )

  abcd <- fake_abcd(
    name_company = c("decreasing", "increasing"),
    technology = c("ice", "electric"),
    plant_location = c("US", "FR"),
  )

  scenario <- fake_scenario(
    technology = rep(c("electric", "ice"), 2),
    region = rep(c("europe", "global"), each = 2),
    tmsr = 1,
    smsp = 0,
    year = 2025
  )

  out <- target_market_share(
    matched,
    abcd,
    scenario,
    region_isos_stable
  ) %>%
    filter(
      metric != "corporate_economy",
      region == "global"
    ) %>%
    split(.$metric)

  expect_equal(
    out$projected$technology_share,
    c(0.5, 0.5)
  )

  expect_equal(
    out$target_sds$technology_share,
    c(0.5, 0.5)
  )
})

test_that("input with only decreasing technologies, outputs both increasing  and decreasing
          technologies (#318)", {
  scenario <- fake_scenario(
    year = rep(c(2020, 2025), 2),
    tmsr = c(1, 0.5, 1, 2),
    smsp = c(0, -0.5, 0, 0.5),
    technology = rep(c("ice", "electric"), each = 2),
  )

  abcd_decreasing <- fake_abcd(
    technology = "ice"
  )

  # testing that input with only decreasing techs outputs both increasing and decreasing techs
  out <- target_market_share(
    fake_matched(),
    abcd_decreasing,
    scenario,
    region_isos_demo
  )

  expect_equal(
    setdiff(
      c("ice", "electric"),
      unique(out$technology)
    ),
    character(0)
  )
})

test_that("input with unexpected sectors errors gracefully (#329)", {
  matched <- fake_matched(
    sector_abcd = "a"
  )

  abcd <- fake_abcd(
    sector = "a"
  )

  scenario <- fake_scenario(
    sector = "a"
  )

  expect_warning(
    target_market_share(
      matched,
      abcd,
      scenario,
      region_isos_stable
    ),
    class = "has_zero_rows"
  )
})

test_that("outputs only sectors that are present in the input `data` (#329)", {
  matched <- fake_matched(
    sector_abcd = "automotive"
  )

  abcd <- fake_abcd(
    sector = c("automotive", "power"),
    technology = c("ice", "coalcap")
  )

  scenario <- fake_scenario(
    sector = c("automotive", "power")
  )

  out <- target_market_share(
    matched,
    abcd,
    scenario,
    region_isos_stable
  )

  expect_equal(
    unique(out$sector),
    "automotive"
  )
})

test_that("outputs only positive values of `production`(#336)", {
  abcd <- fake_abcd(
    year = c(2025, 2026)
  )

  scenario <- fake_scenario(
    technology = rep(c("ice", "electric"), 2),
    smsp = rep(c(0, -1), each = 2),
    year = rep(c(2025, 2026), each = 2)
  )

  out <- target_market_share(
    fake_matched(),
    abcd,
    scenario,
    region_isos_stable
  )

  expect_false(any(out$production < 0))
})

test_that("outputs as expected for companies with 0 initial sectoral production (#306)", {
  abcd <- fake_abcd(
    production = c(0, 1),
    year = c(2020, 2021)
  )

  scenario <- fake_scenario(
    tmsr = c(1, 0.5),
    year = c(2020, 2021)
  )

  out <- target_market_share(
    fake_matched(),
    abcd,
    scenario,
    region_isos_stable
  ) %>%
    arrange(.$year) %>%
    split(.$metric)


  expect_equal(out$corporate_economy$production, c(0, 1))
  expect_equal(out$projected$production, c(0, 1))
  expect_equal(out$target_sds$production, c(0, 0))


  expect_equal(out$corporate_economy$technology_share, c(0, 1))
  expect_equal(out$projected$technology_share, c(0, 1))
  expect_equal(out$target_sds$technology_share, c(0, 0))
})

test_that("outputs columns `percent_change_by_scope` and `scope`", {
  out <- target_market_share(
    fake_matched(),
    fake_abcd(),
    fake_scenario(),
    region_isos_stable
  )

  expected_added_columns <- c("scope", "percentage_of_initial_production_by_scope")

  expect_equal(setdiff(expected_added_columns, names(out)), character(0))
})

test_that("w/ known input, outputs `percent_of_initial_production_by_scope` as
          expected, at portfolio level", {
  portfolio <- fake_matched(
    id_loan = c("i1", "i2"),
    name_abcd = c("comp1", "comp2")
  )

  abcd <- fake_abcd(
    technology = c("electric", "ice", "electric", "ice", "electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2021, 2021, 2020, 2020, 2021, 2021),
    name_company = paste0("comp", c(rep(1, 4), rep(2, 4))),
    production = c(10, 30, 20, 20, 90, 95, 100, 100)
  )

  scenario <- fake_scenario(
    technology = c("electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2021, 2021),
    tmsr = c(1, 1, 1.85, 0.6),
    smsp = c(0, 0, 0.34, -0.2)
  )

  out <- target_market_share(
    portfolio,
    abcd,
    scenario,
    region_isos_stable,
    by_company = FALSE,
    weight_production = FALSE
  )

  out_percent <- out %>%
    filter(.$year == 2021) %>%
    split(.$metric)

  expect_equal(
    out_percent$corporate_economy$percentage_of_initial_production_by_scope,
    c(0.089, -0.040),
    tolerance = 1e-2
  )

  expect_equal(
    out_percent$projected$percentage_of_initial_production_by_scope,
    c(0.089, -0.040),
    tolerance = 1e-2
  )

  expect_equal(
    out_percent$target_sds$percentage_of_initial_production_by_scope,
    c(0.34, -0.40),
    tolerance = 1e-2
  )
})

test_that("w/ known input, outputs `percent_of_initial_production_by_scope` as
          expected, at company level", {
  portfolio <- fake_matched(
    id_loan = c("i1", "i2"),
    name_abcd = c("comp1", "comp2")
  )

  abcd <- fake_abcd(
    technology = c("electric", "ice", "electric", "ice", "electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2021, 2021, 2020, 2020, 2021, 2021),
    name_company = paste0("comp", c(rep(1, 4), rep(2, 4))),
    production = c(10, 30, 20, 20, 90, 95, 100, 100)
  )

  scenario <- fake_scenario(
    technology = c("electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2021, 2021),
    tmsr = c(1, 1, 1.85, 0.6),
    smsp = c(0, 0, 0.34, -0.2)
  )

  out <- target_market_share(
    portfolio,
    abcd,
    scenario,
    region_isos_stable,
    by_company = TRUE,
    weight_production = FALSE
  )

  out_percent <- out %>%
    filter(.$year == 2021) %>%
    arrange(.$name_abcd) %>%
    split(.$metric)

  expect_equal(
    out_percent$corporate_economy$percentage_of_initial_production_by_scope,
    c(0.089, -0.040),
    tolerance = 1e-2
  )

  expect_equal(
    out_percent$projected$percentage_of_initial_production_by_scope,
    c(0.25, -0.333, 0.054, 0.053),
    tolerance = 1e-2
  )

  expect_equal(
    out_percent$target_sds$percentage_of_initial_production_by_scope,
    c(0.34, -0.40, 0.34, -0.40),
    tolerance = 1e-2
  )
})

test_that("w/ abcd with older years than scenarios, outputs 0 percent change in
          start year", {
  abcd <- fake_abcd(
    year = c(2000, 2020, 2021),
    production = c(10, 30, 40)
  )

  scenario <- fake_scenario(
    technology = "ice",
    year = c(2020, 2021),
    tmsr = c(1, 0.6),
    smsp = c(0, -0.2)
  )

  out <- target_market_share(
    fake_matched(),
    abcd,
    scenario,
    region_isos_stable,
    by_company = FALSE,
    weight_production = FALSE
  )

  expect_equal(min(out$year), 2020L)

  initial_out <- out %>%
    filter(year == 2020L)

  initial_out <- split(initial_out, initial_out$metric)

  expect_equal(
    initial_out$corporate_economy$percentage_of_initial_production_by_scope,
    0L
  )

  expect_equal(
    initial_out$projected$percentage_of_initial_production_by_scope,
    0L
  )

  expect_equal(
    initial_out$target_sds$percentage_of_initial_production_by_scope,
    0L
  )
})

test_that("production column in scenario dataset is removed with a warning #372", {
  bad_scenario <- fake_scenario(production = 1)

  expect_warning(
    class = "scenario_production_column_removed",
    target_market_share(
      fake_matched(),
      fake_abcd(),
      bad_scenario,
      region_isos_stable
    )
  )
})

test_that("region_isos only has lowercase isos #398", {

  bad_region_isos <- mutate(region_isos_demo, isos = toupper(isos))

  expect_warning(
    class = "column_not_in_lowercase",
    target_market_share(
      fake_matched(),
      fake_abcd(),
      fake_scenario(),
      bad_region_isos
    )
  )
})

test_that("with `abcd` with `NA` for start year, replaces `NA` with 0 (#423)", {
  expect_warning(
    out <- target_market_share(
      fake_matched(),
      fake_abcd(year = c(2020, 2025), production = c(NA_real_, 1)),
      fake_scenario(year = c(2020, 2025)),
      region_isos_stable
    ),
    class = "fill_nas_crucial_economic_input"
  )

  out <- out %>%
    filter(metric == "projected") %>%
    arrange(year)

  expect_equal(out$production, c(0, 1))
})

test_that("correctly splits scenario names with hyphen #425", {

  out <- target_market_share(
    fake_matched(),
    fake_abcd(),
    fake_scenario(scenario = "1.5c-scen"),
    region_isos_stable
  ) %>%
    filter(grepl("target", metric))

  expect_equal(unique(out$metric), "target_1.5c-scen")

})

test_that("outputs `target` for full timeline of scenario #157", {

  out <- target_market_share(
    fake_matched(),
    fake_abcd(year = 2020),
    fake_scenario(scenario = "1.5c-scen", year = c(2020, 2025)),
    region_isos_stable
  ) %>%
    filter(grepl("target", metric))

  expect_equal(max(out$year), 2025L)

})

test_that("with duplicated id_loan throws informative error (#489)", {
  match_result <- fake_matched(
    id_loan = c(1, 1),
    name_abcd = rep("large company", 2),
    sector_abcd = "automotive"
  )

  abcd <- fake_abcd(
    sector = "automotive",
    technology = "ice",
    name_company = "large company",
    year = c(2020, 2025)
  )

  scen <- fake_scenario(
    year = c(2020, 2025),
    tmsr = c(1, 0.5)
  )

  expect_error(
    target_market_share(
      match_result,
      abcd,
      scen,
      region_isos = region_isos_stable
    ),
    class = "unique_ids"
  )
})

test_that("target_market_share() calculates target_* values for missing low carbon technologies (#495)", {
  match_result <- fake_matched(name_abcd = "company a")

  abcd <- fake_abcd(
    name_company = "company a",
    sector = c(rep("automotive", 2), rep("hdv", 6)),
    technology = c(rep("ice", 4), rep("hybrid", 2), rep("electric", 2)),
    year = rep(c(2020, 2025), 4)
  )

  scen <- fake_scenario(
    sector = "automotive",
    technology = c(rep("ice", 2), rep("hybrid", 2), rep("electric", 2)),
    year = rep(c(2020, 2025), 3),
    tmsr = c(1, 0.5, 1, 1.5, 1, 1.5),
    smsp = c(0, -0.08, 0, 0.1, 0, 0.1)
  )

  scen_technologies <- scen %>%
    dplyr::filter(.data$sector == "automotive") %>%
    dplyr::arrange(.data$technology) %>%
    dplyr::distinct(.data$technology) %>%
    dplyr::pull()

  results_tms_comp <- target_market_share(
    match_result,
    abcd,
    scen,
    region_isos = region_isos_stable,
    by_company = TRUE,
    weight_production = FALSE
  )

  results_tms_comp_targets <- results_tms_comp %>%
    dplyr::filter(
      .data$name_abcd == "company a",
      .data$sector == "automotive",
      grepl("target_", .data$metric)
    ) %>%
    dplyr::arrange(.data$technology) %>%
    dplyr::distinct(.data$technology) %>%
    dplyr::pull()

  expect_equal(
    results_tms_comp_targets,
    scen_technologies
  )

  results_tms_lbk <- target_market_share(
    match_result,
    abcd,
    scen,
    region_isos = region_isos_stable,
    by_company = FALSE,
    weight_production = TRUE
  )

  results_tms_lbk_targets <- results_tms_lbk %>%
    dplyr::filter(
      .data$sector == "automotive",
      grepl("target_", .data$metric)
    ) %>%
    dplyr::arrange(.data$technology) %>%
    dplyr::distinct(.data$technology) %>%
    dplyr::pull()

  expect_equal(
    results_tms_lbk_targets,
    scen_technologies
  )
})

test_that("columns in output match what is documented in `data_dictionary`", {
  out <- target_market_share(
    data = fake_matched(),
    abcd = fake_abcd(),
    scenario = fake_scenario(),
    region_isos = region_isos_stable
  )

  data_dict <- dplyr::filter(r2dii.analysis::data_dictionary, dataset == "target_market_share_output")

  expect_setequal(names(out), data_dict[["column"]])
  expect_mapequal(sapply(out, typeof), setNames(data_dict[["typeof"]], data_dict[["column"]]))
})
