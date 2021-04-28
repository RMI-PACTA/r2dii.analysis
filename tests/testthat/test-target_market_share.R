library(r2dii.data)

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
    region_isos_stable
  )
  expect_is(out, "tbl_df")
})

test_that("outputs is ungrouped", {
  out <- target_market_share(
    fake_matched(),
    fake_ald(),
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
      fake_ald(),
      fake_scenario(),
      region_isos_stable
    ),
    "Ungrouping"
  )
})

test_that("with fake data outputs known value", {
  out <- target_market_share(
    fake_matched(),
    fake_ald(),
    fake_scenario(),
    region_isos_stable
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
        region_isos_stable
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
        fake_ald(),
        data,
        region_isos_stable
      )
    )
  }

  expect_error_crucial_NAs_portfolio("name_ald")
  expect_error_crucial_NAs_portfolio("sector_ald")

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
    region_isos_stable
  )

  expect_named(
    out,
    c(
      "sector",
      "technology",
      "year",
      "region",
      "scenario_source",
      "metric",
      "production",
      "technology_share"
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

  out <- target_market_share(portfolio, ald, scenario, region_isos_stable)
  out_target <- out %>%
    filter(metric == "target_sds") %>%
    arrange(.data$technology, .data$year)

  expect_equal(out_target$production, c(200, 353, 250, 150))
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
    region_isos_stable,
    by_company = TRUE,
    weight_production = FALSE
  )
  out_target <- out %>%
    filter(metric == "target_sds") %>%
    arrange(.data$technology, .data$year, .data$name_ald)

  expect_equal(
    out_target$production,
    c(10, 90, 23.6, 152.9, 30, 95, 18, 57)
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
    region_isos_stable,
    by_company = TRUE,
    weight_production = FALSE
  )

  out_benchmark <- out %>%
    filter(metric == "corporate_economy") %>%
    arrange(.data$technology, .data$year)

  expect_equal(
    out_benchmark$production,
    c(rep(30, 3), rep(90, 3))
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
    region_isos_stable
  ) %>%
    filter(year == min(year)) %>%
    group_by(sector, technology, region) %>%
    summarize(distinct_intial_values = n_distinct(production)) %>%
    mutate(initial_values_are_equal = (.data$distinct_intial_values == 1))

  expect_true(all(out$initial_values_are_equal))
})

test_that("corporate economy benchmark only aggregates ultimate owners (#103)", {
  out <- target_market_share(
    fake_matched(name_ald = c("company a", "company b")),
    fake_ald(
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

test_that(
  "`sector` column is not used from data (should only use `sector_ald`) (#178)",
  {
    expect_error_free(
      target_market_share(
        fake_matched() %>% select(-sector),
        fake_ald(),
        fake_scenario(),
        region_isos_stable
      )
    )
  }
)

test_that("outputs known value with `weight_production` (#131)", {
  matched <- fake_matched(
    id_loan = c(1, 2),
    loan_size_outstanding = c(1, 9),
    name_ald = c("a", "b")
  )

  ald <- fake_ald(
    name_company = c("a", "b"),
    production = c(1, 2)
  )

  out_weighted <- target_market_share(
    matched,
    ald = ald,
    scenario = fake_scenario(),
    region_isos = region_isos_stable,
    weight_production = TRUE
  ) %>%
    split(.$metric)

  expect_equal(out_weighted$projected$production, 1.9)

  out_unweighted <- target_market_share(
    matched,
    ald = ald,
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
      ald = fake_ald(),
      scenario = fake_scenario(),
      region_isos = region_isos_stable,
      by_company = TRUE,
      weight_production = TRUE
    ),
    "`by_company = TRUE` and `weight_production = TRUE`"
  )
})

test_that("outputs same names regardless of the value of `weight_production` (#186)", {
  out_weighted <- target_market_share(
    fake_matched(),
    fake_ald(),
    fake_scenario(),
    region_isos_stable,
    weight_production = TRUE
  )

  out_unweighted <- target_market_share(
    fake_matched(),
    fake_ald(),
    fake_scenario(),
    region_isos_stable,
    weight_production = FALSE
  )

  diff_names <- setdiff(names(out_unweighted), names(out_weighted))

  expect_equal(diff_names, character(0))
})

test_that("with known input outputs `technology_share` as expected (#184, #262)", {
  matched <- fake_matched(
    id_loan = c("L1", "L2"),
    loan_size_outstanding = c(1, 3),
    name_ald = c("a", "b")
  )

  ald <- fake_ald(
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
    ald,
    scenario,
    region_isos_stable
  ) %>%
    split(.$metric)

  expect_equal(
    out$projected$technology_share,
    c(0.6875, 0.3125)
  )

  expect_equal(
    out$corporate_economy$technology_share,
    c(0.666, 0.333),
    tolerance = 1e-3
  )

  expect_equal(
    out$target_sds$technology_share,
    c(0.923, 0.076),
    tolerance = 1e-3
  )

  ################ More extensive test (#262)

  matched <- fake_matched(
    id_loan = c("L1", "L2", "L3"),
    loan_size_outstanding = c(1000, 15000, 1200),
    name_ald = c("a", "b", "a"),
    id_2dii = c("DL1", "DL2", "DL1"),
    sector = "power",
    sector_ald = "power"
  )

  ald <- fake_ald(
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
    ald,
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

test_that("w/ some region missing some scenario outputs expected `production`
          values (#203)", {
  scenario <- fake_scenario(
    scenario = c("cps", "sds", "sds"),
    region = c("global", "global", "non opec"),
    scenario_source = "weo_2019"
  )

  out <- target_market_share(fake_matched(), fake_ald(), scenario)

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
      fake_ald(),
      fake_scenario()
    )
  )

  expect_equal(nrow(out), 0L)

  good_names <- names(
    target_market_share(
      fake_matched(),
      fake_ald(),
      fake_scenario(),
      region_isos_stable
    )
  )

  expect_equal(setdiff(names(out), good_names), character(0))
})

test_that("w/ technology in ald but not loanbook, outputs all techs (#235)", {
  matched <- fake_matched(
    name_ald = "company a"
  )

  ald <- fake_ald(
    name_company = c("company a", "company b"),
    technology = c("ice", "electric"),
    production = c(1, 3)
  )

  scenario <- fake_scenario(
    technology = c("electric", "ice")
  )

  out <- target_market_share(
    matched,
    ald,
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
  ald <- fake_ald()
  scenario <- fake_scenario()

  out <- target_market_share(
    matched,
    ald,
    scenario,
    region_isos_stable,
    by_company = TRUE,
    weight_production = FALSE
  )

  projected <- filter(out, metric == "projected")

  expect_equal(projected$production, ald$production)
})

test_that("w/ multiple loans to same company, `technology_share` sums to one (#218)", {
  shares_sum_to_one <- function(data) {
    technology_share_sum <- data %>%
      group_by(sector, metric) %>%
      summarize(share_sum = sum(technology_share), .groups = "drop")

    all(technology_share_sum$share_sum == 1)
  }

  # multiple loans to same company
  out <- target_market_share(
    fake_matched(id_loan = c("L1", "L2")),
    fake_ald(),
    fake_scenario(),
    region_isos_stable
  )

  expect_true(shares_sum_to_one(out))
})

test_that("w/ multiple match `level`, unweighted production is equal to ALD production (#249)", {
  matched <- fake_matched(
    id_loan = c(1, 2),
    level = c("direct_loantaker", "ultimate_parent"),
  )

  out <- target_market_share(
    matched,
    fake_ald(),
    fake_scenario(),
    r2dii.data::region_isos_demo,
    by_company = TRUE,
    weight_production = FALSE
  )

  ald_production <- fake_ald() %>%
    dplyr::pull(production)

  out_production <- filter(out, metric == "projected") %>%
    dplyr::pull(production)

  expect_equal(ald_production, out_production)
})

test_that("for one company with multiple loans of different size, unweighted
          production by company equals ald-production (#239)", {
  different_loan_size <- 1:2
  matched <- fake_matched(
    id_loan = c("L1", "L2"),
    loan_size_credit_limit = different_loan_size,
    loan_size_outstanding = different_loan_size
  )

  projected <- target_market_share(
    matched,
    fake_ald(),
    scenario = fake_scenario(),
    region_isos = region_isos_stable,
    by_company = TRUE,
    weight_production = FALSE
  ) %>%
    filter(metric == "projected")

  expect_equal(projected$production, fake_ald()$production)
})

test_that("with bad column errors with informative message (#267)", {
  bad_matched <- fake_matched(
    bad_column = "bad"
  )

  expect_error(
    class = "invalid_columns",
    target_market_share(
      bad_matched,
      fake_ald(),
      fake_scenario()
    )
  )
})

test_that("`technology_share` outputs consistently when multiple
          direct_loantakers match to a single company (#265)", {
  matched <- fake_matched(
    id_loan = c("L1", "L2", "L3", "L4", "L5"),
    name_ald = c(rep("company a", 4), "company b")
  )

  matched_split_dl <- matched %>%
    mutate(name_direct_loantaker = c("company a1", "company a2", "company a3", "company a4", "company b"))

  ald <- fake_ald(
    name_company = rep(c("company a", "company b"), each = 2),
    technology = rep(c("ice", "electric"), 2),
    production = c(8, 2, 15, 5)
  )

  scenario <- fake_scenario(
    technology = c("ice", "electric")
  )

  out <- target_market_share(
    matched,
    ald,
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
    ald,
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
    name_ald = c("company a", "company a", "company b")
  )

  matched_diff_level <- matched_same_level %>%
    mutate(
      level = c("ultimate_parent", "direct_loantaker", "ultimate_parent")
    )

  ald <- fake_ald(
    name_company = rep(c("company a", "company b"), each = 2),
    technology = rep(c("ice", "electric"), 2),
    production = c(8, 2, 15, 5)
  )

  scenario <- fake_scenario(
    technology = c("ice", "electric")
  )

  out_same_level <- target_market_share(
    matched_same_level,
    ald,
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
    ald,
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
