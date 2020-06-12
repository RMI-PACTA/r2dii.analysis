test_that("with bad `data` errors with informative message", {
  expect_error(target_market_share_company("bad"), "data.frame.*not.*TRUE")
})

test_that("outputs a tibble", {
  out <- summarize_company_production(fake_master()) %>%
    target_market_share_company()
  expect_is(out, "tbl_df")
})

test_that("with fake data outputs known value", {
  out <- summarize_company_production(fake_master()) %>%
    target_market_share_company()

  expect_known_value(out, "ref-target_market_share_company", update = FALSE)
})

test_that("with data lacking crucial columns errors with informative message", {
  expect_error_missing_names <- function(name) {
    data <- summarize_company_production(fake_master()) %>%
      rename(bad = name)

    expect_error(
      class = "missing_names",
      target_market_share_company(data)
    )
  }

  expect_error_missing_names("weighted_production")
  expect_error_missing_names("name_ald")
  expect_error_missing_names("sector")
  expect_error_missing_names("scenario")
  expect_error_missing_names("year")
  expect_error_missing_names("tmsr")
  expect_error_missing_names("smsp")
})

test_that("with NAs in crucial columns errors with informative message", {
  data <- fake_master(
    technology = c("ta", "ta", "tb", "tb"),
    id_loan = c("i1", "i2", "i1", "i2"),
    loan_size_outstanding = c(40, 10, 40, 10),
    production = c(10, 30, 20, 40),
  ) %>%
    summarize_company_production()

  data[1, "smsp"] <- NA
  path <- test_path(
    "output", "target_market_share_company-some_value_is_missing.txt"
  )
  verify_output(path, target_market_share_company(data))
})

test_that("with NAs in crucial columns throws error of expected class", {
  expect_error_crucial_NAs <- function(name, data = this_data) {
    data <- fake_master(
      technology = c("ta", "ta", "tb", "tb"),
      id_loan = c("i1", "i2", "i1", "i2"),
      loan_size_outstanding = c(40, 10, 40, 10),
      production = c(10, 30, 20, 40),
    ) %>%
      summarize_company_production()

    data[1, name] <- NA

    expect_error(
      class = "some_value_is_missing",
      target_market_share_company(data)
    )
  }

  expect_error_crucial_NAs("weighted_production")
  expect_error_crucial_NAs("name_ald")
  expect_error_crucial_NAs("sector")
  expect_error_crucial_NAs("scenario")
  expect_error_crucial_NAs("year")
  expect_error_crucial_NAs("tmsr")
  expect_error_crucial_NAs("smsp")
})

test_that("outputs expected names", {
  out <- fake_master() %>%
    summarize_company_production() %>%
    target_market_share_company()

  expect_named(
    out,
    c(
      "sector",
      "technology",
      "year",
      "name_ald",
      "region",
      "weighted_production_metric",
      "weighted_production_value"
    )
  )
})

test_that("with grouped data returns same groups as input", {
  out <- fake_master() %>%
    summarize_company_production() %>%
    group_by(.data$sector) %>%
    target_market_share_company()

  expect_equal(dplyr::group_vars(out), "sector")
})

test_that("with known input outputs as expected", {
  # styler: off
  data <- fake_master(
    technology = c("electric", "ice", "electric", "ice", "electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2021, 2021, 2020, 2020, 2021, 2021),
    name_ald = paste0("comp", c(rep(1, 4), rep(2, 4))),
    scenario = "sds",
    tmsr = c(1, 1, 1.85, 0.6, 1, 1, 1.85, 0.6),
    smsp = c(0, 0, 0.34, -0.2, 0, 0, 0.34, -0.2),
    weighted_production = c(20, 60, 40, 40, 180, 190, 200, 200)
  )
  out <- target_market_share_company(data)
  out_target <- out %>%
    filter(weighted_production_metric == "target_sds")

  expect_equal(
    out_target$weighted_production_value,
    c(20, 60, 47.2, 36, 180, 190, 305.8, 114)
  )

})

test_that("portfolio values and targets have identical values at start year (#87)", {
  data <- fake_master(
    name_ald = "comp",
    technology = c("electric", "ice", "electric", "ice"),
    year = c(2020, 2020, 2020, 2020),
    region = c("global", "global", "europe", "europe"),
    scenario = "sds",
    tmsr = 1,
    smsp = 0,
    weighted_production = c(200, 250, 100, 150)
  )
  out <- target_market_share_company(data) %>%
    filter(year == min(year)) %>%
    group_by(sector, technology, region, name_ald) %>%
    summarize(distinct_intial_values = dplyr::n_distinct(weighted_production_value)) %>%
    mutate(initial_values_are_equal = (.data$distinct_intial_values == 1))

  expect_true(all(out$initial_values_are_equal))

})
