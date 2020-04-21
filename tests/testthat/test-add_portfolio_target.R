test_that("with bad `data` errors with informative message", {
  expect_error(add_portfolio_target("bad"), "data.frame.*not.*TRUE")
})

test_that("outputs a tibble", {
  out <- summarize_portfolio_production(fake_master()) %>%
    add_portfolio_target()
  expect_is(out, "tbl_df")
})

test_that("with fake data outputs known value", {
  out <- summarize_portfolio_production(fake_master()) %>%
    add_portfolio_target()

  expect_known_value(out, "ref-add_portfolio_target", update = T)
})

test_that("with data lacking crucial columns errors with informative message", {
  expect_error_missing_names <- function(name) {
    data <- dplyr::rename(
      summarize_portfolio_production(fake_master()), bad = name
    )

    expect_error(
      class = "missing_names",
      add_portfolio_target(data)
    )
  }

  expect_error_missing_names("weighted_production")
  expect_error_missing_names("sector")
  expect_error_missing_names("scenario")
  expect_error_missing_names("year")
  expect_error_missing_names("tmsr")
  expect_error_missing_names("smsp")
})

test_that("with NAs in crucial columns errors with informative message", {
  expect_error_crucial_NAs <- function(name) {
    data <- fake_master(
      technology = c("ta", "ta", "tb", "tb"),
      id_loan = c("i1", "i2", "i1", "i2"),
      loan_size_outstanding = c(40, 10, 40, 10),
      production = c(10, 30, 20, 40),
    ) %>%
      summarize_portfolio_production()

    data[1, name] <- NA
    expect_error(
      class = "some_value_is_missing",
      add_portfolio_target(data)
    )
  }

  expect_error_crucial_NAs("weighted_production")
  expect_error_crucial_NAs("sector")
  expect_error_crucial_NAs("scenario")
  expect_error_crucial_NAs("year")
  expect_error_crucial_NAs("tmsr")
  expect_error_crucial_NAs("smsp")
})

test_that("outputs expected names", {
  out <- fake_master() %>%
    summarize_portfolio_production() %>%
    add_portfolio_target()

  expect_named(
    out,
    c(
      "sector",
      "technology",
      "year",
      "scenario",
      "weighted_production",
      "tmsr_target_weighted_production",
      "smsp_target_weighted_production"
    )
  )
})

test_that("with grouped data returns same groups as input", {
  out <- fake_master() %>%
    summarize_company_production() %>%
    dplyr::group_by(.data$sector) %>%
    add_company_target()

  expect_equal(dplyr::group_vars(out), "sector")
})

test_that("with known input outputs as expected", {
  # styler: off
  data <- fake_master(
    technology = c("ta", "tb", "ta", "tb"),
    year = c(2020, 2020, 2021, 2021),
    name_ald = c("comp1", "comp1", "comp1", "comp1"),
    scenario = "sds",
    tmsr = c(1, 1, 1.85, 0.6),
    smsp = c(0, 0, 0.34, -0.2),
    weighted_production = c(200, 250, 240, 240)
  )
  out1 <- add_portfolio_target(data)

  expect_equal(out1$tmsr_target_weighted_production, c(200, 250, 370, 150))
  expect_equal(out1$smsp_target_weighted_production, c(200, 250, 353, 160))
})
