test_that("with bad `data` errors with informative message", {
  expect_error(add_company_target("bad"), "data.frame.*not.*TRUE")
})

test_that("outputs a tibble", {
  out <- summarize_company_production(fake_master()) %>%
    add_company_target()
  expect_is(out, "tbl_df")
})

test_that("with fake data outputs known value", {
  out <- summarize_company_production(fake_master()) %>%
    add_company_target()

  expect_known_value(out, "ref-add_company_target", update = TRUE)
})

test_that("with data lacking crucial columns errors with informative message", {
  expect_error_missing_names <- function(name) {
    data <- summarize_company_production(fake_master()) %>%
      dplyr::rename(bad = name)

    expect_error(
      class = "missing_names",
      add_company_target(data)
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
  expect_error_crucial_NAs <- function(name) {
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
      add_company_target(data)
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
    add_company_target()

  expect_named(
    out,
    c(
      "sector", "technology", "year", "name_ald", "scenario",
      "weighted_production", "tmsr_target_weighted_production",
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
    technology = c("ta", "tb", "ta", "tb", "ta", "tb", "ta", "tb"),
    year = c(2020, 2020, 2021, 2021, 2020, 2020, 2021, 2021),
    name_ald = c("comp1", "comp1", "comp1", "comp1", "comp2", "comp2", "comp2", "comp2"),
    scenario = "sds",
    tmsr = c(1, 1, 1.85, 0.6, 1, 1, 1.85, 0.6),
    smsp = c(0, 0, 0.34, -0.2, 0, 0, 0.34, -0.2),
    weighted_production = c(20, 60, 40, 40, 180, 190, 200, 200)
  )
  out1 <- add_company_target(data)

  expect_equal(out1$tmsr_target_weighted_production, c(20, 60, 37, 36, 180, 190, 333, 114))
  expect_equal(out1$smsp_target_weighted_production, c(20, 60, 47.2, 44, 180, 190, 305.8, 116))

})
