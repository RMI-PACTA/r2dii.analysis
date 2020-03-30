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

  expect_known_value(out, "ref-add_company_target", update = FALSE)
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
      class = "column_has_na",
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
