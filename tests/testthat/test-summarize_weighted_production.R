library(r2dii.data)

test_that("with bad `data` errors with informative message", {
  expect_error(summarize_weighted_production("bad"), "data.frame.*not.*TRUE")
})

test_that("with bad use_credit_limit errors with informative message", {
  expect_error(
    summarize_weighted_production(fake_master(), use_credit_limit = "bad"),
    "not TRUE"
  )

  expect_error(
    summarize_weighted_production(fake_master(), use_credit_limit = 1),
    "not TRUE"
  )

  expect_error(
    summarize_weighted_production(fake_master(), use_credit_limit = NA),
    "not TRUE"
  )

  expect_error(
    summarize_weighted_production(fake_master(), use_credit_limit = NULL),
    "not TRUE"
  )
})

test_that("with data lacking crucial columns errors with informative message", {
  expect_error_missing_names <- function(name, use_credit_limit = FALSE) {
    data <- rename(fake_master(), bad = name)

    expect_error(
      class = "missing_names",
      summarize_weighted_production(data, use_credit_limit = use_credit_limit)
    )
  }

  expect_error_missing_names("id_loan")
  expect_error_missing_names("loan_size_outstanding")
  expect_error_missing_names("loan_size_credit_limit", use_credit_limit = TRUE)
  expect_error_missing_names("production")
  expect_error_missing_names("sector")
  expect_error_missing_names("technology")
  expect_error_missing_names("year")
})

test_that("with NAs in crucial columns errors with informative message", {
  expect_error_crucial_NAs <- function(name, use_credit_limit = FALSE) {
    data <- fake_master(
      technology = c("ta", "ta", "tb", "tb"),
      id_loan = c("i1", "i2", "i1", "i2"),
      loan_size_outstanding = c(40, 10, 40, 10),
      production = c(10, 30, 20, 40),
    )

    data[1, name] <- NA
    expect_error(
      class = "some_value_is_missing",
      summarize_weighted_production(data, use_credit_limit = use_credit_limit)
    )
  }

  expect_error_crucial_NAs("id_loan")
  expect_error_crucial_NAs("loan_size_outstanding")
  expect_error_crucial_NAs("loan_size_credit_limit", use_credit_limit = TRUE)
  expect_error_crucial_NAs("production")
  expect_error_crucial_NAs("sector")
  expect_error_crucial_NAs("technology")
  expect_error_crucial_NAs("year")
})

test_that("with bad but unused loan_size_column is error free", {
  bad_unused <- rename(fake_master(), bad = "loan_size_credit_limit")
  expect_error_free(
    summarize_weighted_production(bad_unused, use_credit_limit = FALSE)
  )

  bad_unused <- rename(fake_master(), bad = "loan_size_outstanding")
  expect_error_free(
    add_weighted_loan_production(bad_unused, use_credit_limit = TRUE)
  )
})

test_that("with duplicated loan_size by id_loan throws error", {
  expect_error(
    class = "multiple_loan_size_values_by_id_loan",
    summarize_weighted_production(fake_master(loan_size_outstanding = 1:2))
  )

  expect_error(
    class = "multiple_loan_size_values_by_id_loan",
    summarize_weighted_production(
      fake_master(loan_size_credit_limit = 1:2),
      use_credit_limit = TRUE
    )
  )
})

test_that("with known input outputs as expected", {
  # styler: off
  data <- fake_master(
    technology =            c("ta", "ta", "tb", "tb"),
    id_loan    =            c("i1", "i2", "i1", "i2"),
    loan_size_outstanding = c( 40,   10,   40,   10),
    production            = c( 10,   30,   20,   40),
  )
  out1 <- summarize_weighted_production(data)
  out1$weighted_production
  expect_equal(out1$weighted_production, c(14, 24))

  # Is sensitive to `use_credit_limit`
  # Reversing loan_size and production outputs reverse result
  data2 <- fake_master(
    technology =             c("ta", "ta", "tb", "tb"),
    id_loan    =             c("i1", "i2", "i1", "i2"),
    loan_size_credit_limit = c( 10,   40,   10,   40),
    production            =  c( 40,   20,   30,   10),
  )
  out2 <- summarize_weighted_production(data2, use_credit_limit = TRUE)
  expect_equal(out2$weighted_production, c(24, 14))
  # styler: on
})

test_that("outputs expected names", {
  expect_named(
    summarize_weighted_production(fake_master()),
    c("sector", "technology", "year", "weighted_production")
  )
})

test_that("with multiple years outputs as expected", {
  # styler: off
  data <- fake_master(
    technology =            c("ta", "ta"),
    id_loan    =            c("i1", "i2"),
    loan_size_outstanding = c(40,   10),
    production            = c(10,   30),
  )
  # styler: on

  data2 <- dplyr::bind_rows(data, data)
  data2$year <- c(2020, 2020, 2021, 2021)

  expect_equal(
    summarize_weighted_production(data2)$year,
    c(2020, 2021)
  )
  expect_equal(
    summarize_weighted_production(data2)$weighted_production,
    c(14, 14)
  )
})

test_that("with grouped data returns same groups as input", {
  out <- fake_master() %>%
    group_by(.data$sector) %>%
    summarize_weighted_production()

  expect_equal(dplyr::group_vars(out), "sector")
})

test_that("can group by `plant_location`", {
  data <- fake_master(plant_location = c("a", "b"))
  out <- summarize_weighted_production(data, plant_location)
  expect_equal(nrow(out), 2L)
})

test_that("preserves groups passed to ...", {
  data <- fake_master(plant_location = c("a", "b")) %>%
    group_by(plant_location)

  out <- summarize_weighted_production(data, plant_location)
  expect_equal(nrow(out), 2L)

  expect_equal(dplyr::group_vars(out), "plant_location")
})
