library(r2dii.dataraw)

test_that("with data lacking crucial columns errors with informative message", {
  expect_error_missing_names <- function(name, use_credit_limit = FALSE) {
    data <- dplyr::rename(fake_master(), bad = name)

    expect_error(
      class = "missing_names",
      add_weighted_loan_production(data, use_credit_limit = use_credit_limit)
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

test_that("with bad but unused loan_size_column is error free", {
  bad_unused <- dplyr::rename(fake_master(), bad = "loan_size_credit_limit")
  expect_error_free(
    add_weighted_loan_production(bad_unused, use_credit_limit = FALSE)
  )

  bad_unused <- dplyr::rename(fake_master(), bad = "loan_size_outstanding")
  expect_error_free(
    add_weighted_loan_production(bad_unused, use_credit_limit = TRUE)
  )
})

test_that("with grouped data returns same groups as input", {
  out <- fake_master() %>%
    dplyr::group_by(.data$sector) %>%
    add_weighted_loan_production()

  expect_equal(dplyr::group_vars(out), "sector")
})

test_that("with known input outputs as expected", {
  # styler: off
  data <- fake_master(
    technology =            c("ta", "ta", "tb", "tb"),
    id_loan    =            c("i1", "i2", "i1", "i2"),
    loan_size_outstanding = c( 40,   10,   40,   10),
    production            = c( 10,   30,   20,   40),
  )
  out1 <- add_weighted_loan_production(data)
  out1$weighted_loan_production
  expect_equal(out1$weighted_loan_production, c(8, 6, 16, 8))

  # Is sensitive to `use_credit_limit`
  # Reversing loan_size and production outputs reverse result
  data2 <- fake_master(
    technology =             c("ta", "ta", "tb", "tb"),
    id_loan    =             c("i1", "i2", "i1", "i2"),
    loan_size_credit_limit = c( 10,   40,   10,   40),
    production            =  c( 40,   20,   30,   10),
  )
  out2 <- add_weighted_loan_production(data2, use_credit_limit = TRUE)
  expect_equal(out2$weighted_loan_production, c(8, 16, 6, 8))
  # styler: on
})

test_that("with duplicated loan_size by id_loan throws erro", {
  expect_error(
    class = "multiple_loan_size_values_by_id_loan",
    add_weighted_loan_production(fake_master(loan_size_outstanding = 1:2))
  )

  expect_error(
    class = "multiple_loan_size_values_by_id_loan",
    add_weighted_loan_production(
      fake_master(loan_size_credit_limit = 1:2), use_credit_limit = TRUE)
  )
})
