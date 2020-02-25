library(r2dii.dataraw)

# master <- r2dii.dataraw::loanbook_demo %>%
#   r2dii.match::match_name(r2dii.dataraw::ald_demo) %>%
#   r2dii.match::prioritize() %>%
#   join_ald_scenario(r2dii.dataraw::ald_demo, r2dii.dataraw::scenario_demo) %>%
#   dplyr::slice(1) %>%
#   dplyr::select(
#     c(
#       "sector",
#       "id_loan",
#       "loan_size_outstanding",
#       "loan_size_credit_limit",
#       "production",
#       "year",
#       "technology"
#     )
#   )
# master$loan_size_outstanding <- 1
# master$loan_size_credit_limit <- 2
# master$production <- 1
master <- tibble::tibble(
  sector = "automotive",
  id_loan = "L151",
  loan_size_outstanding = 1,
  loan_size_credit_limit = 2,
  production = 1,
  year = 2020,
  technology = "ice"
)

test_that("with data lacking crucial columns errors with informative message", {
  # FIXEM: Check more crucial names
  bad <- function(data, x) dplyr::rename(data, bad = x)

  expect_error(
    class = "missing_names",
    add_loan_weighted_production(bad(master, "sector"))
  )

  expect_error(
    class = "missing_names",
    add_loan_weighted_production(bad(master, "loan_size_outstanding"))
  )

  expect_error(
    add_loan_weighted_production(
      bad(master, "loan_size_credit_limit"),
      use_loan_size_credit_limit = TRUE
    )
  )
})

test_that("add_loan_weighted_production outputs a dataframe", {
  expect_is(add_loan_weighted_production(master), "tbl_df")
})

test_that("with grouped data returns same groups as input", {
  out <- master %>%
    dplyr::group_by(.data$sector) %>%
    add_loan_weighted_production()

  expect_equal(dplyr::group_vars(out), "sector")
})

test_that("FIXME: test is sensitive. defaults to using `loan_size_outstanding`", {

  expect_error_free(
    add_loan_weighted_production(master)
  )
  expect_error_free(
    add_loan_weighted_production(master, use_loan_size_credit_limit = TRUE)
  )
})

test_that("outputs new column `loan_weighted_production`", {
  expect_true(
    has_name(add_loan_weighted_production(master), "loan_weighted_production")
  )
})
