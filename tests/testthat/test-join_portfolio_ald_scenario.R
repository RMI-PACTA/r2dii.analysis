library(r2dii.dataraw)

test_that("w/ loanbook, ald or scenario with missing names errors gracefully", {
  invalid <- function(data, x) dplyr::rename(data, bad = x)

  expect_error_class_missing_names <- function(match_result = NULL,
                                               ald = NULL,
                                               scenario = NULL) {
    expect_error(
      class = "missing_names",
      join_portfolio_ald_scenario(match_result %||% fake_matched(),
                                  ald %||% fake_ald(),
                                  scenario %||% fake_scenario())
    )
  }

  expect_error_class_missing_names(invalid(fake_matched(), "name_ald"))
  expect_error_class_missing_names(invalid(fake_matched(), "sector_ald"))

  expect_error_class_missing_names(invalid(fake_ald(), "name_company"))
  expect_error_class_missing_names(invalid(fake_ald(), "sector"))
  expect_error_class_missing_names(invalid(fake_ald(), "technology"))
  expect_error_class_missing_names(invalid(fake_ald(), "year"))

  expect_error_class_missing_names(invalid(fake_scenario(), "sector"))
  expect_error_class_missing_names(invalid(fake_scenario(), "technology"))
  expect_error_class_missing_names(invalid(fake_scenario(), "year"))

})
