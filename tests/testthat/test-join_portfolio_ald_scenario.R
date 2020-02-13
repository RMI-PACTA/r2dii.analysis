library(r2dii.dataraw)
library(r2dii.match)

test_that("w/ loanbook, ald or scenario with missing names errors gracefully", {
  bad <- function(data, x) dplyr::rename(data, bad = x)

  expect_error_missing_names <- function(match_result = fake_matched(),
                                         ald = fake_ald(),
                                         scenario = fake_scenario()) {
    expect_error(
      class = "missing_names",
      join_portfolio_ald_scenario(match_result, ald, scenario)
    )
  }

  expect_error_missing_names(match_result = bad(fake_matched(), "name_ald"))
  expect_error_missing_names(match_result = bad(fake_matched(), "sector_ald"))

  expect_error_missing_names(ald = bad(fake_ald(), "name_company"))
  expect_error_missing_names(ald = bad(fake_ald(), "sector"))
  expect_error_missing_names(ald = bad(fake_ald(), "technology"))
  expect_error_missing_names(ald = bad(fake_ald(), "year"))

  expect_error_missing_names(scenario = bad(fake_scenario(), "sector"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "technology"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "year"))
})

test_that("with fake data outputs known value", {
  out <- join_portfolio_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario()
  )

  expect_known_value(out, "ref-join_portfolio_ald_scenario", update = FALSE)
})

test_that("outputs expected names", {
  minimum_names <- c(
    "id_loan",
    "id_2dii",
    "level",
    "sector",
    "technology",
    "year",
    "name_ald"
  )

  expect_named(
    expected = minimum_names,
    join_portfolio_ald_scenario(
      fake_matched(),
      ald = fake_ald(),
      scenario = fake_scenario()
    )
  )

  expect_named(
    expected = minimum_names,
    join_portfolio_ald_scenario(
      fake_matched(unknown_column = "any"),
      ald = fake_ald(),
      scenario = fake_scenario()
    )
  )

  expect_named(
    expected = c(minimum_names, "production"),
    join_portfolio_ald_scenario(
      fake_matched(production = "any"),
      ald = fake_ald(),
      scenario = fake_scenario()
    )
  )
})
