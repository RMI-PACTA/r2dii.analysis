test_that("multiplication works", {
  out_portfolio <- read_raw_portfolio("TEST")

  out_clean_column_names <- clean_column_names(out_portfolio)
  expect_is(out_clean_column_names, "data.frame")

  # FIXME:
  # "Investor.Name" becomes "InvestorName" but "Portfolio.Name" stays the same
  # Is this expected? (ASK @CLARE2D)
  expect_false(
    identical(
      names(out_portfolio),
      names(out_clean_column_names)
    )
  )

  expected_names <- c(
      "investor_name",
      "portfolio_name",
      "isin",
      "number_of_shares",
      "market_value",
      "currency"
    )
  expect_named(out_clean_column_names, expected_names)
})
