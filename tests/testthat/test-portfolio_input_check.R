test_that("multiplication works", {
  portfolio <- read_raw_portfolio("TEST")
  out_clean_colnames <- clean_colnames(portfolio)
  expect_is(out_clean_colnames, "data.frame")

  # FIXME:
  # "Investor.Name" becomes "InvestorName" but "Portfolio.Name" stays the same
  # Is this expected? (ASK @CLARE2D)
  expect_false(
    identical(
      names(portfolio),
      names(out_clean_colnames)
    )
  )


})
