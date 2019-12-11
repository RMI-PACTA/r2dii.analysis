test_that("sda_calculation with sample_<data>` returns a tibble", {
  expect_is(sda_calculation(sample_market, sample_portfolio), "tbl")
})

test_that("sda_calculation with `market` and `portfolio` returns a tibble", {
  expect_is(sda_calculation(market, portfolio), "tbl")
})
