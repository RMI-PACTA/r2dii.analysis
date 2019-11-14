test_that("read_raw_portfolio_file outputs a data.frame", {
  expect_is(read_raw_portfolio_file("TEST"), "data.frame")
})
