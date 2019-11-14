test_that("read_raw_portfolio outputs a data.frame", {
  skip_if_not(r2dii.utils::dropbox_exists())

  out <- read_raw_portfolio("TEST")
  expect_is(out, "data.frame")
  expect_true(ncol(out) > 1)
})
