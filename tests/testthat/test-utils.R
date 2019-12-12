test_that("get_current_year returns the current year", {
  expect_equal(
    get_current_year(),
    lubridate::year(Sys.time())
  )
})
