test_that("sda_calculation() with `market` and `portfolio` throws no error", {
  sda_calculation(
    market = market,
    port = portfolio,
    ref_sectors = c("Cement", "Steel"),
    ref_scenario = "B2DS",
    start_year = 2019,
    target_year = 2040
  )
})
