# Variable names as per commit sha 8923fe0 (2dii-demo_7-1_flag)
# input_results <- r2dii.analysis::sample_results
# input_audit <- r2dii.analysis::sample_audit
# scenario_relationships <- r2dii.analysis::scenario_relationships
# sector_weightings <- r2dii.analysis::tech_sector_weighting

test_that("single_indicator with provided example throws no error", {
  expect_error(
    single_indicator(
      input_results = r2dii.analysis::sample_results,
      upper_temp_threshold = 10,
      lower_temp_threshold = 1.5,
      start_year = 2019,
      time_horizon = 5,
      allocation = "PortfolioWeight",
      group_vars = c("Investor.Name", "Portfolio.Name", "Asset.Type")
    ),
    NA
  )
})
