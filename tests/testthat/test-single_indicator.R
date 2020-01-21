library(dplyr)

test_that("single_indicator and friends yield known values", {
  out_single_indicator <- single_indicator(
    input_results = r2dii.analysis::sample_results,
    upper_temp_threshold = 10,
    lower_temp_threshold = 1.5,
    start_year = 2019,
    time_horizon = 5,
    allocation = "PortfolioWeight",
    group_vars = c("Investor.Name", "Portfolio.Name", "Asset.Type"),
    scenario_relationships = r2dii.analysis::scenario_relationships
  )

  expect_known_value(
    out_single_indicator,
    "ref-single_indicator",
    update = FALSE
  )



  out_influencemap_weighting_methodology <- influencemap_weighting_methodology(
    input_results = out_single_indicator,
    input_audit = r2dii.analysis::sample_audit,
    metric_name = "temperature",
    group_vars = c("Investor.Name", "Portfolio.Name"),
    sector_weightings = r2dii.analysis::tech_sector_weighting
  )

  expect_known_value(
    out_influencemap_weighting_methodology,
    "ref-influencemap_weighting_methodology",
    update = FALSE
  )



  out_mapped_sector_exposure <- mapped_sector_exposure(
    input_audit = r2dii.analysis::sample_audit
  )

  expect_known_value(
    out_mapped_sector_exposure,
    "ref-mapped_sector_exposure",
    update = FALSE
  )



  out_influencemap_weighting_methodology2 <-
    out_influencemap_weighting_methodology  %>%
    distinct(Investor.Name, Portfolio.Name, Allocation, temperature)

  joint <- inner_join(
    out_influencemap_weighting_methodology2, out_mapped_sector_exposure,
    by = c("Investor.Name", "Portfolio.Name")
  )

  out_find_range <- find_range(joint, range = c(1.75, 2, 2.75, 3.5))

  expect_known_value(
    out_find_range,
    "ref-out_find_range",
    update = FALSE
  )
})


