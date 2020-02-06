library(dplyr)

test_that("calculate_temperature_indicator and friends yield known values", {
  out_calculate_temperature_indicator <- calculate_temperature_indicator(
    input_results = r2dii.analysis::sample_results,
    upper_temp_threshold = 10,
    lower_temp_threshold = 1.5,
    start_year = 2019,
    time_horizon = 5,
    allocation = "PortfolioWeight",
    group_vars = c("Investor.Name", "Portfolio.Name", "Asset.Type"),
  )

  expect_known_value(
    out_calculate_temperature_indicator,
    "ref-calculate_temperature_indicator",
    update = FALSE
  )



  out_influencemap_weighting_methodology <- apply_influencemap_portfolio_weighting(
    input_results = out_calculate_temperature_indicator,
    input_audit = r2dii.analysis::sample_audit,
    metric_name = "temperature",
    group_vars = c("Investor.Name", "Portfolio.Name")
  )

  expect_known_value(
    out_influencemap_weighting_methodology,
    "ref-influencemap_weighting_methodology",
    update = FALSE
  )



  out_map_sector_exposure <- map_sector_exposure(
    input_audit = r2dii.analysis::sample_audit
  )

  expect_known_value(
    out_map_sector_exposure,
    "ref-map_sector_exposure",
    update = FALSE
  )

  out_influencemap_weighting_methodology2 <-
    out_influencemap_weighting_methodology  %>%
    distinct(Investor.Name, Portfolio.Name, Allocation, temperature_metric_port)

  joint <- inner_join(
    out_influencemap_weighting_methodology2, out_map_sector_exposure,
    by = c("Investor.Name", "Portfolio.Name")
  )

  out_find_range <- find_range(
    joint,
    range = c(1.75, 2, 2.75, 3.5),
    metric_name = "temperature_metric_port"
    )

  expect_known_value(
    out_find_range,
    "ref-out_find_range",
    update = TRUE
  )
})


