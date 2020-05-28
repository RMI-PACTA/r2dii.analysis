library(r2dii.data)
library(r2dii.match)

test_that("w/ loanbook, ald or scenario with missing names errors gracefully", {
  bad <- function(data, x) dplyr::rename(data, bad = x)

  expect_error_missing_names <- function(match_result = fake_matched(),
                                         ald = fake_ald(),
                                         scenario = fake_scenario()) {
    expect_error(
      class = "missing_names",
      join_ald_scenario(match_result, ald, scenario)
    )
  }

  expect_error_missing_names(match_result = bad(fake_matched(), "name_ald"))
  expect_error_missing_names(match_result = bad(fake_matched(), "sector_ald"))

  expect_error_missing_names(ald = bad(fake_ald(), "name_company"))
  expect_error_missing_names(ald = bad(fake_ald(), "sector"))
  expect_error_missing_names(ald = bad(fake_ald(), "technology"))
  expect_error_missing_names(ald = bad(fake_ald(), "year"))

  expect_error_missing_names(scenario = bad(fake_scenario(), "sector"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "technology"))
  expect_error_missing_names(scenario = bad(fake_scenario(), "year"))

  expect_error_missing_names(scenario = bad(fake_scenario(), "scenario_source"))
})

test_that("is sensitive to region_isos", {
  out1 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario(scenario_source = "weo_2019"),
    region_isos = r2dii.data::region_isos
  )

  out2 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario(scenario_source = "demo_2020"),
    region_isos = r2dii.data::region_isos_demo
  )

  expect_false(identical(out1, out2))
})

test_that("with fake data outputs known value", {
  out <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario(),
    region_isos = r2dii.data::region_isos_demo
  )

  expect_known_value(out, "ref-join_ald_scenario", update = FALSE)
})

test_that("outputs expected names", {
  expected <- setdiff(
    c(
      names(fake_matched()),
      names(fake_ald()),
      names(fake_scenario()),
      # Comes from region_isos which is used internally
      "scenario_source"
    ),
    "name_company"
  )

  out <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario(),
    region_isos = r2dii.data::region_isos_demo
  )
  expect_equal(
    sort(unique(names(out))),
    sort(unique(expected))
  )

  out2 <- join_ald_scenario(
    fake_matched(new_column = "anything"),
    ald = fake_ald(),
    scenario = fake_scenario(),
    region_isos = r2dii.data::region_isos_demo
  )
  expect_equal(
    sort(unique(c(names(out2)), "new_column")),
    sort(unique(c(expected, "new_column")))
  )
})

test_that("include/excludes `plant_location`s inside/outside a region", {
  outside <- "us"
  europe <- c("de", "fr")
  china <- c("cn")
  inside <- c(europe, china)
  these_locations <- c(outside, inside)

  this_scenario <- dplyr::bind_rows(
    fake_scenario(region = "oecd_europe"),
    fake_scenario(region = "china")
  )

  region_isos_toy <- tibble::tribble(
    ~region,       ~isos, ~source,
    "oecd_europe", "de",  "demo_2020",
    "oecd_europe", "fr",  "demo_2020",
    "china",       "cn",  "demo_2020",
  )

  out <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(plant_location = these_locations),
    scenario = this_scenario,
    region_isos = region_isos_toy
  )

  # Includes locations inside matching regions
  expect_true(all(unique(out$plant_location) %in% inside))

  # Excludes locations outside matching regions
  expect_false(any(unique(out$plant_location) %in% outside))
})

test_that("case insensitive to input `plant_location`", {
  out1 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(plant_location = c("de")),
    scenario = fake_scenario(region = "oecd_europe"),
    region_isos = r2dii.data::region_isos_demo
  )

  out2 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(plant_location = c("DE")),
    scenario = fake_scenario(region = "oecd_europe"),
    region_isos = r2dii.data::region_isos_demo
  )

  expect_equal(out1, out2)
})

test_that("outputs a number of rows equal to matches by `scenario_source`", {
  matching_0 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario(scenario_source = "weo_2019"),
    region_isos = r2dii.data::region_isos_demo
  )
  expect_equal(nrow(matching_0), 0L)

  matching_1 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario(scenario_source = c("weo_2019", "demo_2020")),
    region_isos = r2dii.data::region_isos_demo
  )

  expect_equal(nrow(matching_1), 1L)

  matching_1 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario(scenario_source = "weo_2019"),
    region_isos = r2dii.data::region_isos
  )

  expect_equal(nrow(matching_1), 1L)

  matching_2 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario(scenario_source = rep("demo_2020", 2L)),
    region_isos = r2dii.data::region_isos_demo
  )
  expect_equal(nrow(matching_2), 2L)
})

test_that("without `sector` throws no error", {
  # 2DegreesInvesting/r2dii.analysis/pull/62#issuecomment-634651157
  without_sector <- dplyr::select(fake_matched(), -sector)
  expect_error_free(
    join_ald_scenario(
      without_sector,
      ald = fake_ald(),
      scenario = fake_scenario(),
      region_isos = r2dii.data::region_isos_demo
    )
  )
})

test_that("with 0-row output throws a warning", {
  expect_warning(
    class = "0-row-output",
    join_ald_scenario(
      fake_matched(),
      ald = fake_ald(),
      scenario = fake_scenario(scenario_source = "weo_2019"),
      region_isos = r2dii.data::region_isos_demo
    )
  )
})
