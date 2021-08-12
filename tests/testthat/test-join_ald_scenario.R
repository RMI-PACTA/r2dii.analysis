library(dplyr)
library(r2dii.data)

test_that("with fake data outputs known value", {
  out <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario(),
    region_isos = region_isos_stable
  )

  expect_known_value(out, "ref-join_ald_scenario", update = FALSE)
})

test_that("returns visibly", {
  expect_visible(
    join_ald_scenario(
      fake_matched(),
      ald = fake_ald(),
      scenario = fake_scenario(),
      region_isos = region_isos_stable
    )
  )
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
    region_isos = region_isos_stable
  )
  expect_equal(
    sort(unique(names(out))),
    sort(unique(expected))
  )

  out2 <- join_ald_scenario(
    fake_matched(new_column = "anything"),
    ald = fake_ald(),
    scenario = fake_scenario(),
    region_isos = region_isos_stable
  )
  expect_equal(
    sort(unique(c(names(out2)), "new_column")),
    sort(unique(c(expected, "new_column")))
  )
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
    region_isos = region_isos_stable
  )

  expect_false(identical(out1, out2))
})

test_that("is case-insensitive to `plant_location` inputs", {
  lowercase <- "a"
  out1 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(plant_location = lowercase),
    scenario = fake_scenario(region = "b", scenario_source = "c"),
    region_isos = tibble(isos = "a", region = "b", source = "c")
  )

  uppercase <- "A"
  out2 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(plant_location = uppercase),
    scenario = fake_scenario(region = "b", scenario_source = "c"),
    region_isos = tibble(isos = "a", region = "b", source = "c")
  )

  expect_equal(out1, out2)
})

test_that("outputs a number of rows equal to matches by `scenario_source`", {
  matching_0 <- suppressWarnings(
    join_ald_scenario(
      fake_matched(),
      ald = fake_ald(plant_location = "a"),
      scenario = fake_scenario(region = "b", scenario_source = "c"),
      region_isos = tibble(isos = "a", region = "b", source = "-")
    )
  )
  expect_equal(nrow(matching_0), 0L)

  matching_1 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(plant_location = "a"),
    scenario = fake_scenario(region = "b", scenario_source = "c"),
    region_isos = tibble(isos = "a", region = "b", source = "c")
  )
  expect_equal(nrow(matching_1), 1L)

  matching_2 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(plant_location = "a"),
    scenario = fake_scenario(region = "b", scenario_source = c("c", "c")),
    region_isos = tibble(isos = "a", region = "b", source = "c")
  )
  expect_equal(nrow(matching_2), 2L)
})

test_that("w/ loanbook, ald or scenario with missing names errors gracefully", {
  bad <- function(data, x) rename(data, bad = x)

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

test_that("without `sector` throws no error", {
  # 2DegreesInvesting/r2dii.analysis/pull/62#issuecomment-634651157
  without_sector <- select(fake_matched(), -sector)
  expect_error_free(
    join_ald_scenario(
      without_sector,
      ald = fake_ald(),
      scenario = fake_scenario(),
      region_isos = region_isos_stable
    )
  )
})

test_that("warns 0-rows caused by scenario or region_isos", {
  join_ald_scenario2 <- function(l, scenario = NULL, region_isos = NULL) {
    scenario <- scenario %||% fake_scenario(
      region = l$region, sector = l$sector, scenario_source = l$source
    )
    region_isos <- region_isos %||% tibble(
      region = l$region, isos = l$isos, source = l$source
    )

    join_ald_scenario(
      fake_matched(sector_ald = l$sector),
      ald = fake_ald(plant_location = l$isos, sector = l$sector),
      scenario = scenario,
      region_isos = region_isos
    )
  }

  l <- list(sector = "a", region = "b", isos = "c", source = "d")

  expect_warning(join_ald_scenario2(l), NA)

  # testthat < 2.99.0.9000 seems to lack the `class` argument to expect_warning
  # This function passes `class` only when testthat is >= 2.99.0.9000
  .args <- function(expr) {
    supports_class <- packageVersion("testthat") >= "2.99.0.9000"
    out <- list(
      object = rlang::expr({{ expr }}),
      regexp = "region_isos",
      class = if (supports_class) "has_zero_rows" else NULL
    )
    # Exclude `class = NULL`
    out[!vapply(out, is.null, logical(1))]
  }

  bad_scenario <- fake_scenario(
    region = l$region, scenario_source = l$source, sector = "bad"
  )
  # Silence warnings other than the first one
  suppressWarnings(
    do.call(expect_warning, .args(join_ald_scenario2(l, bad_scenario)))
  )

  bad_reg1 <- tibble(region = "bad", isos = l$isos, source = l$source)
  do.call(expect_warning, .args(join_ald_scenario2(l, region_isos = bad_reg1)))

  bad_reg2 <- tibble(region = l$region, isos = "bad", source = l$source)
  do.call(expect_warning, .args(join_ald_scenario2(l, region_isos = bad_reg2)))

  bad_reg3 <- tibble(region = l$region, isos = l$isos, source = "bad")
  do.call(expect_warning, .args(join_ald_scenario2(l, region_isos = bad_reg3)))
})

test_that("include/excludes `plant_location` inside/outside a region", {
  # styler: off
  region_isos_toy <- tribble(
    ~region,         ~isos, ~source,
    "north america", "us",  "demo_2020",
    "oecd",          "de",  "demo_2020",
    "oecd",          "fr",  "demo_2020",
    "china",         "cn",  "demo_2020",
  )
  # styler: on

  out <- join_ald_scenario(
    fake_matched(),
    # We have isos to match all countries and regions;
    region_isos = region_isos_toy,
    # And we have asset-level data from all countries;
    ald = fake_ald(plant_location = c("de", "fr", "cn", "us")),
    # But our scenario if is only relevant to Europe and China -- not US
    scenario = fake_scenario(region = c("oecd", "china"))
  )

  # The output includes locations inside matching regions

  expect_true(all(unique(out$plant_location) %in% c("de", "fr", "cn")))
  # The output excludes locations outside matching regions
  expect_false(any(unique(out$plant_location) %in% "us"))
})

test_that("outputs the same with upper/lower ald$sector or ald$technology", {
  matched <- fake_matched()
  ald <- fake_ald()
  scenario <- fake_scenario()
  regions <- region_isos_stable

  out_lower <- join_ald_scenario(matched, ald, scenario, regions)

  upper_sector <- modify_at_(ald, "sector", toupper)
  out_upper <- join_ald_scenario(matched, upper_sector, scenario, regions)
  expect_equal(out_upper, out_lower)

  upper_technology <- modify_at_(ald, "technology", toupper)
  out_upper <- join_ald_scenario(matched, upper_technology, scenario, regions)
  expect_equal(out_upper, out_lower)
})
