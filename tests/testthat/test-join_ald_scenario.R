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
})

test_that("with fake data outputs known value", {
  out <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario()
  )

  expect_known_value(out, "ref-join_ald_scenario", update = TRUE)
})

test_that("outputs expected names", {
  expected <- setdiff(
    c(names(fake_matched()), names(fake_ald()), names(fake_scenario())),
    "name_company"
  )

  out <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(),
    scenario = fake_scenario()
  )
  expect_equal(
    sort(unique(names(out))),
    sort(unique(expected))
  )

  out2 <- join_ald_scenario(
    fake_matched(new_column = "anything"),
    ald = fake_ald(),
    scenario = fake_scenario()
  )
  expect_equal(
    sort(unique(c(names(out2)), "new_column")),
    sort(unique(c(expected, "new_column")))
  )
})

test_that("excludes `plant_location`s outside a region", {
  these_regions <- c("oecd_europe", "oecd_europe", "china", "china")
  this_scenario <- dplyr::bind_rows(
    fake_scenario(region = "oecd_europe"),
    fake_scenario(region = "china")
  )
  out <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(plant_location = c("de", "fr", "cn", "us")),
    scenario = this_scenario
  )

  valid_isos_in_these_regions <- r2dii.data::region_isos %>%
    dplyr::filter(region %in% unique(out$region)) %>%
    dplyr::pull(isos) %>%
    unique()

  expect_true(all(unique(out$plant_location) %in% valid_isos_in_these_regions))
})

test_that("case insensitive to input `plant_location`", {
  out1 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(plant_location = c("de")),
    scenario = fake_scenario(region = "oecd_europe")
  )

  out2 <- join_ald_scenario(
    fake_matched(),
    ald = fake_ald(plant_location = c("DE")),
    scenario = fake_scenario(region = "oecd_europe")
  )

  expect_equal(out1, out2)
})
