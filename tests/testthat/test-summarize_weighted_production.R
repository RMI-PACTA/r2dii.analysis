library(r2dii.data)

# Production --------------------------------------------------------------

test_that("with bad `data` errors with informative message", {
  expect_error(summarize_weighted_production("bad"), "data.frame.*not.*TRUE")
})

test_that("with bad use_credit_limit errors with informative message", {
  expect_error(
    summarize_weighted_production(fake_master(), use_credit_limit = "bad"),
    "not TRUE"
  )

  expect_error(
    summarize_weighted_production(fake_master(), use_credit_limit = 1),
    "not TRUE"
  )

  expect_error(
    summarize_weighted_production(fake_master(), use_credit_limit = NA),
    "not TRUE"
  )

  expect_error(
    summarize_weighted_production(fake_master(), use_credit_limit = NULL),
    "not TRUE"
  )
})

test_that("with data lacking crucial columns errors with informative message", {
  expect_error_missing_names <- function(name, use_credit_limit = FALSE) {
    data <- rename(fake_master(), bad = name)

    expect_error(
      class = "missing_names",
      summarize_weighted_production(data, use_credit_limit = use_credit_limit)
    )
  }

  expect_error_missing_names("id_loan")
  expect_error_missing_names("loan_size_outstanding")
  expect_error_missing_names("loan_size_credit_limit", use_credit_limit = TRUE)
  expect_error_missing_names("production")
  expect_error_missing_names("sector_ald")
  expect_error_missing_names("technology")
  expect_error_missing_names("year")
})

test_that("with NAs in crucial columns errors with informative message", {
  expect_error_crucial_NAs <- function(name, use_credit_limit = FALSE) {
    data <- fake_master(
      technology = c("ta", "ta", "tb", "tb"),
      id_loan = c("i1", "i2", "i1", "i2"),
      loan_size_outstanding = c(40, 10, 40, 10),
      production = c(10, 30, 20, 40),
    )

    data[1, name] <- NA
    expect_error(
      class = "some_value_is_missing",
      summarize_weighted_production(data, use_credit_limit = use_credit_limit)
    )
  }

  expect_error_crucial_NAs("id_loan")
  expect_error_crucial_NAs("loan_size_outstanding")
  expect_error_crucial_NAs("loan_size_credit_limit", use_credit_limit = TRUE)
  expect_error_crucial_NAs("production")
  expect_error_crucial_NAs("sector_ald")
  expect_error_crucial_NAs("technology")
  expect_error_crucial_NAs("year")
})

test_that("with bad but unused loan_size_column is error free", {
  bad_unused <- rename(fake_master(), bad = "loan_size_credit_limit")
  expect_error_free(
    summarize_weighted_production(bad_unused, use_credit_limit = FALSE)
  )

  bad_unused <- rename(fake_master(), bad = "loan_size_outstanding")
  expect_error_free(
    summarize_weighted_production(bad_unused, use_credit_limit = TRUE)
  )
})

test_that("with duplicated loan_size by id_loan throws error", {
  expect_error(
    class = "multiple_loan_size_values_by_id_loan",
    summarize_weighted_production(fake_master(loan_size_outstanding = 1:2))
  )

  expect_error(
    class = "multiple_loan_size_values_by_id_loan",
    summarize_weighted_production(
      fake_master(loan_size_credit_limit = 1:2),
      use_credit_limit = TRUE
    )
  )
})

test_that("with known input outputs as expected", {
  # styler: off
  data <- fake_master(
    technology =            c("ta", "ta", "tb", "tb"),
    id_loan    =            c("i1", "i2", "i1", "i2"),
    loan_size_outstanding = c( 40,   10,   40,   10),
    production            = c( 10,   30,   20,   40),
  )
  out1 <- summarize_weighted_production(data)
  out1$weighted_production
  expect_equal(out1$weighted_production, c(14, 24))

  # Is sensitive to `use_credit_limit`
  # Reversing loan_size and production outputs reverse result
  data2 <- fake_master(
    technology =             c("ta", "ta", "tb", "tb"),
    id_loan    =             c("i1", "i2", "i1", "i2"),
    loan_size_credit_limit = c( 10,   40,   10,   40),
    production            =  c( 40,   20,   30,   10),
  )
  out2 <- summarize_weighted_production(data2, use_credit_limit = TRUE)
  expect_equal(out2$weighted_production, c(24, 14))
  # styler: on
})

test_that("outputs expected names", {
  expect_named(
    summarize_weighted_production(fake_master()),
    c(
      "sector_ald",
      "technology",
      "year",
      "weighted_production",
      "weighted_technology_share"
    )
  )
})

test_that("with multiple years outputs as expected", {
  # styler: off
  data <- fake_master(
    technology =            c("ta", "ta"),
    id_loan    =            c("i1", "i2"),
    loan_size_outstanding = c(40,   10),
    production            = c(10,   30),
  )
  # styler: on

  data2 <- dplyr::bind_rows(data, data)
  data2$year <- c(2020, 2020, 2021, 2021)

  expect_equal(
    summarize_weighted_production(data2)$year,
    c(2020, 2021)
  )
  expect_equal(
    summarize_weighted_production(data2)$weighted_production,
    c(14, 14)
  )
})

test_that("with grouped data returns same groups as input", {
  out <- fake_master() %>%
    group_by(.data$sector_ald) %>%
    summarize_weighted_production()

  expect_equal(dplyr::group_vars(out), "sector_ald")
})

test_that("can group by `plant_location`", {
  data <- fake_master(plant_location = c("a", "b"))
  out <- summarize_weighted_production(data, plant_location)
  expect_equal(nrow(out), 2L)
})

test_that("preserves groups passed to ...", {
  data <- fake_master(plant_location = c("a", "b")) %>%
    group_by(plant_location)

  out <- summarize_weighted_production(data, plant_location)
  expect_equal(nrow(out), 2L)

  expect_equal(dplyr::group_vars(out), "plant_location")
})

# Percent-change ---------------------------------------------------------------

test_that("with bad `data` errors with informative message", {
  expect_error(
    summarize_weighted_percent_change("bad"),
    "data.frame.*not.*TRUE"
  )
})

test_that("with bad use_credit_limit errors with informative message", {
  expect_error(
    summarize_weighted_percent_change(fake_master(), use_credit_limit = "bad"),
    "not TRUE"
  )

  expect_error(
    summarize_weighted_percent_change(fake_master(), use_credit_limit = 1),
    "not TRUE"
  )

  expect_error(
    summarize_weighted_percent_change(fake_master(), use_credit_limit = NA),
    "not TRUE"
  )

  expect_error(
    summarize_weighted_percent_change(fake_master(), use_credit_limit = NULL),
    "not TRUE"
  )
})

test_that("with data lacking crucial columns errors with informative message", {
  expect_error_missing_names <- function(name, use_credit_limit = FALSE) {
    data <- rename(fake_master(), bad = name)

    expect_error(
      class = "missing_names",
      summarize_weighted_percent_change(
        data,
        use_credit_limit = use_credit_limit
      )
    )
  }

  expect_error_missing_names("id_loan")
  expect_error_missing_names("loan_size_outstanding")
  expect_error_missing_names("loan_size_credit_limit", use_credit_limit = TRUE)
  expect_error_missing_names("production")
  expect_error_missing_names("sector_ald")
  expect_error_missing_names("technology")
  expect_error_missing_names("year")
})

test_that("with NAs in crucial columns errors with informative message", {
  expect_error_crucial_NAs <- function(name, use_credit_limit = FALSE) {
    data <- fake_master(
      technology = c("ta", "ta", "tb", "tb"),
      id_loan = c("i1", "i2", "i1", "i2"),
      loan_size_outstanding = c(40, 10, 40, 10),
      production = c(10, 30, 20, 40),
    )

    data[1, name] <- NA
    expect_error(
      class = "some_value_is_missing",
      summarize_weighted_percent_change(
        data,
        use_credit_limit = use_credit_limit
      )
    )
  }

  expect_error_crucial_NAs("id_loan")
  expect_error_crucial_NAs("loan_size_outstanding")
  expect_error_crucial_NAs("loan_size_credit_limit", use_credit_limit = TRUE)
  expect_error_crucial_NAs("production")
  expect_error_crucial_NAs("sector_ald")
  expect_error_crucial_NAs("technology")
  expect_error_crucial_NAs("year")
})

test_that("with bad but unused loan_size_column is error free", {
  bad_unused <- rename(fake_master(), bad = "loan_size_credit_limit")
  expect_error_free(
    summarize_weighted_percent_change(bad_unused, use_credit_limit = FALSE)
  )

  bad_unused <- rename(fake_master(), bad = "loan_size_outstanding")
  expect_error_free(
    summarize_weighted_percent_change(bad_unused, use_credit_limit = TRUE)
  )
})

test_that("with duplicated loan_size by id_loan throws error", {
  expect_error(
    class = "multiple_loan_size_values_by_id_loan",
    summarize_weighted_percent_change(fake_master(loan_size_outstanding = 1:2))
  )

  expect_error(
    class = "multiple_loan_size_values_by_id_loan",
    summarize_weighted_percent_change(
      fake_master(loan_size_credit_limit = 1:2),
      use_credit_limit = TRUE
    )
  )
})

test_that("outputs expected names", {
  expect_named(
    summarize_weighted_percent_change(fake_master()),
    c("sector_ald", "technology", "year", "weighted_percent_change")
  )
})

test_that("is sensitive to `use_credit_limit`", {
  data <- fake_master(
    name_ald = rep(c("company a", "company b"), 2),
    year = c(2020, 2020, 2021, 2021),
    id_loan = c("i1", "i2", "i1", "i2"),
    loan_size_credit_limit = c(20, 30, 20, 30),
    production = c(10, 10, 20, 40),
  )
  out2 <- data %>%
    summarize_weighted_percent_change(name_ald, use_credit_limit = TRUE)
  out3 <- data %>%
    summarize_weighted_percent_change(name_ald, use_credit_limit = FALSE)

  expect_false(identical(out2, out3))
})

test_that("with grouped data returns same groups as input", {
  out <- fake_master() %>%
    group_by(.data$sector_ald) %>%
    summarize_weighted_percent_change()

  expect_equal(dplyr::group_vars(out), "sector_ald")
})

test_that("can group by `plant_location`", {
  data <- fake_master(plant_location = c("a", "b"))
  out <- summarize_weighted_percent_change(data, plant_location)
  expect_equal(nrow(out), 2L)
})

test_that("outputs names that include two grouping columns", {
  data <- fake_master(plant_location = c("a", "b"))
  out <- summarize_weighted_percent_change(data, plant_location, region)

  grp_cols <- c("plant_location", "region")
  names_include_groups <- identical(setdiff(grp_cols, names(out)), character(0))
  expect_true(names_include_groups)
})

test_that("preserves groups passed to ...", {
  data <- fake_master(plant_location = c("a", "b")) %>%
    group_by(plant_location)

  out <- summarize_weighted_percent_change(data, plant_location)
  expect_equal(nrow(out), 2L)

  expect_equal(dplyr::group_vars(out), "plant_location")
})

test_that("with zero initial production errors with informative message", {
  data <- fake_master(production = 0)

  expect_error(
    class = "zero_initial_production",
    summarize_weighted_percent_change(data)
  )
})

test_that("with known input outputs as expected", {
  data <- fake_master(
    name_ald = rep(c("company a", "company b"), 2),
    year = c(2020, 2020, 2021, 2021),
    id_loan = c("i1", "i2", "i1", "i2"),
    production = c(10, 10, 20, 40),
  )
  out1 <- summarize_weighted_percent_change(data, name_ald)
  out1$weighted_percent_change
  expect_equal(out1$weighted_percent_change, c(0, 0, 50, 150))

  # Is sensitive to `use_credit_limit`
  # Reversing loan_size and production outputs reverse result
  data2 <- fake_master(
    name_ald = rep(c("company a", "company b"), 2),
    year = c(2020, 2020, 2021, 2021),
    id_loan = c("i1", "i2", "i1", "i2"),
    loan_size_credit_limit = c(20, 30, 20, 30),
    production = c(10, 10, 20, 40),
  )
  out2 <- data2 %>%
    summarize_weighted_percent_change(name_ald, use_credit_limit = TRUE)
  expect_equal(out2$weighted_percent_change, c(0, 0, 40, 180))
})

test_that("with different currencies errors with informative message (#137)", {
  # styler: off
  data <- fake_master(
    id_loan = c(1,2),
    loan_size_outstanding_currency = c("a", "b"),
    loan_size_credit_limit_currency = c("a","b")
  )

  # outstanding
  expect_error(
    summarize_weighted_production(data),
    class = "multiple_currencies"
  )

  #credit_limit
  expect_error(
    summarize_weighted_production(data, use_credit_limit = TRUE),
    class = "multiple_currencies"
  )
})
