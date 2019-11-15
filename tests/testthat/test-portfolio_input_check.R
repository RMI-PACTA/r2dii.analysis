test_that("portfolio_input_check runs until TODO", {
  portfolio <- "raw_portfolio.csv" %>%
    path_example("r2dii.analysis") %>%
    read_raw_portfolio()

  expect_error(
    suppressWarnings(portfolio_input_check(portfolio)),
    "TODO"
  )
})

test_that("read_raw_portfolio outputs a tibble dataframe", {
  out <- "raw_portfolio.csv" %>%
    path_example("r2dii.analysis") %>%
    read_raw_portfolio()

  out <- expect_is(out, "data.frame")
})

test_that("clean_column_names outputs the expected names", {
  out <- "raw_portfolio.csv" %>%
    path_example("r2dii.analysis") %>%
    read_raw_portfolio() %>%
    clean_column_names()

  expect_is(out, "data.frame")

  expected_names <- c(
    "investor_name",
    "portfolio_name",
    "isin",
    "number_of_shares",
    "market_value",
    "currency"
  )

  expect_named(out, expected_names)
})

test_that("is_missing detects '' and `NA`", {
  expect_equal(
    is_missing(c("a", "", NA)),
    c(FALSE, TRUE, TRUE)
  )
})

test_that("warn_if_removing warns if any is TRUE", {
  expect_warning(warn_if_removing(TRUE), "Removing 1")
  expect_warning(warn_if_removing(c(FALSE, TRUE)), "Removing 1")
  expect_warning(warn_if_removing(FALSE), NA)
})

test_that("is_missing + dplyr::filter drops expected rows with a warning", {
  # Create a toy dataset
  out <- "raw_portfolio.csv" %>%
    path_example("r2dii.analysis") %>%
    read_raw_portfolio() %>%
    head(4) %>%
    clean_column_names()
  out[1, "investor_name"] <- ""

  expect_warning(
    out <- out %>%
      dplyr::filter(warn_if_removing(!is_missing(.data$investor_name))),
    "Removing"
  )
  expect_equal(nrow(out), 3L)
})

test_that("may_add_column_holding_id adds holding_id if it doesnt exist", {
  portfolio <- dplyr::tibble(x = 1)
  expect_named(may_add_column_holding_id(portfolio), c("x", "holding_id"))

  portfolio <- dplyr::tibble(holding_id = 1)
  expect_named(may_add_column_holding_id(portfolio), "holding_id")
})
