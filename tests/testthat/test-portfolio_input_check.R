test_that("read_raw_portfolio outputs a tibble dataframe", {
  out <- "TEST" %>%
    find_project_input_files() %>%
    read_raw_portfolio()

  out <- expect_is(out, "data.frame")
})

test_that("clean_column_names outputs the expected names", {
  out <- "TEST" %>%
    find_project_input_files() %>%
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

test_that("drop_rows_with_empty_string drops expected rows with a warning", {
  # Create a toy dataset
  out <- "TEST" %>%
    find_project_input_files() %>%
    read_raw_portfolio() %>%
    head(4) %>%
    clean_column_names()
  out[1, "investor_name"] <- ""
  out[2, "portfolio_name"] <- ""

  expect_warning(
    out1 <- drop_rows_with_empty_string(out, "investor_name"),
    "Removing.*investor_name.*empty"
  )
  expect_equal(nrow(out1), 3L)

  expect_warning(
    out2 <- drop_rows_with_empty_string(out, "portfolio_name"),
    "Removing.*portfolio_name.*empty"
  )
  expect_equal(nrow(out2), 3L)
})
