test_that("read_raw_portfolio outputs a data.frame", {
  skip_if_not(r2dii.utils::dropbox_exists())

  out <- "raw_portfolio.csv" %>%
    path_example("r2dii.analysis") %>%
    read_raw_portfolio()

  expect_is(out, "data.frame")
  expect_true(ncol(out) > 1)
})

test_that("read_raw_portfolio reads a comma-separated .csv", {
  path <- path_example("raw_portfolio.csv", "r2dii.analysis")
  expect_is(read_raw_portfolio(path), "data.frame")
})

test_that("read_raw_portfolio reads a semi-colon-separated .csv", {
  out <- "raw_portfolio_semicolon.csv" %>%
    path_example("r2dii.analysis") %>%
    read_raw_portfolio(delim = ";")

  expect_is(out, "data.frame")
})

test_that("find_project_input_files finds expected files", {
  expect_match(
    find_project_input_files("TEST"),
    "PortCheck_v2/10_Projects/TEST/20_Raw_Inputs/TEST_Input.csv"
  )
})
