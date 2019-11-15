test_that("read_raw_portfolio outputs a data.frame", {
  skip_if_not(r2dii.utils::dropbox_exists())

  out <- "TEST" %>%
    find_project_input_files() %>%
    read_raw_portfolio()

  expect_is(out, "data.frame")
  expect_true(ncol(out) > 1)
})

test_that("read_raw_portfolio reads a comma-separated .csv", {
  skip_if_not(r2dii.utils::dropbox_exists())

  path <- r2dii.utils::path_example("raw_portfolio.csv", "r2dii.analysis")
  expect_is(read_raw_portfolio(path), "data.frame")
})

test_that("read_raw_portfolio reads a semi-colon-separated .csv", {
  skip_if_not(r2dii.utils::dropbox_exists())

  path <- r2dii.utils::path_example(
    "raw_portfolio_semicolon.csv", "r2dii.analysis"
  )

  expect_is(read_raw_portfolio(path, delim = ";"), "data.frame")
})
