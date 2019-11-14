test_that("data_path with 'a' creates a path to file 'a'", {
  expect_equal(
    fs::path_file(data_path("a")),
    "a"
  )
})

test_that("data_path with and without arguments is an fs_path", {
  expect_is(data_path("a"), "fs_path")
  expect_is(data_path("a", "b"), "fs_path")

  expect_is(data_path(), "fs_path")
})

test_that("data_path outputs the expected path inside 2dii's drobbox", {
  expect_match(
    data_path(),
    "^.*Dropbox .2. Investing..PortCheck.00_Data$"
  )
})

test_that("project_meta_investor_name outputs the expected string", {
  config <- r2dii.utils::create_config("
    default:
      ComparisonBenchmarks:
        MetaInvestorName: value
  ")

  expect_equal(
    project_meta_investor_name(TRUE, file = config),
    "Project value"
  )
})

test_that("project_meta_portfolio_name outputs the expected string", {
  config <- r2dii.utils::create_config("
    default:
      ComparisonBenchmarks:
        MetaPortfolioName: value
  ")

  expect_equal(
    project_meta_portfolio_name(TRUE, file = config),
    "Project value"
  )
})

test_that("is_dataframe_with_some_row works as expected", {
  expect_true(is_dataframe_with_some_row(mtcars))
  expect_true(is_dataframe_with_some_row(mtcars[ , 0]))

  # Not a dataframe
  expect_false(is_dataframe_with_some_row(1))
  # No rows
  expect_false(is_dataframe_with_some_row(mtcars[0, ]))
})

test_that("is_na_or_empty_string with length-1 input works as expected", {
  expect_true(is_na_or_empty_string(NA))
  expect_true(is_na_or_empty_string(""))

  # FIXME: Works with non string. Is this okay? (ASK @Clare2D)
  expect_false(is_na_or_empty_string(" "))
  expect_false(is_na_or_empty_string(1))

  # The original implementation works only with length-1 inputs. I made this
  # explicit via an assertion. Is this the expected behaviour? (ASK @Clare2D)
  expect_error(is_na_or_empty_string(NULL), "has_length_1 is not TRUE")
  expect_error(is_na_or_empty_string(1:2), "has_length_1 is not TRUE")
})
