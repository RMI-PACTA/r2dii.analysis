test_that("matches raw data dictionary CSVs", {
  if (nzchar(Sys.getenv("R_CMD"))) {
    skip("Not run in R CMD check (env var `R_CMD` is set)")
  }
  if (nzchar(Sys.getenv("TESTTHAT_IS_CHECKING"))) {
    skip("Not run in `testthat::test_check()` (env var `TESTTHAT_IS_CHECKING` is set)")
  }
  
  paths <- list.files(test_path("../../data-raw/data_dictionary"), full.names = TRUE)
  out <- readr::read_csv(file = paths, show_col_types = FALSE)
  data_raw <- out[order(out[["dataset"]], out[["column"]]), , drop = FALSE]

  comp <- waldo::compare(data_raw, data_dictionary)

  expect(
    ok = length(comp) == 0,
    failure_message = cli::format_message(c(
      "{.var data_dictionary} does not match the raw data dictionary CSVs",
      i = "You may need to manually run {.file data-raw/build_data_dictionary.R}"
    ))
  )
})

test_that("columns are complete", {
  expect(
    ok = !any(is.na(data_dictionary[["dataset"]])),
    failure_message = "the `dataset` column contains `NA` value/s"
  )

  expect(
    ok = !any(is.na(data_dictionary[["column"]])),
    failure_message = "the `column` column contains `NA` value/s"
  )

  expect(
    ok = !any(is.na(data_dictionary[["typeof"]])),
    failure_message = "the `typeof` column contains `NA` value/s"
  )

  expect(
    ok = !any(is.na(data_dictionary[["definition"]])),
    failure_message = "the `definition` column contains `NA` value/s"
  )
})
