test_that("path_input_files outputs an object of the expected class", {
  out <- path_input_files()
  expect_is(out, "character")
  expect_is(out, "fs_path")
  # expect_true(grepl("Portcheck_v2/00_Administration/20_Input_Files", out))
})

test_that("path_input_files output matches the expected paths", {
  expected <- c(
    ".*Portcheck_v2/00_Administration/20_Input_Files/ProjectName_Input.csv",
    ".*Portcheck_v2/00_Administration/20_Input_Files/ReportParameterFile.yml",
    ".*Portcheck_v2/00_Administration/20_Input_Files/AnalysisParameters.yml"
  )
  out <- path_input_files()
  all_matching <- all(purrr::map_lgl(expected, ~ any(grepl(.x, out))))
  expect_true(all_matching)
})
