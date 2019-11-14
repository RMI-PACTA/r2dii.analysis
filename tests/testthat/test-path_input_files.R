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

test_that("path_output_files outputs an object of the expected class", {
  out <- path_output_files("a-project")
  expect_is(out, "character")
  expect_is(out, "fs_path")
})

test_that("path_output_files output matches the expected paths", {
  expected <- c(
    ".*PortCheck_v2/10_Projects/a-project/ProjectName_Input.csv",
    ".*PortCheck_v2/10_Projects/a-project/ReportParameterFile.yml",
    ".*PortCheck_v2/10_Projects/a-project/AnalysisParameters.yml"
  )
  out <- path_output_files("a-project")
  all_matching <- all(purrr::map_lgl(expected, ~ any(grepl(.x, out))))
  expect_true(all_matching)
})

test_that("path_output_files is sensitive to `parent`", {
  fourth_parent <- function(x) {
    x %>%
      fs::path_dir() %>%
      fs::path_dir() %>%
      fs::path_dir()
  }

  "a-project" %>%
    path_output_files(parent = "parent") %>%
    fourth_parent() %>%
    unique() %>%
    expect_equal(".")
})
