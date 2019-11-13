test_that("path_project_dirs outputs a character vector", {
  expect_is(path_project_dirs("a-project"), "character")
})

test_that("path_project_dirs outputs an 'fs_path'", {
  expect_is(path_project_dirs("a-project"), "fs_path")
})

test_that("path_project_dirs outputs the expected nested directories", {
  expect_equal(
    fs::path_file(path_project_dirs("a-project")),
    c(
      "00_Log_Files",
      "10_Parameter_File",
      "20_Raw_Inputs",
      "30_Processed_Inputs",
      "40_Results",
      "50_Outputs"
    )
  )
})

test_that("path_project_dirs outputs the expected project directory", {
  expect_equal(
    unique(fs::path_file(fs::path_dir(path_project_dirs("a-project")))),
    "a-project"
  )
})
