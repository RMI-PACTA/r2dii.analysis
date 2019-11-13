test_that("get_project_paths outputs a character vector", {
  expect_is(get_project_paths("a-project"), "character")
})

test_that("get_project_paths outputs an 'fs_path'", {
  expect_is(get_project_paths("a-project"), "fs_path")
})

test_that("get_project_paths outputs the expected nested directories", {
  expect_equal(
    fs::path_file(get_project_paths("a-project")),
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

test_that("get_project_paths outputs the expected project directory", {
  expect_equal(
    unique(fs::path_file(fs::path_dir(get_project_paths("a-project")))),
    "a-project"
  )
})
