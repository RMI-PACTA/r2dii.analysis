test_that("path_project_files", {
  out <- path_project_files()
  expect_true(grepl("Portcheck_v2/00_Administration/20_Input_Files", out))
})
