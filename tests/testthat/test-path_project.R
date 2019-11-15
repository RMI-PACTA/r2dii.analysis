test_that("path_project", {
  expect_match(
    path_project("a-project"),
    "PortCheck_v2/10_Projects/a-project"
  )

  expect_equal(
    path_project("a-project", "a", "subdirectory", parent = "a-parent"),
    "a-parent/a-project/a/subdirectory"
  )

  expect_warning(
    path_project("a-project", "a", "subdirectory"),
    NA
  )
})
