test_that("with_path_in_10_projects returns the expected path", {
  expect_is(with_path_in_10_projects("a-project"), "function")

  actual <- with_path_in_10_projects("a-dir")()
  expected <- fs::path(r2dii.utils::dbox_port2_10proj(), "a-dir")

  expect_equal(actual, expected)

  expect_true(
    grepl(".*Dropbox .2. Investing..PortCheck_v2.10_Projects.a-dir$", actual)
  )
})
