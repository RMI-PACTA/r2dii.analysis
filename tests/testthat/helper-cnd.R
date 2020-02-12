expect_error_free <- function(object, ...) {
  expect_error(object, regexp = NA)
}
