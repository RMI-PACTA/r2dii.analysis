check_no_value_is_missing <- function(data, column) {
  if (anyNA(data[[column]])) {
    rlang::abort(
      class = "some_value_is_missing",
      glue::glue("Column `{column}` must not contain any `NA`s.")
    )
  }

  invisible(data)
}

# Avoid dependency on purrr
walk <- function(.x, .f, ...) {
  .f <- rlang::as_function(.f)
  lapply(.x, .f, ...)
  invisible(.x)
}

# We can remove this once we depend on R >= 3.5. See ?backports::isTRUE
isTRUE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}

# We can remove this once we depend on R >= 3.5. See ?backports::isFALSE
isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}
