check_no_value_is_missing <- function(data, column) {
  if (anyNA(data[[column]])) {
    rlang::abort(
      class = "some_value_is_missing",
      glue::glue("Column `{column}` must not contain any `NA`s.")
    )
  }

  invisible(data)
}
