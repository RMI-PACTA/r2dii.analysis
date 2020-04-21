check_no_value_is_missing <- function(data, column) {
  if (anyNA(data[[column]])) {
    rlang::abort(
      class = "column_has_na",
      glue::glue("Column `{column}` must not contain any `NA`s.")
    )
  }

  invisible(data)
}
