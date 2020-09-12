check_no_value_is_missing <- function(data, column) {
  if (anyNA(data[[column]])) {
    abort(
      class = "some_value_is_missing",
      glue("Column `{column}` must not contain any `NA`s.")
    )
  }

  invisible(data)
}

warn_grouped <- function(data, message) {
  if (dplyr::is_grouped_df(data)) warn(message)

  invisible(data)
}

# Avoid dependency on purrr
walk_ <- function(.x, .f, ...) {
  .f <- rlang::as_function(.f)
  lapply(.x, .f, ...)
  invisible(.x)
}

# Avoid dependency on purrr
modify_at_ <- function(.x, .at, .f) {
  .x[[.at]] <- .f(.x[[.at]])
  .x
}
