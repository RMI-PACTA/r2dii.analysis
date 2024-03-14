check_no_value_is_missing <- function(data, column) {
  if (anyNA(data[[column]])) {
    abort(
      class = "some_value_is_missing",
      glue("Column `{column}` must not contain any `NA`s.")
    )
  }

  invisible(data)
}

filter_and_warn_na <- function(data, column) {
  if (anyNA(data[[column]])) {
    name_dataset <- deparse(substitute(data))
    warning_message = paste("Removing rows in", name_dataset, "where `{column}` is NA")
    warn(
      glue(warning_message),
      class = "na_crucial_economic_input"
    )

    data <- filter(data, !is.na(.data[[column]]))
  }
  return(data)
}

fill_and_warn_na <- function(data, column) {
  if (anyNA(data[[column]])) {
    name_dataset <- deparse(substitute(data))
    warning_message = paste("Filling in rows of", name_dataset, "where `{column}` is NA with 0")
    warn(
      glue(warning_message),
      class = "fill_nas_crucial_economic_input"
    )

    data[[column]] <- tidyr::replace_na(data[[column]], 0)
  }
  return(data)
}

warn_grouped <- function(data, message) {
  if (dplyr::is_grouped_df(data)) warn(message)

  invisible(data)
}

warn_if_by_company_and_weight_production <- function(by_company,
                                                     weight_production) {
  if (by_company & weight_production) {
    warn(
      glue(
        "You've supplied `by_company = TRUE` and `weight_production = TRUE`.
        This will result in company-level results, weighted by the portfolio
        loan size, which is rarely useful. Did you mean to set one of these
        arguments to `FALSE`?"
      )
    )
  }
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

change_to_lowercase_and_warn <- function(data, column) {
  if(any(data[[column]] != tolower(data[[column]]), na.rm = TRUE)) {
    name_dataset <- deparse(substitute(data))
    warning_message = paste("The column `{column}` of", name_dataset, "has been updated to only contain lower-cases.")
    warn(
      glue(warning_message),
      class = "column_not_in_lowercase"
    )
    data[[column]] <- tolower(data[[column]])
  }

  return(data)

}
