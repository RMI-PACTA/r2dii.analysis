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

rename_and_warn_ald_names <- function(data) {

  if (all(c("name_ald", "name_abcd") %in% names(data))) {

    rlang::abort(
      "too_many_sectors",
      message = glue(
        "Column `name_ald` is deprecated as of r2dii.match 0.1.0, please use
        `name_abcd` instead."
      )
    )

  } else if ("name_ald" %in% names(data)) {

    rlang::warn(
      "deprecated_name",
      message = glue(
        "Column `name_ald` is deprecated as of r2dii.match 0.1.0, please use
        `name_abcd` instead."
      )
    )

    data <- dplyr::rename(data, name_abcd = .data$name_ald)
  }

  if (all(c("sector_ald", "sector_abcd") %in% names(data))) {

    rlang::abort(
      "too_many_sectors",
      message = glue(
        "Column `sector_ald` is deprecated as of r2dii.analysis 0.2.0, please use
        `sector_abcd` instead."
      )
    )

  } else if ("sector_ald" %in% names(data)) {

    rlang::warn(
      "deprecated_name",
      message = glue(
        "Column `sector_ald` is deprecated as of r2dii.analysis 0.2.0, please use
        `sector_abcd` instead."
      )
    )

    data <- dplyr::rename(data, sector_abcd = .data$sector_ald)
  }

  data

}
