check_no_value_is_missing <- function(data, column) {
  if (anyNA(data[[column]])) {
    abort(
      class = "some_value_is_missing",
      glue("Column `{column}` must not contain any `NA`s.")
    )
  }

  invisible(data)
}

check_unique_id <- function(data, column) {
  if (sum(duplicated(data[[column]]))) {
    abort(
      class = "unique_ids",
      glue("Column `{column}` must not contain any duplicates.", column)
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

# We can remove this once we depend on R >= 3.5. See ?backports::isTRUE
isTRUE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}

# We can remove this once we depend on R >= 3.5. See ?backports::isFALSE
isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

aggregate_ald_by_columns <- function(data, columns) {
  data %>%
    dplyr::group_by_at(setdiff(names(data), c("production", "emission_factor", columns))) %>%
    dplyr::filter(.data$production > 0) %>%
    mutate(
      weight = .data$production / sum(.data$production)
    ) %>%
    summarize(
      production = sum(.data$production),
      emission_factor = sum(.data$emission_factor * .data$weight / sum(.data$weight))
    ) %>%
    ungroup()
}

tmsr_or_smsp <- function() {
  dplyr::tribble(
    ~which_metric, ~green_or_brown,
    "tmsr_target_weighted_production", "brown",
    "smsp_target_weighted_production", "green"
  )
}
