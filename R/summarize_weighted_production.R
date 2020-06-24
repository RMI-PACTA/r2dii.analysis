#' Summarize production based on the weight of each loan per sector per year
#'
#' Summarize production based on the weight of each loan per sector per year.
#'
#' @param data A data frame like the output of [join_ald_scenario()].
#' @param use_credit_limit Logical vector of length 1. `FALSE` defaults to using
#'   the column `loan_size_outstanding`. Set to `TRUE` to use the column
#'   `loan_size_credit_limit` instead.
#' @param ... Variables to group by.
#'
#' @seealso [join_ald_scenario()].
#'
#' @export
#'
#' @family utility functions
#'
#' @return A tibble with the same groups as the input (if any) and columns:
#'   `sector`, `technology`, `year`, and `weighted_production`.
#'
#' @examples
#' library(r2dii.analysis)
#' library(r2dii.data)
#' library(r2dii.match)
#'
#' master <- r2dii.data::loanbook_demo %>%
#'   r2dii.match::match_name(r2dii.data::ald_demo) %>%
#'   r2dii.match::prioritize() %>%
#'   join_ald_scenario(r2dii.data::ald_demo,
#'     r2dii.data::scenario_demo_2020,
#'     region_isos = region_isos_demo
#'   )
#'
#' summarize_weighted_production(master)
#'
#' summarize_weighted_production(master, use_credit_limit = TRUE)
summarize_weighted_production <- function(data, ..., use_credit_limit = FALSE) {
  data %>%
    add_weighted_loan_production(use_credit_limit = use_credit_limit) %>%
    group_by(.data$sector, .data$technology, .data$year, ...) %>%
    summarize(
      weighted_production = sum(.data$weighted_loan_production)
    ) %>%
    # Restore old groups
    group_by(!!!dplyr::groups(data))
}

add_weighted_loan_production <- function(data, use_credit_limit = FALSE) {
  stopifnot(
    is.data.frame(data),
    isTRUE(use_credit_limit) || isFALSE(use_credit_limit)
  )

  loan_size <- paste0(
    "loan_size_", ifelse(use_credit_limit, "credit_limit", "outstanding")
  )

  crucial <- c(
    "id_loan",
    loan_size,
    "production",
    "sector",
    "technology",
    "year"
  )
  check_crucial_names(data, crucial)
  walk(crucial, ~ check_no_value_is_missing(data, .x))

  old_groups <- dplyr::groups(data)
  data <- ungroup(data)

  distinct_loans_by_sector <- data %>%
    ungroup() %>%
    group_by(.data$sector) %>%
    distinct(.data$id_loan, .data[[loan_size]]) %>%
    check_unique_loan_size_values_per_id_loan()

  total_size_by_sector <- distinct_loans_by_sector %>%
    summarize(total_size = sum(.data[[loan_size]]))

  data %>%
    left_join(total_size_by_sector, by = "sector") %>%
    mutate(
      loan_weight = .data[[loan_size]] / .data$total_size,
      weighted_loan_production = .data$production * .data$loan_weight
    ) %>%
    group_by(!!!old_groups)
}

check_unique_loan_size_values_per_id_loan <- function(data) {
  dups <- data %>%
    group_by(.data$sector, .data$id_loan) %>%
    mutate(is_duplicated = any(duplicated(.data$id_loan))) %>%
    ungroup() %>%
    filter(.data$is_duplicated)

  if (nrow(dups) > 0L) {
    abort(
      class = "multiple_loan_size_values_by_id_loan",
      "Every `id_loan` by `sector` must have unique `loan_size*` values."
    )
  }

  invisible(data)
}
