#' Summarize build-out based on the weight of each loan per sector per year
#'
#' Summarize build-out based on the weight of each loan per sector per year.
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
#' @section Warning:
#' Build-out is undefined for companies that have no initial production.
#' Build-out percentage in this case would be infinite! Companies with 0
#' production will be removed automatically prior to analysis.
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
#' summarize_weighted_buildout(master)
#'
#' summarize_weighted_buildout(master, use_credit_limit = TRUE)
summarize_weighted_buildout <- function(data, ..., use_credit_limit = FALSE) {
  data %>%
    add_weighted_loan_buildout(use_credit_limit = use_credit_limit) %>%
    group_by(.data$sector, .data$technology, .data$year, ...) %>%
    summarize(
      weighted_buildout = mean(.data$weighted_loan_buildout)
    ) %>%
    # Restore old groups
    group_by(!!!dplyr::groups(data))
}

add_weighted_loan_buildout <- function(data, use_credit_limit = FALSE) {

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
    "year",
    "scenario"
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

  data_with_buildout <- data %>%
    add_buildout()

  check_and_filter_infinite_buildout(data_with_buildout)

  data_with_buildout %>%
    left_join(total_size_by_sector, by = "sector") %>%
    mutate(
      loan_weight = .data[[loan_size]] / .data$total_size,
      weighted_loan_buildout = .data$build_out * .data$loan_weight
    ) %>%
    group_by(!!!old_groups)
}

add_buildout <- function(data){

  cols <- names(data)

  data %>%
    inner_join(green_or_brown, by = c(.data$sector, .data$technology)) %>%
    group_by(.data$sector, .data$year, .data$scenario) %>%
    mutate(sector_production = sum(production)) %>%
    group_by(.data$name_ald) %>%
    arrange(name_ald, year) %>%
    mutate(brown_build_out = (production - first(production))/first(production),
           green_build_out = (production - first(production))/first(sector_production)) %>%
    mutate(build_out = case_when(green_or_brown == "green" ~ green_build_out,
                                 green_or_brown == "brown" ~ brown_build_out)) %>%
    select(c(cols, "build_out")) %>%
    ungroup()

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

check_and_filter_infinite_buildout <- function(data) {

  #TODO: Filter out companies with 0 production in start year, and warn!

  invisible(data)
}
