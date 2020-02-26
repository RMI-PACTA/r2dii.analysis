#' Adds column `loan_weighted_production`
#'
#' This function computes the production of a portfolio weighted by loan size.
#' It aggregates the production -- weighted by loan size -- off all loans for
#' each sector, technology, and year.
#'
#' @param data A dataset of class "data.frame", like the output of
#'   [join_ald_scenario()].
#' @param use_loan_size_credit_limit Logical vector of length 1. `FALSE`
#'   defaults to using the column `loan_size_outstanding`. Set to `TRUE` to use
#'   the column `loan_size_credit_limit` instead.
#'
#' @seealso [join_ald_scenario()].
#'
#' @return A dataframe with the same columns as `data` and the additional column
#'   `loan_weighted_production`, where each row is aggregates information of all
#'   `loan_id`s within groups defined by the unique combinations of the columns
#'   `sector`, `technology`, and `year`.
#'
#' @export
#'
#' @examples
#' library(r2dii.dataraw)
#'
#' master <- r2dii.dataraw::loanbook_demo %>%
#'   r2dii.match::match_name(r2dii.dataraw::ald_demo) %>%
#'   r2dii.match::prioritize() %>%
#'   join_ald_scenario(r2dii.dataraw::ald_demo, r2dii.dataraw::scenario_demo)
#'
#' add_loan_weighted_production(master)
#'
#' add_loan_weighted_production(master, use_loan_size_credit_limit = TRUE)
add_loan_weighted_production <- function(data, use_loan_size_credit_limit = FALSE) {
  old_groups <- dplyr::groups(data)

  crucial <- c(
    "id_loan",
    "loan_size_credit_limit",
    "loan_size_outstanding",
    "production",
    "sector",
    "technology",
    "year"
  )
  check_crucial_names(data, crucial)

  loan_size <- "loan_size_outstanding"
  if (use_loan_size_credit_limit) {
    loan_size <- "loan_size_credit_limit"
  }

  data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::mutate(loan_size_by_sector = sum(.data[[loan_size]])) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$id_loan) %>%
    dplyr::mutate(
      loan_size_by_sector_w = .data[[loan_size]] / .data$loan_size_by_sector
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      production_proxy_w = .data$loan_size_by_sector_w * .data$production
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$sector, .data$technology, .data$year) %>%
    mutate(loan_weighted_production = sum(.data$production_proxy_w)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -.data$loan_size_by_sector,
      -.data$loan_size_by_sector_w,
      -.data$production_proxy_w
    ) %>%
    dplyr::group_by(!!!old_groups)
}
