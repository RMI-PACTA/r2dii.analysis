#' Summarize the portfolio-weighted production at portfolio level
#'
#' A simpler wrapper used to calculate the loan-weighted production for each
#' matched company, and aggregate to the portfolio level.
#'
#' @param data A "data.frame" like the output of [join_ald_scenario()].
#' @param use_credit_limit Logical vector of length 1. `FALSE` defaults to
#' using the column `loan_size_outstanding`. Set to `TRUE` to use the column
#' `loan_size_credit_limit` instead.
#' @param ... Variables to group by.
#'
#' @return A tibble with the same groups as the input (if any) and columns:
#'   `sector`, `technology`, `year`, and `weighted_production`.
#'   `weighted_production` results are aggregated to the portfolio-level.
#' @export
#'
#' @examples
#' library(r2dii.data)
#'
#' master <- r2dii.data::loanbook_demo %>%
#'   r2dii.match::match_name(r2dii.data::ald_demo) %>%
#'   r2dii.match::prioritize() %>%
#'   join_ald_scenario(r2dii.data::ald_demo, r2dii.scenario::scenario_demo)
#'
#' summarize_portfolio_production(master)
#'
#' summarize_portfolio_production(master, use_credit_limit = TRUE)
#'
summarize_portfolio_production <- function(data, ..., use_credit_limit = FALSE){

  crucial <- "scenario"

  check_crucial_names(data, crucial)

  data %>%
    summarize_weighted_production(scenario, ...)
}
