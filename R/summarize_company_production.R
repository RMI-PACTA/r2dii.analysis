#' Summarize the portfolio-weighted production at company level
#'
#' A simpler wrapper used to calculate the loan-weighted production for each
#' matched company, and aggregate to the company level.
#'
#' @inherit summarize_weighted_production
#' @inheritDotParams summarize_weighted_production
#'
#' @export
#'
#' @examples
#' library(r2dii.data)
#'
#' master <- r2dii.data::loanbook_demo %>%
#'   r2dii.match::match_name(r2dii.data::ald_demo) %>%
#'   r2dii.match::prioritize() %>%
#'   join_ald_scenario(r2dii.data::ald_demo, r2dii.scenario::scenario_demo_2020)
#'
#' summarize_company_production(master)
#'
#' summarize_company_production(master, use_credit_limit = TRUE)
summarize_company_production <- function(data,
                                         ...,
                                         use_credit_limit = FALSE) {
  crucial <- c(
    "name",
    "scenario"
  )

  check_crucial_names(data, crucial)

  summarize_weighted_production(
    data,
    .data$name, .data$scenario, ...,
    use_credit_limit = use_credit_limit
    )
}
