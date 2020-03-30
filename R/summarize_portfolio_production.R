#' Summarize the portfolio-weighted production at portfolio level
#'
#' A simpler wrapper used to calculate the loan-weighted production for each
#' matched company, and aggregate to the portfolio level.
#'
#' @inherit summarize_weighted_production
#' @inheritDotParams summarize_weighted_production
#'
#' @export
#'
#' @examples
#' # FIXME: Revert when scenario_demo_2020 is available
#' # library(r2dii.data)
#' #
#' # master <- r2dii.data::loanbook_demo %>%
#' #   r2dii.match::match_name(r2dii.data::ald_demo) %>%
#' #   r2dii.match::prioritize() %>%
#' #   join_ald_scenario(r2dii.data::ald_demo, r2dii.scenario::scenario_demo_2020)
#' #
#' # summarize_portfolio_production(master)
#' #
#' # summarize_portfolio_production(master, use_credit_limit = TRUE)
summarize_portfolio_production <- function(data,
                                           ...,
                                           use_credit_limit = FALSE) {
  crucial <- c(
    "scenario",
    "tmsr",
    "smsp"
  )

  check_crucial_names(data, crucial)

  summarize_weighted_production(
    data,
    .data$scenario,
    .data$tmsr,
    .data$smsp,
    ...,
    use_credit_limit = use_credit_limit
  )
}
