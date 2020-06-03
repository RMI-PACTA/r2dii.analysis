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
#' @family functions to weight production
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
#' summarize_portfolio_production(master)
#'
#' summarize_portfolio_production(master, use_credit_limit = TRUE)
summarize_portfolio_production <- function(data,
                                           ...,
                                           use_credit_limit = FALSE) {
  crucial <- portfolio_groups()
  check_crucial_names(data, crucial)

  summarize_weighted_production(
    data,
    !!!rlang::syms(crucial), ...,
    use_credit_limit = use_credit_limit
  )
}

portfolio_groups<- function(){
  c("scenario", "tmsr", "smsp", "region")
}
