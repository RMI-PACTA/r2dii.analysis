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
#' installed <- requireNamespace("r2dii.data", quietly = TRUE) &&
#'   requireNamespace("r2dii.match", quietly = TRUE)
#' if (!installed) stop("Please install r2dii.match and r2dii.data")
#'
#' library(r2dii.data)
#' library(r2dii.match)
#'
#' master <- loanbook_demo %>%
#'   match_name(ald_demo) %>%
#'   prioritize() %>%
#'   join_ald_scenario(ald_demo, scenario_demo_2020)
#'
#' summarize_company_production(master)
#'
#' summarize_company_production(master, use_credit_limit = TRUE)
summarize_company_production <- function(data,
                                         ...,
                                         use_credit_limit = FALSE) {
  crucial <- c(
    "name_ald",
    "scenario",
    "tmsr",
    "smsp"
  )

  check_crucial_names(data, crucial)

  summarize_weighted_production(
    data,
    .data$name_ald,
    .data$scenario,
    .data$tmsr,
    .data$smsp,
    ...,
    use_credit_limit = use_credit_limit
  )
}
