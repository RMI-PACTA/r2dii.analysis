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
#' master <- tibble::tibble(
#'  id_loan = c("i1", "i2", "i1", "i2"),
#'  loan_size_outstanding = c(40, 10, 40, 10),
#'  loan_size_credit_limit = c(2, 2, 2, 2),
#'  sector = c("automotive", "automotive","automotive","automotive"),
#'  name_ald = c("shaanxi auto","shaanxi auto","shaanxi auto", "shaanxi auto"),
#'  technology = c("ta", "ta", "tb", "tb"),
#'  year = c(2025, 2025, 2025, 2025),
#'  production = c(10, 30, 20, 40),
#'  plant_location = c("BF", "BF", "BF", "BF"),
#'  scenario = c("sds", "sds", "sds", "sds"),
#'  region = c("global", "global", "global", "global"),
#'  tmsr = c(0.5, 0.5, 0.5, 0.5),
#'  smsp = c(-0.08, -0.08, -0.08, -0.08)
#' )
#' master
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
