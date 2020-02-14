#' Join a matched-loanbook object to the ald and scenario
#'
#' `join_portfolio_ald_scenario()` is a simple wrapper of several calls to
#' `join()` functions, forming the master dataset to be used in later steps of
#' the analysis.
#'
#' @param match_result A dataframe like the output of
#'   [r2dii.match::prioritize()].
#' @param ald An asset level dataframe like [r2dii.dataraw::ald_demo].
#' @param scenario A scenario dataframe like [r2dii.dataraw::scenario_demo].
#'
#' @return Returns a fully joined dataframe, linking portfolio, ald and
#'   scenario.
#' @export
#'
#' @examples
#' installed <- requireNamespace("r2dii.dataraw", quietly = TRUE) &&
#'   requireNamespace("r2dii.match", quietly = TRUE)
#' if (installed) {
#'   library(r2dii.dataraw)
#'   library(r2dii.match)
#'
#'   matched_loanbook <- match_name(loanbook_demo, ald_demo) %>%
#'     prioritize()
#'
#'   join_portfolio_ald_scenario(matched_loanbook,
#'     ald = ald_demo,
#'     scenario = scenario_demo
#'   )
#' }
join_portfolio_ald_scenario <- function(match_result, ald, scenario) {
  check_portfolio_ald_scenario(match_result, ald, scenario)

  match_result %>%
    left_join(ald, by = ald_columns()) %>%
    inner_join(scenario, by = scenario_columns()) %>%
    select(suppressWarnings(one_of(interesting_scenario_columns())))
}

check_portfolio_ald_scenario <- function(match_result, ald, scenario) {
  check_crucial_names(match_result, names(ald_columns()))
  check_crucial_names(ald, c("name_company", unname(scenario_columns())))
  check_crucial_names(scenario, scenario_columns())

  invisible(match_result)
}

ald_columns <- function() {
  c(
    name_ald = "name_company",
    sector_ald = "sector"
  )
}

scenario_columns <- function() {
  c(
    sector_ald = "sector",
    technology = "technology",
    year = "year"
  )
}

interesting_scenario_columns <- function() {
  c(
    "id_loan",
    "loan_size_outstanding",
    "loan_size_credit_limit",
    "id_2dii",
    "level",
    scenario_columns(),
    "name",
    "name_ald",
    "production",
    "production_unit",
    "emission_factor",
    "plant_location",
    "scenario",
    "region",
    "value",
    "units"
  )
}
