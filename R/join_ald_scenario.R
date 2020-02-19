#' Join a matched-loanbook object to the ald and scenario
#'
#' `join_ald_scenario()` is a simple wrapper of several calls to
#' `join()` functions, forming the master dataset to be used in later steps of
#' the analysis.
#'
#' @param valid_matches A dataframe like the output of
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
#' if (!installed) stop("Please install r2dii.match and r2dii.dataraw")
#'
#' library(r2dii.dataraw)
#' library(r2dii.match)
#'
#' # Example of valid matches after `r2dii.match::match_name()` and manual edits
#' path <- system.file("extdata", "valid_matches.csv", package = "r2dii.analysis")
#' valid_matches <- tibble::as_tibble(read.csv(path, stringsAsFactors = FALSE))
#'
#' valid_matches %>%
#'   join_ald_scenario(
#'     ald = ald_demo,
#'     scenario = add_fair_share_columns(scenario_demo, 2020)
#'   )
join_ald_scenario <- function(valid_matches, ald, scenario) {
  check_portfolio_ald_scenario(valid_matches, ald, scenario)

  valid_matches %>%
    left_join(ald, by = ald_columns()) %>%
    inner_join(scenario, by = scenario_columns()) %>%
    select(suppressWarnings(one_of(interesting_scenario_columns())))
}

check_portfolio_ald_scenario <- function(valid_matches, ald, scenario) {
  check_crucial_names(valid_matches, names(ald_columns()))
  check_crucial_names(ald, c("name_company", unname(scenario_columns())))
  check_crucial_names(scenario, scenario_columns())

  invisible(valid_matches)
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
    "tfsr",
    "mfsp",
    "units"
  )
}
