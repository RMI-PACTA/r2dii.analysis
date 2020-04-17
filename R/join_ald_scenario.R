#' Join a data-loanbook object to the ald and scenario
#'
#' `join_ald_scenario()` is a simple wrapper of several calls to
#' `join()` functions, forming the master dataset to be used in later steps of
#' the analysis.
#'
#' @param data A dataframe like the output of
#'   [r2dii.match::prioritize()].
#' @param ald An asset level dataframe like [r2dii.data::ald_demo].
#' @param scenario A scenario dataframe like [r2dii.data::scenario_demo_2020].
#'
#' @return Returns a fully joined dataframe, linking portfolio, ald and
#'   scenario.
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
#' valid_matches <- match_name(loanbook_demo, ald_demo) %>%
#'   # WARNING: Remember to validate matches (see `?prioritize`)
#'   prioritize()
#'
#' valid_matches %>%
#'   join_ald_scenario(ald = ald_demo, scenario = r2dii.data::scenario_demo_2020)
join_ald_scenario <- function(data, ald, scenario) {
  check_portfolio_ald_scenario(data, ald, scenario)

  data %>%
    left_join(ald, by = ald_columns()) %>%
    inner_join(scenario, by = scenario_columns()) %>%
    pick_plant_location_in_region()
}

check_portfolio_ald_scenario <- function(valid_matches, ald, scenario) {
  check_crucial_names(valid_matches, names(ald_columns()))
  check_crucial_names(ald, c("name_company", "plant_location", unname(scenario_columns())))
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

pick_plant_location_in_region <- function(data) {
  dplyr::inner_join(data, r2dii.data::region_isos,
    by = c("region", "plant_location" = "isos")
  )
}
