#' Join a matched-loanbook object to the ald and scenario
#'
#'  `join_portfolio_ald_scenario()` is a simple wrapper of several calls to `join()` functions, forming the
#'  master dataset to be used in later steps of the analysis.
#'
#' @param match_result A dataframe like the output of [r2dii.match::prioritize()]
#' @param ald An asset level dataframe like [r2dii.dataraw::ald_demo]
#' @param scenario A scenario dataframe like [r2dii.dataraw::scenario_demo]
#'
#' @return Returns a fully joined dataframe, linking portfolio, ald and scenario.
#' @export
#'
#' @examples
#' library(r2dii.dataraw)
#' library(r2dii.match)
#'
#' matched_loanbook <- match_name(loanbook_demo, ald_demo) %>%
#'   prioritize()
#'
#' join_portfolio_ald_scenario(matched_loanbook,
#'                             ald = ald_demo,
#'                             scenario = scenario_demo)

join_portfolio_ald_scenario <- function(match_result,
                                        ald,
                                        scenario) {

  interesting_columns <- c("id_loan", "loan_size_outstanding", "loan_size_credit_limit", "id_2dii", "level",
                           "sector", "technology", "name", "name_ald", "year",
                           "production","production_unit", "emission_factor", "plant_location",
                           "scenario", "region", "value", "units")

  match_result %>%
    left_join(ald, by = c("name_ald" = "name_company", "sector_ald" = "sector")) %>%
    inner_join(scenario, by = c("sector_ald" = "sector", "technology" = "technology", "year" = "year")) %>%
    select(interesting_columns)

}
