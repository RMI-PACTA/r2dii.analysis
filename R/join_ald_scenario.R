#' Join a data-loanbook object to the ald and scenario
#'
#' `join_ald_scenario()` is a simple wrapper of several calls to
#' `dplyr::join_*()`, forming the master dataset to be used in later steps of
#' the analysis.
#'
#' @param data A data frame like the output of
#'   [r2dii.match::prioritize()].
#' @param ald An asset level data frame like [r2dii.data::ald_demo].
#' @param scenario A scenario data frame like [r2dii.data::scenario_demo_2020].
#' @param region_isos A data frame like [r2dii.data::region_isos] (default).
#'
#' @return Returns a fully joined data frame, linking portfolio, ald and
#'   scenario.
#' @export
#'
#' @family utility functions
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
#'   join_ald_scenario(
#'     ald = ald_demo,
#'     scenario = scenario_demo_2020,
#'     region_isos = region_isos_demo
#'   )
join_ald_scenario <- function(data,
                              ald,
                              scenario,
                              region_isos = r2dii.data::region_isos) {
  check_portfolio_ald_scenario(data, ald, scenario)

  # Track provenance to avoid clash in the column name "source"
  region_isos <- region_isos %>%
    rename(scenario_source = .data$source)

  data %>%
    left_join(ald, by = ald_columns()) %>%
    inner_join(scenario, by = scenario_columns()) %>%
    warn_if_has_zero_rows("Joining `scenario` outputs 0 rows.") %>%
    mutate(plant_location = tolower(.data$plant_location)) %>%
    inner_join(
      region_isos,
      by = c("region", "plant_location" = "isos", "scenario_source")
    ) %>%
    warn_if_has_zero_rows("Joining `region_isos` outputs 0 rows.") %>%
    # Return visibly
    identity()
}

warn_if_has_zero_rows <- function(data, message) {
  if (nrow(data) == 0L) warn(message)

  invisible(data)
}

check_portfolio_ald_scenario <- function(valid_matches, ald, scenario) {
  check_crucial_names(valid_matches, names(ald_columns()))
  walk(names(ald_columns()), ~ check_no_value_is_missing(valid_matches, .x))

  check_crucial_names(
    ald, c("name_company", "plant_location", unname(scenario_columns()))
  )
  walk(
    c("name_company", unname(scenario_columns())),
    ~ check_no_value_is_missing(ald, .x)
  )


  check_crucial_names(scenario, c(scenario_columns(), "scenario_source", "region"))
  walk(
    c(scenario_columns(), "scenario_source", "region"),
    ~ check_no_value_is_missing(scenario, .x)
  )

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
