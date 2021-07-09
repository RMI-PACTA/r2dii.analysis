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
#' @param add_green_technologies Logical vector of length 1. `FALSE` defaults to
#' outputting only technologies that are present in both `data` and `ald`. Set
#' to `FALSE` to add rows of all possible green technologies (with 0
#' production).
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
                              region_isos = r2dii.data::region_isos,
                              add_green_technologies = FALSE) {
  check_portfolio_ald_scenario(data, ald, scenario)

  # Track provenance to avoid clash in the column name "source"
  region_isos <- region_isos %>%
    rename(scenario_source = .data$source)

  ald <- modify_at_(ald, "sector", tolower)
  ald <- modify_at_(ald, "technology", tolower)

  if (add_green_technologies) {
    ald <- add_green_technologies_to_ald(ald, scenario)
  }

  out <- data %>%
    left_join(ald, by = ald_columns()) %>%
    inner_join(scenario, by = scenario_columns()) %>%
    warn_if_has_zero_rows("Joining `scenario` outputs 0 rows.") %>%
    mutate(plant_location = tolower(.data$plant_location)) %>%
    inner_join(
      region_isos,
      by = c("region", "plant_location" = "isos", "scenario_source")
    ) %>%
    warn_if_has_zero_rows("Joining `region_isos` outputs 0 rows.")
  out
}

warn_if_has_zero_rows <- function(data, message) {
  if (nrow(data) == 0L) warn(message = message, class = "has_zero_rows")
  invisible(data)
}

check_portfolio_ald_scenario <- function(valid_matches, ald, scenario) {
  check_crucial_names(valid_matches, names(ald_columns()))
  walk_(names(ald_columns()), ~ check_no_value_is_missing(valid_matches, .x))

  check_crucial_names(
    ald, c("name_company", "plant_location", unname(scenario_columns()))
  )
  walk_(
    c("name_company", unname(scenario_columns())),
    ~ check_no_value_is_missing(ald, .x)
  )


  check_crucial_names(scenario, c(scenario_columns(), "scenario_source", "region"))
  walk_(
    c(scenario_columns(), "scenario_source", "region"),
    ~ check_no_value_is_missing(scenario, .x)
  )

  invisible(valid_matches)
}

add_green_technologies_to_ald <- function(data, scenario) {
  green_techs <- r2dii.data::green_or_brown %>%
    filter(.data$green_or_brown == "green") %>%
    select(-.data$green_or_brown)

  green_techs_in_scenario <- scenario %>%
    select(.data$sector, .data$technology) %>%
    unique() %>%
    inner_join(green_techs, by = c("sector", "technology"))

  green_rows_to_add <- data %>%
    group_by(
      .data$name_company,
      .data$sector,
      .data$year,
      .data$plant_location,
      .data$is_ultimate_owner
    ) %>%
    summarize() %>%
    left_join(green_techs_in_scenario, by = "sector") %>%
    mutate(production = 0)

  dplyr::bind_rows(data, green_rows_to_add)
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
