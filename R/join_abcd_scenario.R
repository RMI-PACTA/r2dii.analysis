#' Join a data-loanbook object to the abcd and scenario
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated because it is not required as a user-facing
#' function for PACTA for Banks. It is still used internally though.
#' All relevant outputs of the PACTA for Banks analysis can be obtained using
#' the target_market_share() and target_sda() functions.
#'
#' `join_abcd_scenario()` is a simple wrapper of several calls to
#' `dplyr::join_*()`, forming the master dataset to be used in later steps of
#' the analysis.
#'
#' @keywords internal
#'
#' @param data A data frame like the output of
#'   `r2dii.match::prioritize`.
#' @param abcd An asset level data frame like [r2dii.data::abcd_demo].
#' @param scenario A scenario data frame like [r2dii.data::scenario_demo_2020].
#' @param region_isos A data frame like [r2dii.data::region_isos] (default).
#' @param add_green_technologies Logical vector of length 1. `FALSE` defaults to
#' outputting only technologies that are present in both `data` and `abcd`. Set
#' to `FALSE` to add rows of all possible green technologies (with 0
#' production).
#'
#' @return Returns a fully joined data frame, linking portfolio, abcd and
#'   scenario.
#' @export
#'
#' @family utility functions
#'
#' @examplesIf rlang::is_installed("r2dii.data") && rlang::is_installed("r2dii.match", version = "0.1.0")
#' library(r2dii.data)
#' library(r2dii.match)
#'
#' valid_matches <- match_name(loanbook_demo, abcd_demo) %>%
#' # WARNING: Remember to validate matches (see `?prioritize`)
#'   prioritize()
#'
#' valid_matches %>%
#'   join_abcd_scenario(
#'   abcd = abcd_demo,
#'   scenario = scenario_demo_2020,
#'   region_isos = region_isos_demo
#'   )
join_abcd_scenario <- function(data,
                              abcd,
                              scenario,
                              region_isos = r2dii.data::region_isos,
                              add_green_technologies = FALSE) {
  lifecycle::deprecate_soft(when = "0.5.0", what = "join_abcd_scenario()")

  check_portfolio_abcd_scenario(data, abcd, scenario)

  # Track provenance to avoid clash in the column name "source"
  region_isos <- region_isos %>%
    rename(scenario_source = "source")

  abcd <- modify_at_(abcd, "sector", tolower)
  abcd <- modify_at_(abcd, "technology", tolower)

  if (add_green_technologies) {
    abcd <- add_green_technologies_to_abcd(abcd, scenario)
  }

  abcd <- abcd %>%
    arrange(.data[["year"]]) %>%
    mutate(
      .start_year_abcd = first(.data[["year"]]),
      .by = c("name_company", "sector", "plant_location")
      )

  scenario <- scenario %>%
    arrange(.data[["year"]]) %>%
    mutate(
      .start_year_scenario = first(.data[["year"]]),
      .by = c("sector")
    )

  abcd_wide <- tidyr::pivot_wider(
    abcd,
    names_from = "year",
    names_sep = "~",
    values_from = c("production", "emission_factor")
  )

  scenario_wide <- tidyr::pivot_wider(
    scenario,
    names_from = "year",
    names_sep = "~",
    values_from = c("tmsr", "smsp")
  )

  data <- data %>%
    left_join(abcd_wide, by = abcd_columns(), relationship = "many-to-many") %>%
    left_join(scenario_wide, by = scenario_columns(), relationship = "many-to-many") %>%
    warn_if_has_zero_rows("Joining `scenario` outputs 0 rows.")

  data <- tidyr::pivot_longer(
    data,
    cols = tidyr::contains("~"),
    names_sep = "~",
    names_to = c("metric", "year")
  )

  data <- dplyr::mutate(data, year = as.integer(.data[["year"]]))

  data <- tidyr::pivot_wider(
    data,
    names_from = "metric",
    values_from = "value"
  )

  data <- data %>%
    dplyr::filter(.data[["year"]] >= .data[[".start_year_abcd"]]) %>%
    dplyr::filter(.data[["year"]] >= .data[[".start_year_scenario"]]) %>%
    dplyr::mutate(
      .start_year_abcd = NULL,
      .start_year_scenario = NULL
      )

  out <- data %>%
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

check_portfolio_abcd_scenario <- function(valid_matches, abcd, scenario) {
  check_crucial_names(valid_matches, names(abcd_columns()))
  walk_(names(abcd_columns()), ~ check_no_value_is_missing(valid_matches, .x))

  check_crucial_names(
    abcd,
    c("name_company", "plant_location", "year", unname(scenario_columns()))
  )
  walk_(
    c("name_company", "year", unname(scenario_columns())),
    ~ check_no_value_is_missing(abcd, .x)
  )


  check_crucial_names(
    scenario,
    c(scenario_columns(), "scenario_source", "region", "year")
    )
  walk_(
    c(scenario_columns(), "scenario_source", "region", "year"),
    ~ check_no_value_is_missing(scenario, .x)
  )

  invisible(valid_matches)
}

add_green_technologies_to_abcd <- function(data, scenario) {
  increasing_techs <- r2dii.data::increasing_or_decreasing %>%
    filter(.data$increasing_or_decreasing == "increasing") %>%
    select(-all_of("increasing_or_decreasing"))

  increasing_techs_in_scenario <- scenario %>%
    select(all_of(c("sector", "technology"))) %>%
    unique() %>%
    inner_join(increasing_techs, by = c("sector", "technology"))

  increasing_techs_not_in_abcd <- dplyr::anti_join(
    increasing_techs_in_scenario,
    data,
    by = c("sector", "technology")
  )

  green_rows_to_add <- data %>%
    group_by(
      .data$name_company,
      .data$sector,
      .data$year,
      .data$plant_location,
      .data$is_ultimate_owner
    ) %>%
    summarize() %>%
    left_join(
      increasing_techs_not_in_abcd,
      by = "sector",
      relationship = "many-to-many"
      ) %>%
    mutate(production = 0)

  dplyr::bind_rows(data, green_rows_to_add)
}

abcd_columns <- function() {
  c(
    name_abcd = "name_company",
    sector_abcd = "sector"
  )
}

scenario_columns <- function() {
  c(
    sector_abcd = "sector",
    technology = "technology"
  )
}
