#' Add portfolio level SDA target
#'
#' This function calculates the portfolio-level emission factor targets, as
#' calculated using the sectoral-decarbonisation approach.
#'
#' @param data A dataframe like the output of
#'   [r2dii.match::prioritize()].
#' @param ald An asset level dataframe like [r2dii.data::ald_demo].
#' @param co2_intensity_scenario A scenario dataframe like [r2dii.analysis::co2_intensity_scenario].
#'
#' @return  A tibble with portfolio-level emission_factor targets
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
#'   add_sda_target(
#'     ald = ald_demo,
#'     co2_intensity_scenario = r2dii.analysis::co2_intensity_scenario
#'   )
add_sda_target <- function(data, ald, co2_intensity_scenario) {

  start_year <- co2_intensity_scenario %>%
    dplyr::select(.data$sector, .data$year) %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::summarise(start_year = min(.data$year))

  ald <- ald %>%
    dplyr::left_join(start_year, by = "sector") %>%
    dplyr::filter(.data$year >= .data$start_year) %>%
    dplyr::filter(!is.na(.data$emission_factor), !is.na(.data$production)) %>%
    dplyr::select(-.data$start_year)


  ald_sda_market_benchmark <- calculate_sda_market_benchmark(ald, co2_intensity_scenario)

  co2_scenario_with_py_and_g <- co2_intensity_scenario %>%
    add_py_and_g_to_scenario()

  target_benchmark_emission_factor <- co2_scenario_with_py_and_g %>%
    calculate_sda_market_benchmark_target(ald_sda_market_benchmark) %>%
    dplyr::select(-.data$emission_factor_unit)

  formatted_co2_intensity <- co2_scenario_with_py_and_g %>%
    dplyr::select(.data$sector, .data$year, .data$emission_factor, .data$py) %>%
    dplyr::rename(scenario_emission_factor = .data$emission_factor)

  loanbook_with_weighted_emission_factors <- data %>%
    calculate_weighted_emission_factor(ald)

  loanbook_with_weighted_emission_factors %>%
    dplyr::full_join(target_benchmark_emission_factor, by = c("year", "sector")) %>%
    dplyr::full_join(formatted_co2_intensity, by = c("year", "sector")) %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(
      initial_portfolio_factor = dplyr::first(.data$weighted_emission_factor),
      d = .data$initial_portfolio_factor - dplyr::last(.data$target_weighted_emission_factor),
      portfolio_target_emission_factor = (.data$d * .data$py) + dplyr::last(.data$scenario_emission_factor)
    ) %>%
    dplyr::select(
      .data$sector,
      .data$year,
      .data$weighted_emission_factor,
      .data$portfolio_target_emission_factor,
      .data$scenario_emission_factor
    ) %>%
    dplyr::filter(!is.na(.data$portfolio_target_emission_factor)) %>%
    tidyr::pivot_longer(
      cols = tidyr::ends_with("factor"),
      names_to = "emission_factor_name",
      values_to = "emission_factor_value"
    ) %>%
    dplyr::filter(!is.na(.data$emission_factor_value))

}

calculate_sda_market_benchmark <- function(market, co2_intensity_scenario) {

  market %>%
    dplyr::group_by(.data$sector, .data$year) %>%
    dplyr::summarise(
      sector_total_production = sum(.data$production),
      production_weighted_emission_factor = .data$production * .data$emission_factor
    ) %>%
    dplyr::group_by(.data$sector, .data$year) %>%
    dplyr::summarise(
      production_weighted_emission_factor =
        sum(.data$production_weighted_emission_factor / .data$sector_total_production)
    )

}

add_py_and_g_to_scenario <- function(co2_intensity_scenario) {
  co2_intensity_scenario %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(
      g = .data$emission_factor / dplyr::first(.data$emission_factor),
      py = (.data$emission_factor - dplyr::last(.data$emission_factor)) / (dplyr::first(.data$emission_factor) - dplyr::last(.data$emission_factor))
    )
}

calculate_sda_market_benchmark_target <- function(co2_intensity_scenario_with_py_and_g,
                                                  ald_sda_market_benchmark) {
  co2_intensity_scenario_with_py_and_g %>%
    dplyr::filter(dplyr::row_number() == 1L | dplyr::row_number() == dplyr::n()) %>%
    dplyr::select(-.data$emission_factor, -.data$py) %>%
    dplyr::left_join(ald_sda_market_benchmark, by = c("sector", "year")) %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(target_weighted_emission_factor = dplyr::first(.data$production_weighted_emission_factor) * .data$g) %>%
    dplyr::select(.data$sector, .data$year, .data$emission_factor_unit, .data$target_weighted_emission_factor)
}

add_weighted_loan_emission_factor <- function(data, use_credit_limit = FALSE) {

  loan_size <- paste0(
    "loan_size_", ifelse(use_credit_limit, "credit_limit", "outstanding")
  )

  distinct_loans_by_sector <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::distinct(.data$id_loan, .data[[loan_size]])

  total_size_by_sector <- distinct_loans_by_sector %>%
    dplyr::summarize(total_size = sum(.data[[loan_size]]))

  data %>%
    dplyr::left_join(total_size_by_sector, by = "sector") %>%
    dplyr::mutate(
      loan_weight = .data[[loan_size]] / .data$total_size,
      weighted_loan_emission_factor = .data$emission_factor * .data$loan_weight
    )
}

summarize_weighted_emission_factor <- function(data, ..., use_credit_limit = FALSE) {
  data %>%
    add_weighted_loan_emission_factor(use_credit_limit = use_credit_limit) %>%
    dplyr::group_by(.data$sector, .data$year, ...) %>%
    dplyr::summarize(
      weighted_emission_factor = sum(.data$weighted_loan_emission_factor)
    )
}

calculate_weighted_emission_factor <- function(data, ald) {
  data %>%
    inner_join(ald, by = ald_columns()) %>%
    summarize_weighted_emission_factor()
}

ald_columns <- function() {
  c(
    name_ald = "name_company",
    sector_ald = "sector"
  )
}
