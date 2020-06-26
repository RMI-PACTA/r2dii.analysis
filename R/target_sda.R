#' Add targets for CO2 emissions per unit production at the portfolio level,
#' using the SDA approach
#'
#' This function calculates targets of CO2 emissions per unit production at the
#' portfolio-level, otherwise referred to as "emissions factors". It uses the
#' [sectoral-decarbonization approach
#' (SDA)](https://2degreesinvesting.github.io/r2dii.analysis/articles/sda-target.html)
#' to calculate these targets.
#'
#' @template ignores-existing-groups
#'
#' @param data A dataframe like the output of
#'   [r2dii.match::prioritize()].
#' @param ald An asset-level data frame like [r2dii.data::ald_demo].
#' @param co2_intensity_scenario A scenario data frame like
#'   [r2dii.data::co2_intensity_scenario_demo].
#' @param use_credit_limit Logical vector of length 1. `FALSE` defaults to using
#'   the column `loan_size_outstanding`. Set to `TRUE` to instead use the column
#'   `loan_size_credit_limit`.
#'
#' @return  A tibble with the CO2 emissions factors attributed to
#' the portfolio. These values include the portfolio's actual projected CO2
#' emissions factors, the scenario pathway CO2 emissions factors and the SDA
#' calculated portfolio target emissions factors (see column
#' `emission_factor_metric`).
#'
#' @export
#'
#' @family functions to calculate scenario targets
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
#' out <- valid_matches %>%
#'   target_sda(
#'     ald = ald_demo,
#'     co2_intensity_scenario = co2_intensity_scenario_demo
#'   )
#'
#' # The output includes the portfolio's actual projected emissions factors, the
#' # scenario pathway emissions factors, and the portfolio's target emissions
#' # factors.
#' out
#'
#' # Split view by metric
#' split(out, out$emission_factor_metric)
target_sda <- function(data,
                       ald,
                       co2_intensity_scenario,
                       use_credit_limit = FALSE) {
  stopifnot(
    is.data.frame(data),
    is.data.frame(ald),
    is.data.frame(co2_intensity_scenario),
    is.logical(use_credit_limit)
  )

  data <- ungroup(warn_grouped(data, "Ungrouping input data."))

  crucial_portfolio <- c(
    "loan_size_outstanding",
    "loan_size_credit_limit",
    "name_ald",
    "sector_ald"
  )

  crucial_ald <- c(
    "name_company",
    "sector",
    "year",
    "emission_factor",
    "production"
  )

  crucial_scenario <- c(
    "sector",
    "year",
    "emission_factor"
  )

  check_crucial_names(data, crucial_portfolio)
  check_crucial_names(ald, crucial_ald)
  check_crucial_names(co2_intensity_scenario, crucial_scenario)

  start_year <- co2_intensity_scenario %>%
    select(.data$sector, .data$year) %>%
    group_by(.data$sector) %>%
    summarize(start_year = min(.data$year))

  ald <- ald %>%
    left_join(start_year, by = "sector") %>%
    filter(.data$year >= .data$start_year) %>%
    filter(!is.na(.data$emission_factor), !is.na(.data$production)) %>%
    select(-.data$start_year)

  ald_market_average <- calculate_market_average(ald)

  co2_scenario_with_py_and_g <- add_py_and_g_to_scenario(co2_intensity_scenario)

  target_benchmark_emission_factor <- co2_scenario_with_py_and_g %>%
    add_sda_market_benchmark_target(ald_market_average)

  formatted_co2_intensity <- co2_scenario_with_py_and_g %>%
    select(.data$sector, .data$year, .data$emission_factor, .data$py) %>%
    rename(emission_factor_scenario_benchmark = .data$emission_factor)

  loanbook_with_weighted_emission_factors <- data %>%
    calculate_weighted_emission_factor(ald, use_credit_limit = use_credit_limit)

  loanbook_with_weighted_emission_factors %>%
    full_join(
      target_benchmark_emission_factor,
      by = c("year", "sector")
    ) %>%
    full_join(formatted_co2_intensity, by = c("year", "sector")) %>%
    group_by(.data$sector) %>%
    arrange(.data$year) %>%
    mutate(
      initial_portfolio_factor = first(.data$emission_factor_projected),
      d = .data$initial_portfolio_factor -
        last(.data$target_weighted_emission_factor),
      emission_factor_target = (.data$d * .data$py) +
        last(.data$emission_factor_scenario_benchmark)
    ) %>%
    select(
      .data$sector,
      .data$year,
      .data$emission_factor_projected,
      .data$emission_factor_target,
      .data$emission_factor_scenario_benchmark
    ) %>%
    filter(!is.na(.data$emission_factor_target)) %>%
    tidyr::pivot_longer(
      cols = tidyr::starts_with("emission_factor_"),
      names_prefix = "emission_factor_",
      names_to = "emission_factor_metric",
      values_to = "emission_factor_value"
    ) %>%
    filter(!is.na(.data$emission_factor_value)) %>%
    ungroup()
}

calculate_market_average <- function(market) {
  market %>%
    group_by(.data$sector, .data$year) %>%
    summarize(
      sector_total_production = sum(.data$production),
      # Alias production_weighted_emission_factor
      .x = list(.data$production * .data$emission_factor)
    ) %>%
    tidyr::unnest(cols = .data$.x) %>%
    group_by(.data$sector, .data$year) %>%
    summarize(.x = sum(.data$.x / .data$sector_total_production)) %>%
    rename(production_weighted_emission_factor = .data$.x)
}

add_py_and_g_to_scenario <- function(co2_intensity_scenario) {
  co2_intensity_scenario %>%
    group_by(.data$sector) %>%
    arrange(.data$year) %>%
    mutate(
    # styler: off
      .x =  .data$emission_factor,  # For short so the pattern is clearer
      g =   .data$.x / first(.data$.x),
      py = (.data$.x -  last(.data$.x)) / (first(.data$.x) - last(.data$.x)),
      .x = NULL
      # styler: on
    )
}

add_sda_market_benchmark_target <- function(co2_intensity_scenario_with_py_and_g,
                                            ald_sda_market_benchmark) {
  co2_intensity_scenario_with_py_and_g %>%
    filter(row_number() == 1L | row_number() == dplyr::n()) %>%
    select(-.data$emission_factor, -.data$py) %>%
    left_join(ald_sda_market_benchmark, by = c("sector", "year")) %>%
    group_by(.data$sector) %>%
    arrange(.data$year) %>%
    mutate(
      target_weighted_emission_factor =
        first(.data$production_weighted_emission_factor) * .data$g
    ) %>%
    select(
      .data$sector,
      .data$year,
      .data$emission_factor_unit,
      .data$target_weighted_emission_factor
    )
}

calculate_weighted_emission_factor <- function(data, ald, use_credit_limit) {
  data %>%
    inner_join(ald, by = ald_columns()) %>%
    add_loan_weighted_emission_factor(use_credit_limit = use_credit_limit) %>%
    group_by(.data$sector_ald, .data$year) %>%
    summarize(
      emission_factor_projected = sum(.data$weighted_loan_emission_factor)
    ) %>%
    rename(sector = .data$sector_ald)
}

add_loan_weighted_emission_factor <- function(data, use_credit_limit) {
  loan_size <- paste0(
    "loan_size_", ifelse(use_credit_limit, "credit_limit", "outstanding")
  )

  distinct_loans_by_sector <- data %>%
    ungroup() %>%
    group_by(.data$sector_ald) %>%
    distinct(.data$id_loan, .data[[loan_size]])

  total_size_by_sector <- distinct_loans_by_sector %>%
    summarize(total_size = sum(.data[[loan_size]]))

  data %>%
    left_join(total_size_by_sector, by = "sector_ald") %>%
    mutate(
      loan_weight = .data[[loan_size]] / .data$total_size,
      weighted_loan_emission_factor = .data$emission_factor * .data$loan_weight
    )
}

ald_columns <- function() {
  c(
    name_ald = "name_company",
    sector_ald = "sector"
  )
}
