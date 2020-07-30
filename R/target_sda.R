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

  loanbook_with_weighted_emission_factors <- data %>%
    calculate_weighted_emission_factor(ald, use_credit_limit = use_credit_limit)

  if (identical(nrow(loanbook_with_weighted_emission_factors), 0L)) {
    rlang::warn("Found no match between loanbook and ald.")
    return(empty_target_sda_output())
  }

  corporate_economy <- calculate_market_average(ald)

  adjusted_scenario <- compute_ald_adjusted_scenario(
    co2_intensity_scenario,
    corporate_economy
    )

  if (identical(nrow(adjusted_scenario), 0L)) {
    rlang::warn("Found no scenario data for input ald.")
    return(empty_target_sda_output())
  }

  adjusted_scenario_with_p <- add_p_to_scenario(adjusted_scenario)

  loanbook_targets <- compute_loanbook_targets(
    loanbook_with_weighted_emission_factors,
    adjusted_scenario_with_p
  )

  if (identical(nrow(loanbook_targets), 0L)) {
    rlang::warn("Found no scenario data for the loanbook matches.")
    return(empty_target_sda_output())
  }

  format_and_combine_output(
    loanbook_with_weighted_emission_factors,
    corporate_economy,
    loanbook_targets,
    adjusted_scenario
    )
}

format_and_combine_output <- function(lbk, corp_economy, targets, scen){

  projected <- lbk %>%
    pivot_emission_factors_longer()

  corporate_economy <- corp_economy %>%
    pivot_emission_factors_longer()

  targets <- targets %>%
    pivot_wider(
      names_from = .data$scenario,
      names_prefix = "emission_factor_target_",
      values_from = .data$emission_factor_target
    ) %>%
    pivot_emission_factors_longer()

  scenario <- scen %>%
    pivot_wider(
      names_from = .data$scenario,
      names_prefix = "emission_factor_adjusted_scenario_",
      values_from = .data$emission_factor_adjusted_scenario
    ) %>%
    pivot_emission_factors_longer()

  rbind(
    projected,
    corporate_economy,
    targets,
    scenario
    )
}

#TODO: maybe extract these to `summarize_weighted_production`
calculate_weighted_emission_factor <- function(data, ald, use_credit_limit) {
  data %>%
    inner_join(ald, by = ald_columns()) %>%
    add_loan_weighted_emission_factor(use_credit_limit = use_credit_limit) %>%
    group_by(.data$sector_ald, .data$year) %>%
    summarize(
      emission_factor_projected = sum(.data$weighted_loan_emission_factor)
    ) %>%
    ungroup() %>%
    rename(sector = .data$sector_ald)
}

#TODO: maybe extract these to `summarize_weighted_production`
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

calculate_market_average <- function(data) {
  data %>%
    group_by(.data$sector, .data$year) %>%
    summarize(
      sector_total_production = sum(.data$production),
      # Alias emission_factor_corporate_economy
      .x = list(.data$production * .data$emission_factor)
    ) %>%
    tidyr::unnest(cols = .data$.x) %>%
    group_by(.data$sector, .data$year) %>%
    summarize(.x = sum(.data$.x / .data$sector_total_production)) %>%
    rename(emission_factor_corporate_economy = .data$.x) %>%
    ungroup()
}

compute_ald_adjusted_scenario <- function(data, corporate_economy){
  corporate_economy_baseline <- corporate_economy %>%
    group_by(.data$sector) %>%
    filter(.data$year == min(.data$year)) %>%
    select(.data$sector,
           baseline_emission_factor = .data$emission_factor_corporate_economy) %>%
    ungroup()

  data %>%
    inner_join(corporate_economy_baseline, by = "sector") %>%
    group_by(.data$scenario, .data$sector) %>%
    arrange(.data$year) %>%
    mutate(
      baseline_adjustment = .data$baseline_emission_factor / first(.data$emission_factor),
      emission_factor_adjusted_scenario = .data$emission_factor * .data$baseline_adjustment
    ) %>%
    ungroup() %>%
    select(
      .data$scenario,
      .data$sector,
      .data$year,
      .data$emission_factor_adjusted_scenario
    )
}

add_p_to_scenario <- function(data) {
  data %>%
    group_by(.data$sector, .data$scenario) %>%
    arrange(.data$year) %>%
    mutate(
    # styler: off
      .x =  .data$emission_factor_adjusted_scenario,  # For short so the pattern is clearer
      p = (.data$.x -  last(.data$.x)) / (first(.data$.x) - last(.data$.x)),
      .x = NULL
      # styler: on
    ) %>%
    ungroup()
}

compute_loanbook_targets <- function(data,scenario_with_p){
  data %>%
    inner_join(scenario_with_p, by = c("year", "sector")) %>%
    group_by(.data$sector, .data$scenario) %>%
    arrange(.data$year) %>%
    mutate(
      d = first(.data$emission_factor_projected) -
        last(.data$emission_factor_adjusted_scenario),
      emission_factor_target = (.data$d * .data$p) +
        last(.data$emission_factor_adjusted_scenario)
    ) %>%
    ungroup() %>%
    select(
      .data$sector,
      .data$scenario,
      .data$year,
      .data$emission_factor_target
    )

}

pivot_emission_factors_longer <- function(data){
  data %>%
    pivot_longer(
      cols = tidyr::starts_with("emission_factor_"),
      names_prefix = "emission_factor_",
      names_to = "emission_factor_metric",
      values_to = "emission_factor_value"
    )

}

ald_columns <- function() {
  c(
    name_ald = "name_company",
    sector_ald = "sector"
  )
}

empty_target_sda_output <- function() {
  tibble(
    sector = character(),
    year = integer(),
    emission_factor_metric = character(),
    emission_factor_value = numeric()
  )

}
