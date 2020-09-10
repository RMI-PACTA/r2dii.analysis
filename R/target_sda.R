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
#' @param by_company Logical vector of length 1. `FALSE` defaults to outputting
#'   `weighted_production_value` at the portfolio-level. Set to `TRUE` to output
#'   `weighted_production_value` at the company-level.
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
#' installed <- requireNamespace("r2dii.match", quietly = TRUE) &&
#'   requireNamespace("r2dii.data", quietly = TRUE)
#' if (!installed) stop("Please install r2dii.match and r2dii.data")
#'
#' library(r2dii.match)
#' library(r2dii.data)
#'
#' # Example datasets from r2dii.data
#' loanbook <- loanbook_demo
#' ald <- ald_demo
#' co2_scenario <- co2_intensity_scenario_demo
#'
#' # WARNING: Remember to validate matches (see `?prioritize`)
#' matched <- prioritize(match_name(loanbook, ald))
#'
#' # You may need to clean your data
#' anyNA(ald$emission_factor)
#' try(target_sda(matched, ald, co2_intensity_scenario = co2_scenario))
#'
#' ald2 <- subset(ald, !is.na(emission_factor))
#' anyNA(ald2$emission_factor)
#'
#' out <- target_sda(matched, ald2, co2_intensity_scenario = co2_scenario)
#'
#' # The output includes the portfolio's actual projected emissions factors, the
#' # scenario pathway emissions factors, and the portfolio's target emissions
#' # factors.
#' out
#'
#' # Split-view by metric
#' split(out, out$emission_factor_metric)
#'
#' # Calculate company-level targets
#' out <- target_sda(
#'   matched, ald2,
#'   co2_intensity_scenario = co2_scenario,
#'   by_company = TRUE
#' )
target_sda <- function(data,
                       ald,
                       co2_intensity_scenario,
                       use_credit_limit = FALSE,
                       by_company = FALSE) {
  stopifnot(
    is.data.frame(data),
    is.data.frame(ald),
    is.data.frame(co2_intensity_scenario),
    is.logical(use_credit_limit),
    is.logical(by_company)
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
    "technology",
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
  check_unique_id(data, "id_loan")
  walk_(crucial_portfolio, ~ check_no_value_is_missing(data, .x))

  check_crucial_names(ald, crucial_ald)

  if (any(is.na(ald$emission_factor))) {
    warn(
      "Removing ald rows where `emission_factor` is NA",
      class = "na_emission_factor"
    )

    ald <- filter(ald, !is.na(.data$emission_factor))
  }

  walk_(crucial_ald, ~ check_no_value_is_missing(ald, .x))

  check_crucial_names(co2_intensity_scenario, crucial_scenario)
  walk_(crucial_scenario, ~ check_no_value_is_missing(co2_intensity_scenario, .x))

  ald_by_sector <- ald %>%
    aggregate_excluding(c("technology", "plant_location", "country_of_domicile"))

  loanbook_with_weighted_emission_factors <- calculate_weighted_emission_factor(
    data,
    ald = ald_by_sector,
    use_credit_limit = use_credit_limit,
    by_company = by_company
  )

  if (identical(nrow(loanbook_with_weighted_emission_factors), 0L)) {
    warn("Found no match between loanbook and ald.", class = "no_match")
    return(empty_target_sda_output())
  }

  corporate_economy <- calculate_market_average(ald_by_sector)

  adjusted_scenario <- compute_ald_adjusted_scenario(
    co2_intensity_scenario,
    corporate_economy
  )

  if (identical(nrow(adjusted_scenario), 0L)) {
    rlang::warn("Found no scenario data for input ald.")
    return(empty_target_sda_output())
  }

  adjusted_scenario_with_p <- add_p_to_scenario(adjusted_scenario)

  target_summary_groups <- maybe_add_name_ald(
    c("sector", "scenario"),
    by_company
  )

  loanbook_targets <- compute_loanbook_targets(
    loanbook_with_weighted_emission_factors,
    adjusted_scenario_with_p,
    !!!rlang::syms(target_summary_groups)
  )

  if (identical(nrow(loanbook_targets), 0L)) {
    rlang::warn("Found no scenario data for the loanbook matches.")
    return(empty_target_sda_output())
  }

  format_and_combine_output(
    loanbook_with_weighted_emission_factors,
    corporate_economy,
    loanbook_targets,
    adjusted_scenario,
    by_company = by_company
  )
}

check_unique_id <- function(data, column) {
  if (sum(duplicated(data[[column]]))) {
    abort(
      class = "unique_ids",
      glue("Column `{column}` must not contain any duplicates.", column)
    )
  }

  invisible(data)
}

aggregate_excluding <- function(ald, columns) {
  .vars <- setdiff(names(ald), c("production", "emission_factor", columns))

  ald %>%
    dplyr::group_by_at(.vars = .vars) %>%
    dplyr::filter(.data$production > 0) %>%
    mutate(weight = .data$production / sum(.data$production)) %>%
    summarize(
      production = sum(.data$production),
      emission_factor =
        sum(.data$emission_factor * .data$weight / sum(.data$weight))
    ) %>%
    ungroup()
}

maybe_add_name_ald <- function(data, by_company = FALSE) {
  out <- data

  if (by_company) {
    out <- c(data, "name_ald")
  }

  return(out)
}

calculate_weighted_emission_factor <- function(data,
                                               ald,
                                               use_credit_limit = FALSE,
                                               by_company = FALSE) {
  summary_groups <- maybe_add_name_ald(c("sector_ald", "year"), by_company)

  data %>%
    inner_join(ald, by = ald_columns()) %>%
    add_weighted_loan_emission_factor(
      use_credit_limit = use_credit_limit
    ) %>%
    group_by(!!!rlang::syms(summary_groups)) %>%
    summarize(
      emission_factor_projected = sum(.data$weighted_loan_emission_factor)
    ) %>%
    ungroup() %>%
    rename(sector = .data$sector_ald)
}

calculate_market_average <- function(data) {
  data %>%
    group_by(.data$sector, .data$year) %>%
    summarize(
      sector_total_production = sum(.data$production),
      # Alias emission_factor_corporate_economy
      .x = list(.data$production * .data$emission_factor)
    ) %>%
    unnest(cols = .data$.x) %>%
    group_by(.data$sector, .data$year) %>%
    summarize(.x = sum(.data$.x / .data$sector_total_production)) %>%
    rename(emission_factor_corporate_economy = .data$.x) %>%
    ungroup()
}

compute_ald_adjusted_scenario <- function(data, corporate_economy) {
  corporate_economy_baseline <- corporate_economy %>%
    group_by(.data$sector) %>%
    filter(.data$year == min(.data$year)) %>%
    select(
      .data$sector,
      baseline_emission_factor = .data$emission_factor_corporate_economy
    ) %>%
    ungroup()

  data %>%
    inner_join(corporate_economy_baseline, by = "sector") %>%
    group_by(.data$scenario, .data$sector) %>%
    arrange(.data$year) %>%
    mutate(
      baseline_adjustment =
        .data$baseline_emission_factor / first(.data$emission_factor),
      emission_factor_adjusted_scenario =
        .data$emission_factor * .data$baseline_adjustment
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
  p <- function(x) (x - last(x)) / (first(x) - last(x))

  data %>%
    group_by(.data$sector, .data$scenario) %>%
    arrange(.data$year) %>%
    mutate(p = p(.data$emission_factor_adjusted_scenario)) %>%
    ungroup()
}

compute_loanbook_targets <- function(data,
                                     scenario_with_p,
                                     ...) {
  data %>%
    inner_join(scenario_with_p, by = c("year", "sector")) %>%
    group_by(...) %>%
    arrange(.data$year) %>%
    mutate(
      d = first(.data$emission_factor_projected) -
        last(.data$emission_factor_adjusted_scenario),
      emission_factor_target = (.data$d * .data$p) +
        last(.data$emission_factor_adjusted_scenario)
    ) %>%
    ungroup() %>%
    select(
      ...,
      .data$year,
      .data$emission_factor_target
    )
}

pivot_emission_factors_longer <- function(data) {
  data %>%
    pivot_longer(
      cols = tidyr::starts_with("emission_factor_"),
      names_prefix = "emission_factor_",
      names_to = "emission_factor_metric",
      values_to = "emission_factor_value"
    )
}

format_and_combine_output <- function(lbk, corporate_economy, targets, scen, by_company = FALSE) {
  projected <- pivot_emission_factors_longer(lbk)

  corporate_economy <- pivot_emission_factors_longer(corporate_economy)

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
    pivot_emission_factors_longer() %>%
    mutate(name_ald = NULL)

  if (by_company) {
    corporate_economy <- corporate_economy %>%
      mutate(name_ald = "market")

    scenario <- scenario %>%
      mutate(name_ald = "market")
  }

  rbind(
    projected,
    corporate_economy,
    targets,
    scenario
  )
}

ald_columns <- function() {
  c(name_ald = "name_company", sector_ald = "sector")
}

empty_target_sda_output <- function() {
  tibble(
    sector = character(0),
    year = integer(0),
    emission_factor_metric = character(0),
    emission_factor_value = numeric(0)
  )
}
