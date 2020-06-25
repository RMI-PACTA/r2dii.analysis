#' Add targets for production, using the market share approach
#'
#' This function calculates the portfolio-level production targets, as
#' calculated using the market share approach applied to each relevant climate
#' production forecast.
#'
#' @template ignores-existing-groups
#'
#' @param data A "data.frame" like the output of
#'   [r2dii.match::prioritize()].
#' @param ald An asset level data frame like [r2dii.data::ald_demo].
#' @param scenario A scenario data frame like [r2dii.data::scenario_demo_2020].
#' @param region_isos A data frame like [r2dii.data::region_isos] (default).
#' @param use_credit_limit Logical vector of length 1. `FALSE` defaults to using
#'   the column `loan_size_outstanding`. Set to `TRUE` to use the column
#'   `loan_size_credit_limit` instead.
#' @param by_company Logical vector of length 1. `FALSE` defaults to outputting
#' `weighted_production_value` at the portfolio-level. Set to `TRUE` to output
#' `weighted_production_value` at the company-level.
#'
#' @return A tibble with the summarized columns `weighted_production_metric`
#' and `weighted_production_value`. If `by_company = TRUE`, the output will also
#' have the column `name_ald`.
#' @export
#'
#' @family functions to calculate scenario targets
#'
#' @examples
#' library(r2dii.analysis)
#' library(r2dii.data)
#' library(r2dii.match)
#'
#' match_result <- r2dii.data::loanbook_demo %>%
#'   r2dii.match::match_name(r2dii.data::ald_demo) %>%
#'   r2dii.match::prioritize()
#'
#' # calculate targets at portfolio level
#' target_market_share(match_result,
#'   ald = r2dii.data::ald_demo,
#'   scenario = r2dii.data::scenario_demo_2020,
#'   region_isos = r2dii.data::region_isos_demo
#' )
#'
#' # calculate targets at company level
#' target_market_share(match_result,
#'   ald = r2dii.data::ald_demo,
#'   scenario = r2dii.data::scenario_demo_2020,
#'   region_isos = r2dii.data::region_isos_demo,
#'   by_company = TRUE
#' )
#'
target_market_share <- function(data,
                                ald,
                                scenario,
                                region_isos = r2dii.data::region_isos,
                                use_credit_limit = FALSE,
                                by_company = FALSE) {
  stopifnot(is.data.frame(data))

  data <- ungroup(warn_grouped(data, "Ungrouping input data."))

  crucial_scenario <- c("scenario", "tmsr", "smsp")
  check_crucial_names(scenario, crucial_scenario)
  check_crucial_names(ald, "is_ultimate_owner")
  walk(crucial_scenario, ~ check_no_value_is_missing(scenario, .x))

  summary_groups <- maybe_add_name_ald(
    c("scenario", "tmsr", "smsp", "region", "scenario_source"),
    by_company
  )

  data <- data %>%
    join_ald_scenario(
      ald,
      scenario,
      region_isos
    ) %>%
    summarize_weighted_production(
      !!!rlang::syms(summary_groups),
      use_credit_limit = use_credit_limit
    ) %>%
    add_ald_benchmark(ald, region_isos, by_company)

  target_groups <- c("sector", "scenario", "year", "region")

  initial_sector_summaries <- data %>%
    maybe_group_by_name_ald(target_groups,
      by_company = by_company
    ) %>%
    summarize(
      sector_weighted_production = sum(.data$weighted_production)
    ) %>%
    arrange(.data$year) %>%
    maybe_group_by_name_ald(c("sector", "scenario", "region"),
      by_company = by_company
    ) %>%
    filter(row_number() == 1L) %>%
    rename(
      initial_sector_production = .data$sector_weighted_production
    ) %>%
    select(-.data$year)

  initial_technology_summaries <- data %>%
    maybe_group_by_name_ald(c(target_groups, "technology"),
      by_company = by_company
    ) %>%
    summarize(
      technology_weighted_production = sum(.data$weighted_production)
    ) %>%
    arrange(.data$year) %>%
    maybe_group_by_name_ald(c("sector", "technology", "scenario", "region"),
      by_company = by_company
    ) %>%
    filter(row_number() == 1L) %>%
    rename(
      initial_technology_production = .data$technology_weighted_production
    ) %>%
    select(-.data$year)

  data %>%
    left_join(
      initial_sector_summaries,
      by = maybe_add_name_ald(c("sector", "scenario", "region"),
        by_company = by_company
      )
    ) %>%
    left_join(
      initial_technology_summaries,
      by = maybe_add_name_ald(c("sector", "scenario", "region", "technology"),
        by_company = by_company
      )
    ) %>%
    mutate(
      tmsr_target_weighted_production = .data$initial_technology_production *
        .data$tmsr,
      smsp_target_weighted_production = .data$initial_technology_production +
        (.data$initial_sector_production * .data$smsp)
    ) %>%
    select(
      -c(
        .data$tmsr,
        .data$smsp,
        .data$initial_technology_production,
        .data$initial_sector_production
      )
    ) %>%
    tidyr::pivot_longer(
      cols = c("tmsr_target_weighted_production", "smsp_target_weighted_production"),
      names_to = "target_name",
      values_to = "scenario_target_value"
    ) %>%
    inner_join(pick_tmsr_or_smsp, by = c(
      sector = "sector",
      technology = "technology",
      target_name = "which_metric"
    )) %>%
    select(-.data$target_name) %>%
    tidyr::pivot_wider(
      names_from = .data$scenario,
      names_prefix = "weighted_production_target_",
      values_from = .data$scenario_target_value
    ) %>%
    rename(weighted_production_projected = .data$weighted_production) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("weighted_production_"),
      names_prefix = "weighted_production_",
      names_to = "weighted_production_metric",
      values_to = "weighted_production_value"
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

maybe_group_by_name_ald <- function(data, ..., by_company = FALSE) {
  groups <- c(...)

  if (by_company) {
    groups <- c(groups, "name_ald")
  }

  group_by(data, !!!rlang::syms(groups))
}

add_ald_benchmark <- function(data, ald, region_isos, by_company) {
  ald_with_benchmark <- ald %>%
    filter(.data$is_ultimate_owner) %>%
    mutate(plant_location = tolower(.data$plant_location)) %>%
    inner_join(
      region_isos,
      by = c("plant_location" = "isos")
    ) %>%
    warn_if_has_zero_rows("Joining `region_isos` outputs 0 rows.") %>%
    # Return visibly
    identity() %>%
    group_by(.data$sector, .data$technology, .data$year, .data$region, .data$source) %>%
    summarize(production_corporate_economy = sum(.data$production))

  data %>%
    left_join(ald_with_benchmark, by = c(
      sector = "sector",
      technology = "technology",
      year = "year",
      region = "region",
      scenario_source = "source"
    )) %>%
    maybe_group_by_name_ald(c("sector", "technology", "scenario", "region", "scenario_source"),
      by_company = by_company
    ) %>%
    arrange(.data$year) %>%
    mutate(
      weighted_production_normalized_corporate_economy =
        .data$production_corporate_economy * (first(.data$weighted_production) / first(.data$production_corporate_economy))
    ) %>%
    select(-.data$production_corporate_economy)
}
