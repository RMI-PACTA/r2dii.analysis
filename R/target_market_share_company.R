#' Add company-level production targets
#'
#' This function calculates the company-level production targets, as
#' calculated using either the technology market share ratio `tmsr` or sector
#' market share percentage `smsp`.
#'
#' @param data A "data.frame" like the output of
#'   [summarize_company_production()].
#'
#' @return A tibble with the same groups as the input (if any) and columns:
#'   `tmsr_target_weighted_production` and `smsp_target_weighted_production`.
#' @export
#'
#' @family functions to calculate scenario targets
#'
#' @examples
#' library(r2dii.analysis)
#' library(r2dii.data)
#' library(r2dii.match)
#'
#' master <- r2dii.data::loanbook_demo %>%
#'   r2dii.match::match_name(r2dii.data::ald_demo) %>%
#'   r2dii.match::prioritize() %>%
#'   join_ald_scenario(r2dii.data::ald_demo,
#'     r2dii.data::scenario_demo_2020,
#'     region_isos = region_isos_demo
#'   )
#'
#' company_production <- summarize_company_production(master)
#' company_production
#'
#' target_market_share_company(company_production)
target_market_share_company <- function(data) {
  stopifnot(is.data.frame(data))

  by_company <- c("sector", "scenario", "year", "name_ald")
  crucial <- c(by_company, "technology", "weighted_production", "tmsr", "smsp")

  check_crucial_names(data, crucial)
  purrr::walk(crucial, ~ check_no_value_is_missing(data, .x))

  old_groups <- dplyr::groups(data)
  data <- dplyr::ungroup(data)

  initial_sector_summaries <- data %>%
    dplyr::group_by(!!!rlang::syms(by_company)) %>%
    dplyr::summarize(
      sector_weighted_production = sum(.data$weighted_production)
    ) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::group_by(.data$sector, .data$scenario, .data$name_ald) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::rename(
      initial_sector_production = .data$sector_weighted_production
    ) %>%
    dplyr::select(-.data$year)

  initial_technology_summaries <- data %>%
    dplyr::group_by(!!!rlang::syms(c(by_company, "technology"))) %>%
    dplyr::summarize(
      technology_weighted_production = sum(.data$weighted_production)
    ) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::group_by(
      .data$sector,
      .data$technology,
      .data$scenario,
      .data$name_ald
    ) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::rename(
      initial_technology_production = .data$technology_weighted_production
    ) %>%
    select(-.data$year)

  data %>%
    dplyr::left_join(
      initial_sector_summaries,
      by = c("sector", "scenario", "name_ald")
    ) %>%
    dplyr::left_join(
      initial_technology_summaries,
      by = c("sector", "scenario", "technology", "name_ald")
    ) %>%
    dplyr::mutate(
      tmsr_target_weighted_production = .data$initial_technology_production *
        .data$tmsr,
      smsp_target_weighted_production = .data$initial_technology_production +
        (.data$initial_sector_production * .data$smsp)
    ) %>%
    dplyr::select(
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
    inner_join(pick_tmsr_or_smsp, by=c(sector = "sector",
                                       technology = "technology",
                                       target_name = "which_metric")) %>%
    dplyr::select(-.data$target_name) %>%
    tidyr::pivot_wider(names_from = .data$scenario,
                       names_prefix = "weighted_production_target_",
                       values_from = .data$scenario_target_value) %>%
    dplyr::rename(weighted_production_company = .data$weighted_production) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("weighted_production_"),
                        names_prefix = "weighted_production_",
                        names_to = "weighted_production_metric",
                        values_to = "weighted_production_value") %>%
    dplyr::group_by(!!!old_groups)
}
