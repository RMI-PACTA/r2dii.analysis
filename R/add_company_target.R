#' Add company-level targets
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
#' @examples
#' # FIXME: Disabled until scenario_demo_2020 is available
#' # library(r2dii.data)
#' #
#' # master <- r2dii.data::loanbook_demo %>%
#' #   r2dii.match::match_name(r2dii.data::ald_demo) %>%
#' #   r2dii.match::prioritize() %>%
#' #   join_ald_scenario(r2dii.data::ald_demo, r2dii.scenario::scenario_demo_2020)
#' #
#' # company_production <- summarize_company_production(master, tmsr, smsp)
#' #
#' # add_company_target(company_production)
add_company_target <- function(data) {
  stopifnot(is.data.frame(data))

  crucial <- c(
    "weighted_production",
    "name_ald",
    "sector",
    "scenario",
    "year",
    "tmsr",
    "smsp"
  )

  check_crucial_names(data, crucial)
  purrr::walk(crucial, ~ check_column_has_no_na(data, .x))

  old_groups <- dplyr::groups(data)
  data <- dplyr::ungroup(data)

  # TODO: There STILL must be a better way to do this
  initial_sector_summaries <- data %>%
    dplyr::group_by(.data$sector, .data$scenario, .data$year, .data$name_ald) %>%
    dplyr::summarise(sector_weighted_production = sum(.data$weighted_production)) %>%
    dplyr::mutate(initial_sector_production = first(.data$sector_weighted_production)) %>%
    dplyr::select(-.data$sector_weighted_production)

  data %>%
    dplyr::left_join(initial_sector_summaries, by = c("sector", "scenario", "year", "name_ald")) %>%
    dplyr::group_by(.data$sector, .data$scenario, .data$year, .data$name_ald) %>%
    dplyr::mutate(initial_tech_production = first(.data$weighted_production)) %>%
    dplyr::mutate(
      tmsr_target_weighted_production = .data$initial_tech_production * .data$tmsr,
      smsp_target_weighted_production = .data$initial_tech_production + (.data$initial_sector_production * .data$smsp)
    ) %>%
    dplyr::select(-c(.data$tmsr, .data$smsp, .data$initial_tech_production, .data$initial_sector_production)) %>%
    dplyr::group_by(!!!old_groups)
}

