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
#' installed <- requireNamespace("r2dii.data", quietly = TRUE) &&
#'   requireNamespace("r2dii.match", quietly = TRUE)
#' if (!installed) stop("Please install r2dii.match and r2dii.data")
#'
#' library(r2dii.data)
#' library(r2dii.match)
#'
#' master <- loanbook_demo %>%
#'   match_name(ald_demo) %>%
#'   prioritize() %>%
#'   join_ald_scenario(ald_demo, scenario_demo_2020)
#'
#' company_production <- summarize_company_production(master, tmsr, smsp)
#'
#' add_company_target(company_production)
add_company_target <- function(data) {
  stopifnot(is.data.frame(data))

  by_company <- c("sector",  "scenario", "year", "name_ald")
  crucial <- c(by_company, "technology", "weighted_production", "tmsr", "smsp")

  check_crucial_names(data, crucial)
  purrr::walk(crucial, ~ check_column_has_no_na(data, .x))

  old_groups <- dplyr::groups(data)
  data <- dplyr::ungroup(data)

  # TODO: There STILL must be a better way to do this
  initial_sector_summaries <- data %>%
    dplyr::group_by(!!!rlang::syms(by_company)) %>%
    dplyr::summarise(sector_weighted_production = sum(.data$weighted_production)) %>%
    dplyr::arrange(year) %>%
    dplyr::group_by(name_ald) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::rename(initial_sector_production = sector_weighted_production) %>%
    select(-year)

  initial_technology_summaries <- data %>%
    dplyr::group_by(!!!rlang::syms(c(by_company,"technology"))) %>%
    dplyr::summarise(technology_weighted_production = sum(.data$weighted_production)) %>%
    dplyr::arrange(year) %>%
    dplyr::group_by(name_ald, technology) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::rename(initial_technology_production = technology_weighted_production) %>%
    select(-year)

  data %>%
    dplyr::left_join(initial_sector_summaries, by = c("sector", "scenario", "name_ald")) %>%
    dplyr::left_join(initial_technology_summaries, by = c("sector", "scenario", "technology", "name_ald")) %>%
    dplyr::mutate(
      tmsr_target_weighted_production = .data$initial_technology_production * .data$tmsr,
      smsp_target_weighted_production = .data$initial_technology_production + (.data$initial_sector_production * .data$smsp)
    ) %>%
    dplyr::select(-c(.data$tmsr, .data$smsp, .data$initial_technology_production, .data$initial_sector_production)) %>%
    dplyr::group_by(!!!old_groups)
}
