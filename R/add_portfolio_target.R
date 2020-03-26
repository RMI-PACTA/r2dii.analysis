#' Add portfolio-level targets
#'
#' This function calculates the portfolio-level production targets, as
#' calculated using either the technology market share ratio `tmsr` or sector
#' market share percentage `smsp`.
#'
#' @param data A "data.frame" like the output of
#'   [summarize_portfolio_production()].
#'
#' @return A tibble with the same groups as the input (if any) and columns:
#'   `tmsr_target_weighted_production` and `smsp_target_weighted_production`.
#' @export
#'
#' @examples
#' library(r2dii.data)
#'
#' scenario_with_fair_shares <- r2dii.scenario::scenario_demo %>%
#'   r2dii.scenario::add_market_share_columns(start_year = 2020)
#'
#' master <- r2dii.data::loanbook_demo %>%
#'   r2dii.match::match_name(r2dii.data::ald_demo) %>%
#'   r2dii.match::prioritize() %>%
#'   join_ald_scenario(r2dii.data::ald_demo, scenario_with_fair_shares)
#'
#' portfolio_production <- summarize_portfolio_production(master, tmsr, smsp)
#'
#' add_portfolio_target(portfolio_production)
add_portfolio_target <- function(data) {
  stopifnot(is.data.frame(data))

  crucial <- c(
    "weighted_production",
    "sector",
    "scenario",
    "year",
    # ASK @jdhoffa: This comes from `data`, right?
    "tmsr"
  )

  check_crucial_names(data, crucial)

  # TODO: There STILL must be a better way to do this
  initial_sector_summaries <- data %>%
    dplyr::group_by(.data$sector, .data$scenario, .data$year) %>%
    dplyr::summarise(sector_weighted_production = sum(.data$weighted_production)) %>%
    dplyr::mutate(initial_sector_production = first(.data$sector_weighted_production)) %>%
    dplyr::select(-.data$sector_weighted_production)

  data %>%
    dplyr::left_join(initial_sector_summaries, by = c("sector", "scenario", "year")) %>%
    dplyr::mutate(initial_tech_production = first(.data$weighted_production)) %>%
    dplyr::mutate(
      tmsr_target_weighted_production = .data$initial_tech_production * .data$tmsr,
      smsp_target_weighted_production = .data$initial_tech_production + (.data$initial_sector_production * .data$smsp)
    ) %>%
    dplyr::select(-c(.data$tmsr, .data$smsp, .data$initial_tech_production, .data$initial_sector_production))
}
