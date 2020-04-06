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
#' portfolio_production <- summarize_portfolio_production(master, tmsr, smsp)
#'
#' add_portfolio_target(portfolio_production)
add_portfolio_target <- function(data) {
  stopifnot(is.data.frame(data))

  by_portfolio <- c("sector", "scenario", "year")
  crucial <- c(by_portfolio, "weighted_production", "tmsr", "smsp")

  check_crucial_names(data, crucial)
  purrr::walk(crucial, ~ check_column_has_no_na(data, .x))

  old_groups <- dplyr::groups(data)
  data <- dplyr::ungroup(data)

  # TODO: There STILL must be a better way to do this
  initial_sector_summaries <- data %>%
    dplyr::group_by(.data$sector, .data$scenario, .data$year) %>%
    # TODO: See comments in add_company_target() where I ask why we need first()
    # and suspect you want mutate(data, row_number() == 1L) -- see
    # https://dplyr.tidyverse.org/reference/ranking.html
    dplyr::summarise(sector_weighted_production = sum(.data$weighted_production)) %>%
    dplyr::mutate(initial_sector_production = first(.data$sector_weighted_production)) %>%
    dplyr::select(-.data$sector_weighted_production)

  data %>%
    dplyr::left_join(initial_sector_summaries, by = c("sector", "scenario", "year")) %>%
    dplyr::group_by(.data$sector, .data$scenario, .data$year) %>%
    dplyr::mutate(initial_tech_production = first(.data$weighted_production)) %>%
    dplyr::mutate(
      tmsr_target_weighted_production = .data$initial_tech_production * .data$tmsr,
      smsp_target_weighted_production = .data$initial_tech_production + (.data$initial_sector_production * .data$smsp)
    ) %>%
    dplyr::select(-c(.data$tmsr, .data$smsp, .data$initial_tech_production, .data$initial_sector_production)) %>%
    dplyr::group_by(!!!old_groups)
}
