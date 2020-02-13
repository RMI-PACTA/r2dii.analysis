#' Add fair share calculations to a scenario
#'
#' @param scenario A scenario dataframe like [r2dii.dataraw::scenario_demo].
#' @param start_year The start year of the analysis, crucial for calculating the
#'   correct initial sector total
#'
#' @return A dataframe with the same groups (if any) and columns as `scenario`,
#'   and the additional columns:
#'   * `tfsr` - The "Technology Fair Share Ratio", used to calculate scenario
#'   efforts for high-carbon technologies. The portfolio efforts are calculated
#'   as: \eqn{p(t) = p(0) * tfsr}, where `p(0)` is the technology-level value at
#'   `startyear`.
#'   * `mfsp` - The "Market Fair Share Percentage", used to calculate scenario
#'   efforts for low-carbon technologies. The portfolio efforts are calculated
#'   as \eqn{p(t) = p(0) + (P(0) + mfsp}, where where `p(0)` and `P(0)` is are
#'   the technology level and sector-level value at `startyear`.
#' @export
#'
#' @examples
#' library(r2dii.dataraw)
#'
#' scenario_fair_share(scenario, startyear = 2020)
#'
add_scenario_fair_shares <- function(scenario, start_year) {

  crucial_columns <- c("scenario",
                       "sector",
                       "technology",
                       "region",
                       "year",
                       "value",
                       "units")

  check_crucial_names(scenario, crucial_columns)

  old_groups <- dplyr::groups(scenario)
  scenario <- dplyr::ungroup(scenario)

  scenario <- dplyr::filter(scenario, year >= start_year)

  scenario %>%
    add_technology_fair_share_ratio() %>%
    add_market_fair_share_percentage() %>%
    dplyr::group_by(!!!old_groups)
}

add_technology_fair_share_ratio <- function(scenario) {
  scenario %>%
    dplyr::group_by(
      .data$scenario,
      .data$sector,
      .data$technology,
      .data$region
    ) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(tfsr = .data$value / first(.data$value)) %>%
    dplyr::ungroup()
}

add_market_fair_share_percentage <- function(scenario) {
  scenario %>%
    dplyr::group_by(
      .data$scenario,
      .data$sector,
      .data$region,
      .data$year
    ) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(sector_total_by_year = sum(.data$value)) %>%
    dplyr::group_by(
      .data$scenario,
      .data$sector,
      .data$technology,
      .data$region
    ) %>%
    dplyr::mutate(mfsp = (.data$value - first(.data$value)) / first(.data$sector_total_by_year)) %>%
    dplyr::select(-sector_total_by_year)
}
