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
#'   `start_year`.
#'   * `mfsp` - The "Market Fair Share Percentage", used to calculate scenario
#'   efforts for low-carbon technologies. The portfolio efforts are calculated
#'   as \eqn{p(t) = p(0) + (P(0) + mfsp}, where where `p(0)` and `P(0)` is are
#'   the technology level and sector-level value at `start_year`.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("r2dii.dataraw", quietly = TRUE)) {
#'   scenario <- r2dii.dataraw::scenario_demo
#'   add_scenario_fair_shares(scenario, start_year = 2020)
#' }
add_scenario_fair_shares <- function(scenario, start_year) {
  old_groups <- dplyr::groups(scenario)
  scenario <- dplyr::ungroup(scenario)

  scenario %>%
    check_crucial_names(crucial_fs_columns()) %>%
    check_consistent_units()

  scenario %>%
    dplyr::filter(.data$year >= start_year) %>%
    add_technology_fair_share_ratio() %>%
    add_market_fair_share_percentage() %>%
    dplyr::group_by(!!!old_groups)
}

crucial_fs_columns <- function() {
  c(
    common_fs_groups(),
    "technology",
    "year",
    "value",
    "units"
  )
}

common_fs_groups <- function() {
  c("scenario", "sector", "region")
}

check_consistent_units <- function(scenario) {
  units <- scenario %>%
    dplyr::group_by(!!!syms(c(common_fs_groups(), "technology"))) %>%
    dplyr::summarise(are_consistent = (length(unique(units)) == 1L))

  if (all(units$are_consistent)) {
    return(invisible(scenario))
  }

  inconsistent_units <- units %>%
    dplyr::filter(!.data$are_consistent) %>%
    dplyr::ungroup()

  rlang::abort(
    "inconsistent_units",
    message = glue::glue(
      "`scenario` must have consistent `units` per each `technology` group.
      Technologies with inconsistent units: \\
      {commas(inconsistent_units$technology)}"
    )
  )
}

add_technology_fair_share_ratio <- function(scenario) {
  scenario %>%
    dplyr::group_by(!!!syms(c(common_fs_groups(), "technology"))) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(tfsr = .data$value / first(.data$value)) %>%
    dplyr::ungroup()
}

add_market_fair_share_percentage <- function(scenario) {
  scenario %>%
    dplyr::group_by(!!!syms(c(common_fs_groups(), "year"))) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(sector_total_by_year = sum(.data$value)) %>%
    dplyr::group_by(!!!syms(c(common_fs_groups(), "technology"))) %>%
    dplyr::mutate(
      mfsp = (.data$value - first(.data$value)) /
        first(.data$sector_total_by_year)
    ) %>%
    dplyr::select(-.data$sector_total_by_year)
}
