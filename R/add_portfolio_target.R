#' Add portfolio-level targets
#'
#' This function calculates the portfolio-level production targets, as
#' calculated using either the technology market share ratio `tmsr` or sector
#' market share percentage `smsp`.
#'
#' @param data A "data.frame" like the output of [summarize_portfolio_production()].
#'
#' @return A tibble with the same groups as the input (if any) and columns:
#'   `tmsr_target_weighted_production` and `smsp_target_weighted_production`.
#' @export
#'
#' @examples
add_portfolio_target <- function(data, start_year){
  stopifnot(
    is.data.frame(data),
    !is.na(start_year),
    is.numeric(start_year)
  )

  check_start_year_in_years(data, start_year)

  crucial <- c(
    "weighted_production",
    "sector",
    "scenario",
    "year"
  )

  check_crucial_names(data, crucial)

  # TODO: There STILL must be a better way to do this
  initial_sector_summaries <- data %>%
    dplyr::group_by(sector, scenario, year) %>%
    dplyr::summarise(sector_weighted_production = sum(weighted_production)) %>%
    dplyr::mutate(initial_sector_production = first(sector_weighted_production)) %>%
    dplyr::select(-sector_weighted_production)

  data %>%
    r2dii.scenario::add_market_share_columns(data, start_year = start_year) %>%
    dplyr::left_join(intial_sector_summaries, by=c("sector", "scenario", "year")) %>%
    dplyr::mutate(tmsr_target_weighted_production = initial_tech_production * tmsr,
           smsp_target_weighted_production = initial_tech_production + (initial_sector_production*smsp)) %>%
    dplyr::select(-c(tmsr,smsp, initial_tech_production, initial_sector_production))
}

check_start_year_in_years <- function(data, start_year){
  if (!(start_year %in% data$year)){
    rlang::abort(
      class = "start_year_in_range",
      glue::glue("`start_year` must be in the range of column `year`.")
    )
  }
  invisible(data)
}
