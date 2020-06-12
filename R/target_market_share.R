#' Add targets for production, using the market share approach
#'
#' This function calculates the portfolio-level production targets, as
#' calculated using either the technology market share ratio `tmsr` or sector
#' market share percentage `smsp`.
#'
#' @param data A "data.frame" like the output of
#'   [r2dii.match::prioritize()].
#' @param ald An asset level dataframe like [r2dii.data::ald_demo].
#' @param scenario A scenario dataframe like [r2dii.data::scenario_demo_2020].
#' @param region_isos A dataframe like [r2dii.data::region_isos] (default).
#' @param use_credit_limit Logical vector of length 1. `FALSE` defaults to using
#'   the column `loan_size_outstanding`. Set to `TRUE` to use the column
#'   `loan_size_credit_limit` instead.
#' @param by_company Logical vector of length 1. `FALSE` defaults to outputting
#' `weighted_production_value` at the portfolio-level. Set to `TRUE` to output
#' `weighted_production_value` at the company-level.
#'
#' @return A tibble with the same groups as the input (if any) and columns:
#'   `tmsr_target_weighted_production` and `smsp_target_weighted_production`.
#' @export
#'
#' @family functions to calculate scenario targets
#'
#' @examples

target_market_share <- function(data,
                                ald,
                                scenario,
                                region_isos = r2dii.data::region_isos,
                                use_credit_limit = FALSE,
                                by_company = FALSE
                                ) {
  stopifnot(is.data.frame(data))

  old_groups <- dplyr::groups(data)
  data <- dplyr::ungroup(data)


  crucial_scenario <- c("scenario", "tmsr", "smsp")
  check_crucial_names(scenario, crucial_scenario)
  purrr::walk(crucial_scenario, ~ check_no_value_is_missing(scenario, .x))

  summary_groups <- maybe_add_name_ald(
    c("scenario", "tmsr", "smsp", "region"),
    by_company
    )

  data <- data %>%
    join_ald_scenario(ald,
                      scenario,
                      region_isos) %>%
    summarize_weighted_production(
      !!!rlang::syms(summary_groups),
      use_credit_limit = use_credit_limit
    )

  target_groups <- c("sector", "scenario", "year", "region")

  initial_sector_summaries <- data %>%
    maybe_group_by_name_ald(target_groups,
                            by_company = by_company) %>%
    dplyr::summarize(
      sector_weighted_production = sum(.data$weighted_production)
    ) %>%
    dplyr::arrange(.data$year) %>%
    maybe_group_by_name_ald(c("sector", "scenario", "region"),
                            by_company = by_company) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::rename(
      initial_sector_production = .data$sector_weighted_production
    ) %>%
    dplyr::select(-.data$year)

  initial_technology_summaries <- data %>%
    maybe_group_by_name_ald(c(target_groups, "technology"),
                            by_company = by_company) %>%
    dplyr::summarize(
      technology_weighted_production = sum(.data$weighted_production)
    ) %>%
    dplyr::arrange(.data$year) %>%
    maybe_group_by_name_ald(c("sector", "technology","scenario", "region"),
                            by_company = by_company) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::rename(
      initial_technology_production = .data$technology_weighted_production
    ) %>%
    select(-.data$year)

  data %>%
    dplyr::left_join(
      initial_sector_summaries,
      by = maybe_add_name_ald(c("sector", "scenario", "region"),
                              by_company = by_company)
      ) %>%
    dplyr::left_join(
      initial_technology_summaries,
      by = maybe_add_name_ald(c("sector", "scenario", "region", "technology"),
                              by_company = by_company)
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
    inner_join(pick_tmsr_or_smsp, by = c(
      sector = "sector",
      technology = "technology",
      target_name = "which_metric"
    )) %>%
    dplyr::select(-.data$target_name) %>%
    tidyr::pivot_wider(
      names_from = .data$scenario,
      names_prefix = "weighted_production_target_",
      values_from = .data$scenario_target_value
    ) %>%
    dplyr::rename(weighted_production_projected = .data$weighted_production) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("weighted_production_"),
      names_prefix = "weighted_production_",
      names_to = "weighted_production_metric",
      values_to = "weighted_production_value"
    ) %>%
    dplyr::group_by(!!!old_groups)
}

maybe_add_name_ald <- function(data, by_company = FALSE){
  out <- data

  if (by_company) {
    out <- c(data, "name_ald")
  }

  return(out)
}

maybe_group_by_name_ald <- function(data, ..., by_company = FALSE){
  groups <- c(...)

  if (by_company) {
    groups <- c(groups, "name_ald")
  }

  group_by(data, !!!rlang::syms(groups))
}
