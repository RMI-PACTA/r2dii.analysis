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
#' master <- tibble::tibble(
#'   id_loan = c("i1", "i2", "i1", "i2"),
#'   loan_size_outstanding = c(40, 10, 40, 10),
#'   loan_size_credit_limit = c(2, 2, 2, 2),
#'   sector = c("automotive", "automotive","automotive","automotive"),
#'   name_ald = c("shaanxi auto","shaanxi auto","shaanxi auto", "shaanxi auto"),
#'   technology = c("ta", "ta", "tb", "tb"),
#'   year = c(2025, 2025, 2025, 2025),
#'   production = c(10, 30, 20, 40),
#'   plant_location = c("BF", "BF", "BF", "BF"),
#'   scenario = c("sds", "sds", "sds", "sds"),
#'   region = c("global", "global", "global", "global"),
#'   tmsr = c(0.5, 0.5, 0.5, 0.5),
#'   smsp = c(-0.08, -0.08, -0.08, -0.08)
#' )
#' master
#'
#' company_production <- summarize_company_production(master)
#' company_production
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

  initial_sector_summaries <- data %>%
    dplyr::group_by(!!!rlang::syms(by_company)) %>%
    dplyr::summarise(sector_weighted_production = sum(.data$weighted_production)) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::group_by(.data$name_ald) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::rename(initial_sector_production = .data$sector_weighted_production) %>%
    dplyr::select(-.data$year)

  initial_technology_summaries <- data %>%
    dplyr::group_by(!!!rlang::syms(c(by_company,"technology"))) %>%
    dplyr::summarise(technology_weighted_production = sum(.data$weighted_production)) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::group_by(.data$name_ald, .data$technology) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::rename(initial_technology_production = .data$technology_weighted_production) %>%
    select(-.data$year)

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
