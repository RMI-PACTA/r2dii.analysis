#' Add targets for CO2 emissions per unit production at the portfolio level,
#' using the SDA approach
#'
#' This function calculates targets of CO2 emissions per unit production at the
#' portfolio-level, otherwise referred to as "emissions factors". It uses the
#' [sectoral-decarbonization approach
#' (SDA)](https://rmi-pacta.github.io/r2dii.analysis/articles/sda-target.html)
#' to calculate these targets.
#'
#' @template ignores-existing-groups
#'
#' @param data A dataframe like the output of
#'   `r2dii.match::prioritize()`.
#' @param abcd An asset-level data frame like [r2dii.data::abcd_demo].
#' @param co2_intensity_scenario A scenario data frame like
#'   [r2dii.data::co2_intensity_scenario_demo].
#' @param region_isos A data frame like [r2dii.data::region_isos] (default).
#' @param use_credit_limit Logical vector of length 1. `FALSE` defaults to using
#'   the column `loan_size_outstanding`. Set to `TRUE` to instead use the column
#'   `loan_size_credit_limit`.
#' @param by_company Logical vector of length 1. `FALSE` defaults to outputting
#'   `weighted_production_value` at the portfolio-level. Set to `TRUE` to output
#'   `weighted_production_value` at the company-level.
#'
#' @return A tibble including the summarized columns `emission_factor_metric` and
#'   `emission_factor_value`. If `by_company = TRUE`, the output will also have
#'   the column `name_abcd`.
#' @param ald `r lifecycle::badge('superseded')` `ald` has been superseded by
#'   `abcd`.
#'
#' @export
#'
#' @family functions to calculate scenario targets
#'
#' @examples
#' installed <- requireNamespace("r2dii.match", quietly = TRUE) &&
#'   requireNamespace("r2dii.data", quietly = TRUE) &&
#'   packageVersion("r2dii.match") >= "0.1.0"
#'
#' if (installed) {
#'   library(r2dii.match)
#'   library(r2dii.data)
#'
#'   loanbook <- head(loanbook_demo, 150)
#'   abcd <- head(abcd_demo, 100)
#'
#'   matched <- loanbook %>%
#'     match_name(abcd) %>%
#'     prioritize()
#'
#'   # Calculate targets at portfolio level
#'   matched %>%
#'     target_sda(
#'       abcd = abcd,
#'       co2_intensity_scenario = co2_intensity_scenario_demo,
#'       region_isos = region_isos_demo
#'     )
#'
#'   # Calculate targets at company level
#'   matched %>%
#'     target_sda(
#'       abcd = abcd,
#'       co2_intensity_scenario = co2_intensity_scenario_demo,
#'       region_isos = region_isos_demo,
#'       by_company = TRUE
#'     )
#' }
#'
target_sda <- function(data,
                       abcd,
                       co2_intensity_scenario,
                       use_credit_limit = FALSE,
                       by_company = FALSE,
                       region_isos = r2dii.data::region_isos,
                       ald = deprecated()) {
  stopifnot(
    is.data.frame(data),
    is.data.frame(abcd),
    is.data.frame(co2_intensity_scenario),
    is.logical(use_credit_limit),
    is.logical(by_company)
  )

  if (lifecycle::is_present(ald)) {
    lifecycle::deprecate_warn(
      "0.2.0 (expected July 2022)",
      "target_market_share(ald)",
      "target_market_share(abcd)"
    )
    abcd <- ald
  }

  data <- rename_and_warn_ald_names(data)

  data <- ungroup(warn_grouped(data, "Ungrouping input data."))

  crucial_portfolio <- c(
    "loan_size_outstanding",
    "loan_size_credit_limit",
    "name_abcd",
    "sector_abcd"
  )

  crucial_abcd <- c(
    "name_company",
    "sector",
    "technology",
    "year",
    "emission_factor",
    "production"
  )

  crucial_scenario <- c(
    "sector",
    "year",
    "emission_factor"
  )

  check_crucial_names(data, crucial_portfolio)
  check_unique_id(data, "id_loan")
  walk_(crucial_portfolio, ~ check_no_value_is_missing(data, .x))

  check_crucial_names(abcd, crucial_abcd)
  check_type_emission_factor(abcd)

  abcd <- filter_and_warn_na(abcd, "production")
  abcd <- filter_and_warn_na(abcd, "emission_factor")

  region_isos <- change_to_lowercase_and_warn(region_isos, "isos")

  walk_(crucial_abcd, ~ check_no_value_is_missing(abcd, .x))

  check_crucial_names(co2_intensity_scenario, crucial_scenario)
  walk_(crucial_scenario, ~ check_no_value_is_missing(co2_intensity_scenario, .x))

  region_isos <- region_isos %>%
    filter(source %in% unique(co2_intensity_scenario$scenario_source))

  if (identical(nrow(region_isos), 0L)) {
    warn("Found no matching regions for input scenario", class = "no_match")
    return(empty_target_sda_output())
  }

  abcd <- abcd %>%
    mutate(plant_location = tolower(.data$plant_location)) %>%
    left_join(region_isos, by = c(plant_location = "isos")) %>%
    rename(scenario_source = "source")

  abcd_by_sector <- abcd %>%
    aggregate_excluding(c("technology", "plant_location", "country_of_domicile"))

  data <- inner_join(data, abcd_by_sector, by = abcd_columns())

  summary_groups <- c(
    "region",
    "scenario_source"
  )

  if (by_company) {
    data <- data %>%
      summarize_unweighted_emission_factor(
        !!!rlang::syms(c(summary_groups, "name_abcd"))
        )
  } else {
    data <- data %>%
      summarize_weighted_emission_factor(
        !!!rlang::syms(summary_groups),
        use_credit_limit = use_credit_limit
      )
  }

  data <- data %>%
    rename(sector = "sector_abcd")

  relevant_sectors <- data$sector

  if (identical(nrow(data), 0L)) {
    warn("Found no match between loanbook and abcd.", class = "no_match")
    return(empty_target_sda_output())
  }

  relevant_abcd_by_sector <- abcd_by_sector %>%
    filter(.data$sector %in% unique(relevant_sectors))

  corporate_economy <- calculate_market_average(relevant_abcd_by_sector)

  interpolate_groups <- c("scenario_source", "scenario", "sector", "region")

  relevant_scenario <- co2_intensity_scenario %>%
    filter(.data$sector %in% unique(relevant_sectors))

  if (identical(nrow(relevant_scenario), 0L)) {
    warn("Found no match between loanbook and scenario.", class = "no_match")
    return(empty_target_sda_output())
  }

  interpolated_scenario <- interpolate_scenario_yearly(
    relevant_scenario,
    !!!rlang::syms(interpolate_groups)
  )

  adjusted_scenario <- compute_abcd_adjusted_scenario(
    interpolated_scenario,
    corporate_economy
  )

  if (identical(nrow(adjusted_scenario), 0L)) {
    rlang::warn("Found no scenario data for input abcd.")
    return(empty_target_sda_output())
  }

  adjusted_scenario_with_p <- add_p_to_scenario(adjusted_scenario)

  target_summary_groups <- maybe_add_name_abcd(
    c("sector", "scenario", "region", "scenario_source"),
    by_company
  )

  loanbook_targets <- compute_loanbook_targets(
    data,
    adjusted_scenario_with_p,
    !!!rlang::syms(target_summary_groups)
  )

  if (identical(nrow(loanbook_targets), 0L)) {
    rlang::warn("Found no scenario data for the loanbook matches.")
    return(empty_target_sda_output())
  }

  format_and_combine_output(
    data,
    corporate_economy,
    loanbook_targets,
    adjusted_scenario,
    by_company = by_company
  )
}

check_type_emission_factor <- function(abcd) {
  if (!is.double(abcd$emission_factor)) {
    abort("The column emission_factor of the asset-level data frame (`abcd`) does not have the type double", class = "crucial_column_wrong_type")
  }
}

check_unique_id <- function(data, column) {
  if (sum(duplicated(data[[column]]))) {
    abort(
      class = "unique_ids",
      glue("Column `{column}` must not contain any duplicates.", column)
    )
  }

  invisible(data)
}

aggregate_excluding <- function(abcd, columns) {
  .vars <- setdiff(names(abcd), c("production", "emission_factor", columns))

  abcd %>%
    dplyr::group_by_at(.vars = .vars) %>%
    dplyr::filter(.data$production > 0) %>%
    mutate(weight = .data$production / sum(.data$production)) %>%
    summarize(
      production = sum(.data$production),
      emission_factor =
        sum(.data$emission_factor * .data$weight / sum(.data$weight))
    ) %>%
    ungroup()
}

maybe_add_name_abcd <- function(data, by_company = FALSE) {
  out <- data

  if (by_company) {
    out <- c(data, "name_abcd")
  }

  return(out)
}

calculate_market_average <- function(data) {
  data %>%
    group_by(.data$sector, .data$year, .data$region, .data$scenario_source) %>%
    summarize(
      sector_total_production = sum(.data$production),
      # Alias emission_factor_corporate_economy
      .x = list(.data$production * .data$emission_factor)
    ) %>%
    unnest(cols = ".x") %>%
    group_by(.data$sector, .data$year, .data$region, .data$scenario_source) %>%
    summarize(.x = sum(.data$.x / .data$sector_total_production)) %>%
    rename(emission_factor_corporate_economy = ".x") %>%
    ungroup()
}

compute_abcd_adjusted_scenario <- function(data, corporate_economy) {
  corporate_economy_baseline <- corporate_economy %>%
    group_by(.data$sector, .data$scenario_source, .data$region) %>%
    filter(.data$year == min(.data$year, na.rm = TRUE)) %>%
    select(
      all_of(
        c(
          "sector",
          "scenario_source",
          "region",
          "emission_factor_corporate_economy"
        )
      )
    ) %>%
    rename(baseline_emission_factor = "emission_factor_corporate_economy") %>%
    ungroup()

  data %>%
    filter(.data$year >= min(corporate_economy$year)) %>%
    inner_join(
      corporate_economy_baseline,
      by = c("sector", "scenario_source", "region")
    ) %>%
    group_by(
      .data$scenario,
      .data$sector,
      .data$scenario_source,
      .data$region
    ) %>%
    arrange(.data$year) %>%
    mutate(
      baseline_adjustment =
        .data$baseline_emission_factor / first(.data$emission_factor),
      emission_factor_adjusted_scenario =
        .data$emission_factor * .data$baseline_adjustment
    ) %>%
    ungroup() %>%
    select(
      all_of(
        c(
          "scenario",
          "sector",
          "scenario_source",
          "region",
          "year",
          "emission_factor_adjusted_scenario"
        )
      )
    )
}

add_p_to_scenario <- function(data) {
  p <- function(x) (x - last(x)) / (first(x) - last(x))

  data %>%
    group_by(
      .data$sector,
      .data$scenario,
      .data$region,
      .data$scenario_source
    ) %>%
    arrange(.data$year) %>%
    mutate(p = p(.data$emission_factor_adjusted_scenario)) %>%
    ungroup()
}

compute_loanbook_targets <- function(data,
                                     scenario_with_p,
                                     ...) {
  data %>%
    right_join(
      scenario_with_p,
      by = c("year", "sector", "region", "scenario_source")
    ) %>%
    group_by(...) %>%
    arrange(.data$year) %>%
    mutate(
      d = first(.data$emission_factor_projected) -
        last(.data$emission_factor_adjusted_scenario),
      emission_factor_target = (.data$d * .data$p) +
        last(.data$emission_factor_adjusted_scenario)
    ) %>%
    ungroup() %>%
    select(..., all_of(c("year", "emission_factor_target")))
}

pivot_emission_factors_longer <- function(data) {
  data %>%
    pivot_longer(
      cols = tidyr::starts_with("emission_factor_"),
      names_prefix = "emission_factor_",
      names_to = "emission_factor_metric",
      values_to = "emission_factor_value"
    )
}

format_and_combine_output <- function(lbk, corporate_economy, targets, scen, by_company = FALSE) {
  scenario_sectors <- unique(scen$sector)

  lbk <- filter(lbk, .data$sector %in% scenario_sectors)
  corporate_economy <- filter(corporate_economy, .data$sector %in% scenario_sectors)
  targets <- filter(targets, .data$sector %in% scenario_sectors)

  projected <- pivot_emission_factors_longer(lbk)

  corporate_economy <- pivot_emission_factors_longer(corporate_economy)

  targets <- targets %>%
    pivot_wider(
      names_from = "scenario",
      names_prefix = "emission_factor_target_",
      values_from = "emission_factor_target"
    ) %>%
    pivot_emission_factors_longer()

  scenario <- scen %>%
    pivot_wider(
      names_from = "scenario",
      names_prefix = "emission_factor_adjusted_scenario_",
      values_from = "emission_factor_adjusted_scenario"
    ) %>%
    pivot_emission_factors_longer() %>%
    mutate(name_abcd = NULL)

  if (by_company) {
    corporate_economy <- corporate_economy %>%
      mutate(name_abcd = "market")

    scenario <- scenario %>%
      mutate(name_abcd = "market")
  }

  rbind(
    projected,
    corporate_economy,
    targets,
    scenario
  )
}

abcd_columns <- function() {
  c(name_abcd = "name_company", sector_abcd = "sector")
}

empty_target_sda_output <- function() {
  tibble(
    sector = character(0),
    year = integer(0),
    emission_factor_metric = character(0),
    emission_factor_value = numeric(0)
  )
}

interpolate_scenario_yearly <- function(data, ...) {
  data %>%
    group_by(...) %>%
    tidyr::complete(year = tidyr::full_seq(.data$year, 1)) %>%
    mutate(
      emission_factor = zoo::na.approx(
        .data$emission_factor,
        .data$year,
        na.rm = FALSE
      )
    )
}
