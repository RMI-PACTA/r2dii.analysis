#' Add targets for production, using the market share approach
#'
#' This function calculates the portfolio-level production targets, as
#' calculated using the market share approach applied to each relevant climate
#' production forecast.
#'
#' @template ignores-existing-groups
#'
#' @param data A "data.frame" like the output of
#'   [r2dii.match::prioritize()].
#' @param ald An asset level data frame like [r2dii.data::ald_demo].
#' @param scenario A scenario data frame like [r2dii.data::scenario_demo_2020].
#' @param region_isos A data frame like [r2dii.data::region_isos] (default).
#' @param use_credit_limit Logical vector of length 1. `FALSE` defaults to using
#'   the column `loan_size_outstanding`. Set to `TRUE` to use the column
#'   `loan_size_credit_limit` instead.
#' @param by_company Logical vector of length 1. `FALSE` defaults to outputting
#' `production_value` at the portfolio-level. Set to `TRUE` to output
#' `production_value` at the company-level.
#' @param weight_production Logical vector of length 1. `TRUE` defaults to
#' outputting production, weighted by relative loan-size. Set to `FALSE` to
#' output the unweighted production values.
#'
#' @return A tibble with the summarized columns `metric`, `production` and
#' `technology_share`. If `by_company = TRUE`, the output will also have the
#' column `name_ald`.
#' @export
#'
#' @family functions to calculate scenario targets
#'
#' @examples
#' installed <- requireNamespace("r2dii.data", quietly = TRUE) &&
#'   requireNamespace("r2dii.match", quietly = TRUE)
#' if (!installed) stop("Please install r2dii.match and r2dii.data")
#'
#' library(r2dii.data)
#' library(r2dii.match)
#'
#' matched <- loanbook_demo %>%
#'   match_name(ald_demo) %>%
#'   prioritize()
#'
#' # Calculate targets at portfolio level
#' matched %>%
#'   target_market_share(
#'     ald = ald_demo,
#'     scenario = scenario_demo_2020,
#'     region_isos = region_isos_demo
#'   )
#'
#' # Calculate targets at company level
#' matched %>%
#'   target_market_share(
#'     ald = ald_demo,
#'     scenario = scenario_demo_2020,
#'     region_isos = region_isos_demo,
#'     by_company = TRUE
#'   )
#'
#' matched %>%
#'   target_market_share(
#'     ald = ald_demo,
#'     scenario = scenario_demo_2020,
#'     region_isos = region_isos_demo,
#'     # Calculate unweighted targets
#'     weight_production = FALSE
#'   )
target_market_share <- function(data,
                                ald,
                                scenario,
                                region_isos = r2dii.data::region_isos,
                                use_credit_limit = FALSE,
                                by_company = FALSE,
                                weight_production = TRUE) {
  stopifnot(
    is.data.frame(data),
    is.data.frame(ald),
    is.data.frame(scenario),
    is.data.frame(region_isos),
    is.logical(use_credit_limit),
    is.logical(by_company),
    is.logical(weight_production)
  )

  if (by_company & weight_production) {
    warn(
      glue(
        "You've supplied `by_company = TRUE` and `weight_production = TRUE`.
        This will result in company-level results, weighted by the portfolio
        loan size, which is rarely useful. Did you mean to set one of these
        arguments to `FALSE`?"
      )
    )
  }

  data <- ungroup(warn_grouped(data, "Ungrouping input data."))

  crucial_scenario <- c("scenario", "tmsr", "smsp")
  check_crucial_names(scenario, crucial_scenario)
  check_crucial_names(ald, "is_ultimate_owner")
  walk_(crucial_scenario, ~ check_no_value_is_missing(scenario, .x))

  green_or_brown <- r2dii.data::green_or_brown

  summary_groups <- maybe_add_name_ald(
    c("scenario", "tmsr", "smsp", "region", "scenario_source"),
    by_company
  )

  data <- join_ald_scenario(data, ald, scenario, region_isos)

  if (weight_production) {
    data <- summarize_weighted_production(
      data,
      !!!rlang::syms(summary_groups),
      use_credit_limit = use_credit_limit
    )
  } else {
    data <- summarize_unweighted_production(
      data,
      .data$sector_ald,
      .data$technology,
      .data$year,
      !!!rlang::syms(summary_groups)
    )
  }

  reweighting_groups <- maybe_add_name_ald(
    c("sector_ald", "region", "scenario", "scenario_source", "year"),
    by_company
  )

  data <- reweight_technology_share(
    data,
    !!!rlang::syms(reweighting_groups)
  )

  if (nrow(data) == 0) {
    return(empty_target_market_share_output())
  }

  target_groups <- c("sector_ald", "scenario", "year", "region")

  initial_sector_summaries <- data %>%
    maybe_group_by_name_ald(target_groups, by_company = by_company) %>%
    summarize(sector_weighted_production = sum(.data$weighted_production)) %>%
    arrange(.data$year) %>%
    maybe_group_by_name_ald(
      c("sector_ald", "scenario", "region"),
      by_company = by_company
    ) %>%
    filter(row_number() == 1L) %>%
    rename(
      initial_sector_production = .data$sector_weighted_production
    ) %>%
    select(-.data$year)

  initial_technology_summaries <- data %>%
    maybe_group_by_name_ald(
      c(target_groups, "technology"),
      by_company = by_company
    ) %>%
    summarize(
      technology_weighted_production = sum(.data$weighted_production)
    ) %>%
    arrange(.data$year) %>%
    maybe_group_by_name_ald(
      c("sector_ald", "technology", "scenario", "region"),
      by_company = by_company
    ) %>%
    filter(row_number() == 1L) %>%
    rename(
      initial_technology_production = .data$technology_weighted_production
    ) %>%
    select(-.data$year)

  data <- data %>%
    left_join(
      initial_sector_summaries,
      by = maybe_add_name_ald(
        c("sector_ald", "scenario", "region"),
        by_company = by_company
      )
    ) %>%
    left_join(
      initial_technology_summaries,
      by = maybe_add_name_ald(
        c("sector_ald", "scenario", "region", "technology"),
        by_company = by_company
      )
    ) %>%
    mutate(
      tmsr_target_weighted_production = .data$initial_technology_production *
        .data$tmsr,
      smsp_target_weighted_production = .data$initial_technology_production +
        (.data$initial_sector_production * .data$smsp)
    ) %>%
    select(
      -c(
        .data$tmsr,
        .data$smsp,
        .data$initial_technology_production,
        .data$initial_sector_production
      )
    ) %>%
    pivot_longer(
      cols = c(
        "tmsr_target_weighted_production", "smsp_target_weighted_production"
      ),
      names_to = "target_name",
      values_to = "weighted_production_target"
    ) %>%
    left_join(tmsr_or_smsp(), by = c(target_name = "which_metric")) %>%
    inner_join(
      green_or_brown,
      by = c(
        sector_ald = "sector",
        technology = "technology",
        green_or_brown = "green_or_brown"
      )
    ) %>%
    select(-.data$target_name, -.data$green_or_brown)

  data <- data %>%
    maybe_group_by_name_ald(
      c("sector_ald", "year", "scenario", "region"),
      by_company = by_company
    ) %>%
    mutate(
      .x = .data$weighted_production_target,
      weighted_technology_share_target = .data$.x / sum(.data$.x),
      .x = NULL
    )

  data <- data %>%
    pivot_wider2(
      names_from = .data$scenario,
      values_from = c(
        .data$weighted_production_target,
        .data$weighted_technology_share_target
      )
    )

  data <- data %>%
    rename(
      weighted_production_projected = .data$weighted_production,
      weighted_technology_share_projected = .data$weighted_technology_share,
      sector = .data$sector_ald
    ) %>%
    pivot_longer(cols = starts_with("weighted_")) %>%
    filter(!is.na(.data$value)) %>%
    separate_metric_from_name()

  data <- data %>%
    pivot_wider2()

  ald_with_benchmark <- calculate_ald_benchmark(ald, region_isos, by_company)

  data %>%
    rbind(ald_with_benchmark) %>%
    ungroup()
}

pivot_wider2 <- function(data, ...) {
  abort_if_has_list_colums(data)

  out <- suppressWarnings(pivot_wider(data, ...))
  unnest_list_columns(out)
}

unnest_list_columns <- function(data) {
  if (utils::packageVersion("tidyr") < "1.1.2") {
    suppressWarnings(unnest(data))
  } else {
    unnest(data, where(is.list))
  }
}

tmsr_or_smsp <- function() {
  dplyr::tribble(
    ~which_metric, ~green_or_brown,
    "tmsr_target_weighted_production", "brown",
    "smsp_target_weighted_production", "green"
  )
}

separate_metric_from_name <- function(data) {
  data %>%
    mutate(
      name = sub("weighted_", "", .data$name),
      name = sub("(production)_", "\\1-", .data$name),
      name = sub("(technology_share)_", "\\1-", .data$name)
    ) %>%
    tidyr::separate(.data$name, into = c("name", "metric"), sep = "-")
}

maybe_add_name_ald <- function(data, by_company = FALSE) {
  out <- data

  if (by_company) {
    out <- c(data, "name_ald")
  }

  return(out)
}

maybe_group_by_name_ald <- function(data, ..., by_company = FALSE) {
  groups <- c(...)

  if (by_company) {
    groups <- c(groups, "name_ald")
  }

  group_by(data, !!!rlang::syms(groups))
}

abort_if_has_list_colums <- function(data) {
  if (has_list_colum(data)) {
    abort("`data` must have no list column.")
  }

  invisible(data)
}

has_list_colum <- function(data) {
  any(vapply(data, is.list, logical(1)))
}


calculate_ald_benchmark <- function(ald, region_isos, by_company) {
  out <- ald %>%
    filter(.data$is_ultimate_owner) %>%
    mutate(plant_location = tolower(.data$plant_location)) %>%
    inner_join(
      region_isos,
      by = c("plant_location" = "isos")
    ) %>%
    warn_if_has_zero_rows("Joining `region_isos` outputs 0 rows.") %>%
    # Return visibly
    identity() %>%
    group_by(
      .data$sector, .data$technology, .data$year, .data$region, .data$source
    ) %>%
    summarize(production = sum(.data$production)) %>%
    group_by(
      .data$sector, .data$year, .data$region, .data$source
    ) %>%
    mutate(
      .x = .data$production,
      technology_share = .data$.x / sum(.data$.x),
      .x = NULL,
      metric = "corporate_economy"
    ) %>%
    rename(
      scenario_source = "source"
    )

  if (by_company) {
    out <- out %>%
      mutate(name_ald = "corporate_economy")
  }

  out
}

empty_target_market_share_output <- function() {
  tibble(
    sector = character(0),
    technology = character(0),
    year = integer(0),
    region = character(0),
    scenario_source = character(0),
    metric = character(0),
    production = numeric(0),
    technology_share = numeric(0)
  )
}

reweight_technology_share <- function(data, ...) {
  data %>%
    group_by(...) %>%
    mutate(
      .x = .data$weighted_technology_share,
      weighted_technology_share = .data$.x / sum(.data$.x),
      .x = NULL
    ) %>%
    ungroup()
}
