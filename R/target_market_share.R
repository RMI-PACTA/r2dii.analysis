#' Add targets for production, using the market share approach
#'
#' This function calculates the portfolio-level production targets, as
#' calculated using the market share approach applied to each relevant climate
#' production forecast.
#'
#' @template ignores-existing-groups
#'
#' @param data A "data.frame" like the output of `r2dii.match::prioritize`.
#' @param abcd An asset level data frame like [r2dii.data::abcd_demo].
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
#' @param increasing_or_decreasing A data frame like
#' [r2dii.data::increasing_or_decreasing].
#'
#' @return A tibble including the summarized columns `metric`, `production`,
#'   `technology_share`, `percentage_of_initial_production_by_scope` and
#'   `scope`. If `by_company = TRUE`, the output will also have the column
#'   `name_abcd`.
#' @export
#'
#' @family functions to calculate scenario targets
#'
#' @examplesIf rlang::is_installed("r2dii.data") && rlang::is_installed("r2dii.match", version = "0.1.0")
#'
#' library(r2dii.data)
#' library(r2dii.match)
#'
#' loanbook <- head(loanbook_demo, 100)
#' abcd <- head(abcd_demo, 100)
#'
#' matched <- loanbook %>%
#'   match_name(abcd) %>%
#'   prioritize()
#'
#' # Calculate targets at portfolio level
#' matched %>%
#'   target_market_share(
#'     abcd = abcd,
#'     scenario = scenario_demo_2020,
#'     region_isos = region_isos_demo
#'     )
#'
#' # Calculate targets at company level
#' matched %>%
#'   target_market_share(
#'   abcd = abcd,
#'   scenario = scenario_demo_2020,
#'   region_isos = region_isos_demo,
#'   by_company = TRUE
#'   )
#'
#' matched %>%
#'   target_market_share(
#'     abcd = abcd,
#'     scenario = scenario_demo_2020,
#'     region_isos = region_isos_demo,
#'     # Calculate unweighted targets
#'     weight_production = FALSE
#'     )
target_market_share <- function(data,
                                abcd,
                                scenario,
                                region_isos = r2dii.data::region_isos,
                                use_credit_limit = FALSE,
                                by_company = FALSE,
                                weight_production = TRUE,
                                increasing_or_decreasing = r2dii.data::increasing_or_decreasing) {
  stopifnot(
    is.data.frame(data),
    is.data.frame(abcd),
    is.data.frame(scenario),
    is.data.frame(region_isos),
    is.logical(use_credit_limit),
    is.logical(by_company),
    is.logical(weight_production)
  )

  region_isos <- change_to_lowercase_and_warn(region_isos, "isos")

  warn_if_by_company_and_weight_production(by_company, weight_production)

  data <- ungroup(warn_grouped(data, "Ungrouping input data."))

  check_input_for_crucial_columns(data, abcd, scenario)

  check_unique_id(data, "id_loan")

  abcd <- fill_and_warn_na(abcd, "production")
  abcd <- dplyr::summarize(
    abcd,
    production = sum(.data[["production"]]),
    .by = -"production"
  )

  data <- aggregate_by_name_abcd(data)

  if ("production" %in% colnames(scenario)) {
    warn("The column `production` has been removed from the dataset `scenario`.
         The columns `tmsr` and `smsp` will be used instead",
         class = "scenario_production_column_removed")
    scenario <- dplyr::select(scenario, -all_of("production"))
    return(scenario)
  }

  data <- join_abcd_scenario_(
    data,
    abcd,
    scenario,
    region_isos,
    add_green_technologies = TRUE
  )

  if (nrow(data) == 0) {
    return(empty_target_market_share_output())
  }

  crucial_groups <- c(
    "id_loan",
    "loan_size_outstanding",
    "loan_size_outstanding_currency",
    "loan_size_credit_limit",
    "loan_size_credit_limit_currency",
    "name_abcd",
    "sector_abcd",
    "technology",
    "year",
    "scenario",
    "region",
    "tmsr",
    "smsp",
    "scenario_source"
  )

  data <- data %>%
    group_by(!!!rlang::syms(crucial_groups)) %>%
    summarize(production = sum(.data$production))

  data <- calculate_targets(data)

  tmsr_or_smsp <- tmsr_or_smsp()

  data <- pick_sms_or_tms_target(data, increasing_or_decreasing, tmsr_or_smsp)

  if (nrow(data) == 0) {
    return(empty_target_market_share_output())
  }

  summary_groups <- c(
    "scenario",
    "region",
    "scenario_source",
    "name_abcd"
  )

  if (weight_production) {
    data <- summarize_weighted_production_(
      data,
      !!!rlang::syms(summary_groups),
      use_credit_limit = use_credit_limit,
      with_targets = TRUE
    )
  } else {
    data <- summarize_unweighted_production(
      data,
      !!!rlang::syms(summary_groups),
      with_targets = TRUE
    )
  }

  if (!by_company) {
    aggregate_company_groups <- c(
      "sector_abcd",
      "technology",
      "year",
      "scenario",
      "region",
      "scenario_source"
    )

    data <- data %>%
      group_by(!!!rlang::syms(aggregate_company_groups)) %>%
      summarize(
        weighted_production = sum(.data$weighted_production),
        weighted_production_target = sum(.data$weighted_production_target),
        weighted_technology_share = sum(.data$weighted_technology_share),
        weighted_technology_share_target = sum(.data$weighted_technology_share_target)
      )
  }

  data <- reweight_technology_share(data, by_company)

  data <- format_output_dataframe(data)

  corporate_economy <- calculate_abcd_benchmark(abcd, region_isos, by_company)

  relevant_corporate_economy <- filter(
    corporate_economy,
    .data$sector %in% unique(data$sector),
    .data$year %in% unique(data$year)
  )

  data <- data %>%
    dplyr::bind_rows(relevant_corporate_economy)

  data <- add_percentage_of_initial_production_by_scope(data, increasing_or_decreasing, tmsr_or_smsp, by_company)

  data %>%
    ungroup()
}

add_percentage_of_initial_production_by_scope <- function(data,
                                                          increasing_or_decreasing,
                                                          tmsr_or_smsp,
                                                          by_company) {
  data <- data %>%
    left_join(increasing_or_decreasing, by = c("sector", "technology")) %>%
    left_join(tmsr_or_smsp, by = "increasing_or_decreasing") %>%
    rename(target_name = "which_metric") %>%
    select(-all_of("increasing_or_decreasing"))

  percent_by_sector_groups <- add_name_abcd_if_by_company(
    c("sector", "region", "scenario_source", "metric"),
    by_company
  )

  data %>%
    group_by(!!!rlang::syms(c(percent_by_sector_groups, "technology"))) %>%
    mutate(
      initial_technology_production = first(.data$production, order_by = .data$year)
    ) %>%
    group_by(!!!rlang::syms(c(percent_by_sector_groups, "year"))) %>%
    mutate(sector_production = sum(.data$production)) %>%
    group_by(!!!rlang::syms(percent_by_sector_groups)) %>%
    mutate(initial_sector_production = first(.data$sector_production)) %>%
    ungroup() %>%
    mutate(
      percentage_of_initial_technology_production = (.data$production - .data$initial_technology_production) / .data$initial_technology_production,
      percentage_of_initial_sector_production = (.data$production - .data$initial_technology_production) / .data$initial_sector_production
    ) %>%
    mutate(
      scope = dplyr::case_when(
        target_name == "tmsr_target_production" ~ "technology",
        target_name == "smsp_target_production" ~ "sector"
      ),
      percentage_of_initial_production_by_scope = dplyr::case_when(
        scope == "technology" ~ percentage_of_initial_technology_production,
        scope == "sector" ~ percentage_of_initial_sector_production
      )
    ) %>%
    select(
      -all_of(
        c(
          "target_name",
          "sector_production",
          "initial_technology_production",
          "initial_sector_production",
          "percentage_of_initial_technology_production",
          "percentage_of_initial_sector_production"
        )
      )
    )
}

calculate_targets <- function(data) {
  target_groups <- c("sector_abcd", "scenario", "region", "name_abcd")

  data %>%
    arrange(.data$year) %>%
    group_by(!!!rlang::syms(c(target_groups, "technology", "year"))) %>%
    mutate(technology_production = sum(.data$production)) %>%
    group_by(!!!rlang::syms(c(target_groups, "year"))) %>%
    mutate(sector_production = sum(.data$production)) %>%
    group_by(!!!rlang::syms(c(target_groups, "technology"))) %>%
    mutate(
      initial_technology_production = first(.data$technology_production),
      technology_production = NULL
    ) %>%
    group_by(!!!rlang::syms(target_groups)) %>%
    mutate(
      initial_sector_production = first(.data$sector_production),
      sector_production = NULL
    ) %>%
    ungroup() %>%
    mutate(
      tmsr_target_production = .data$initial_technology_production *
        .data$tmsr,
      smsp_target_production = .data$initial_technology_production +
        (.data$initial_sector_production * .data$smsp),
      tmsr = NULL,
      initial_technology_production = NULL,
      initial_sector_production = NULL,
      smsp = NULL
    ) %>%
    mutate(
      smsp_target_production = ifelse(
        .data$smsp_target_production < 0,
        0,
        .data$smsp_target_production
      )
    ) %>%
    pivot_longer(
      cols = c("tmsr_target_production", "smsp_target_production"),
      names_to = "target_name",
      values_to = "production_target"
    )
}

calculate_abcd_benchmark <- function(abcd, region_isos, by_company) {
  out <- abcd %>%
    filter(.data$is_ultimate_owner) %>%
    mutate(plant_location = tolower(.data$plant_location)) %>%
    inner_join(
      region_isos,
      by = c("plant_location" = "isos"),
      relationship = "many-to-many"
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
      .x = sum(.data$production),
      technology_share = ifelse(.data$.x == 0, 0, .data$production / .data$.x),
      .x = NULL,
      metric = "corporate_economy"
    ) %>%
    rename(
      scenario_source = "source"
    )

  if (by_company) {
    out <- out %>%
      mutate(name_abcd = "corporate_economy")
  }

  out
}

add_name_abcd_if_by_company <- function(list, by_company = FALSE) {
  if (by_company) {
    list <- c(list, "name_abcd")
  }

  list
}

pick_sms_or_tms_target <- function(data, increasing_or_decreasing, tmsr_or_smsp) {
  data %>%
    left_join(tmsr_or_smsp, by = c(target_name = "which_metric")) %>%
    inner_join(
      increasing_or_decreasing,
      by = c(
        sector_abcd = "sector",
        technology = "technology",
        increasing_or_decreasing = "increasing_or_decreasing"
      )
    ) %>%
    warn_if_has_zero_rows("Joining `r2dii.data::increasing_or_decreasing` outputs 0 rows") %>%
    select(-all_of(c("target_name", "increasing_or_decreasing")))
}

tmsr_or_smsp <- function() {
  dplyr::tribble(
    ~which_metric, ~increasing_or_decreasing,
    "tmsr_target_production", "decreasing",
    "smsp_target_production", "increasing"
  )
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

aggregate_by_name_abcd <- function(data) {
  data %>%
    group_by(
      .data$loan_size_outstanding_currency,
      .data$loan_size_credit_limit_currency,
      .data$name_abcd,
      .data$sector_abcd
    ) %>%
    summarize(
      id_loan = first(.data$id_loan),
      loan_size_outstanding = sum(.data$loan_size_outstanding),
      loan_size_credit_limit = sum(.data$loan_size_credit_limit)
    ) %>%
    ungroup()
}

format_output_dataframe <- function(data) {
  data <- data %>%
    pivot_wider2(
      names_from = "scenario",
      values_from = c(
        "weighted_production_target",
        "weighted_technology_share_target"
      )
    )

  data <- data %>%
    rename(
      weighted_production_projected = "weighted_production",
      weighted_technology_share_projected = "weighted_technology_share",
      sector = "sector_abcd"
    )

  data %>%
    pivot_longer(cols = starts_with("weighted_")) %>%
    filter(!is.na(.data$value)) %>%
    separate_metric_from_name() %>%
    pivot_wider2()
}

separate_metric_from_name <- function(data) {
  data %>%
    mutate(
      name = sub("weighted_", "", .data$name),
      name = sub("(production)_", "\\1-", .data$name),
      name = sub("(technology_share)_", "\\1-", .data$name)
    ) %>%
    tidyr::separate(
      .data$name,
      into = c("name", "metric"),
      sep = "-",
      extra = "merge"
      )
}

check_input_for_crucial_columns <- function(data, abcd, scenario) {
  valid_columns <- c(
    "id_loan",
    "id_direct_loantaker",
    "name_direct_loantaker",
    "id_intermediate_parent_1",
    "name_intermediate_parent_1",
    "id_ultimate_parent",
    "name_ultimate_parent",
    "loan_size_outstanding",
    "loan_size_outstanding_currency",
    "loan_size_credit_limit",
    "loan_size_credit_limit_currency",
    "sector_classification_system",
    "sector_classification_input_type",
    "sector_classification_direct_loantaker",
    "fi_type",
    "flag_project_finance_loan",
    "name_project",
    "lei_direct_loantaker",
    "isin_direct_loantaker",
    "id_2dii",
    "level",
    "sector",
    "sector_abcd",
    "name",
    "name_abcd",
    "score",
    "source",
    "borderline"
  )

  check_valid_columns(data, valid_columns)

  crucial_all <- c(
    "sector",
    "technology",
    "year"
  )

  crucial_scenario <- c(
    crucial_all,
    "scenario",
    "region",
    "tmsr",
    "smsp",
    "scenario_source"
  )

  check_crucial_names(scenario, crucial_scenario)

  crucial_abcd <- c(
    crucial_all,
    "name_company",
    "production",
    "plant_location",
    "is_ultimate_owner"
  )

  check_crucial_names(abcd, crucial_abcd)

  walk_(crucial_scenario, ~ check_no_value_is_missing(scenario, .x))
}

check_valid_columns <- function(data, valid_columns) {
  invalid_columns <- setdiff(names(data), valid_columns)

  if (length(invalid_columns) != 0) {
    abort(
      glue("Loanbook has unexpected names: `{invalid_columns}`."),
      class = "invalid_columns"
    )
  }

  invisible(data)
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

abort_if_has_list_colums <- function(data) {
  if (has_list_colum(data)) {
    abort("`data` must have no list column.")
  }

  invisible(data)
}

has_list_colum <- function(data) {
  any(vapply(data, is.list, logical(1)))
}

reweight_technology_share <- function(data, by_company) {
  reweighting_groups <- add_name_abcd_if_by_company(
    c("sector_abcd", "region", "scenario", "scenario_source", "year"),
    by_company
  )

  data %>%
    group_by(!!!rlang::syms(reweighting_groups)) %>%
    mutate(
      .x = sum(.data$weighted_technology_share),
      .y = sum(.data$weighted_technology_share_target),
      weighted_technology_share = ifelse(
        .data$.x == 0,
        0,
        .data$weighted_technology_share / .data$.x
      ),
      weighted_technology_share_target = ifelse(
        .data$.y == 0,
        0,
        .data$weighted_technology_share_target / .data$.y
      ),
      .x = NULL,
      .y = NULL,
    ) %>%
    ungroup()
}
