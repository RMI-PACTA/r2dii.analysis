#' Summaries based on the weight of each loan per sector per year
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions (summarize_weighted_production() and
#' summarize_weighted_percent_change()) were deprecated because they are not
#' required as a user-facing function for PACTA for Banks.
#' They are still used internally though.
#' All relevant outputs of the PACTA for Banks analysis can be obtained using
#' the target_market_share() and target_sda() functions.
#'
#' Based on on the weight of each loan per sector per year,
#' `summarize_weighted_production()` and `summarize_weighted_percent_change()`
#' summarize the production and percent-change, respectively.
#'
#' @keywords internal
#'
#' @param data A data frame like the output of [join_abcd_scenario()].
#' @param use_credit_limit Logical vector of length 1. `FALSE` defaults to using
#'   the column `loan_size_outstanding`. Set to `TRUE` to instead use the column
#'   `loan_size_credit_limit`.
#' @param ... Variables to group by.
#'
#' @seealso [join_abcd_scenario()].
#'
#' @export
#'
#' @section Warning:
#' The percent-change analysis excludes companies with 0 production. percent-change is
#' undefined for companies that have no initial production; including such
#' companies would cause percent-change percentage to be infinite, which is wrong.
#'
#' @family utility functions
#'
#' @return A tibble with the same groups as the input (if any) and columns:
#'   `sector`, `technology`, and `year`; and `weighted_production` or
#'   `weighted_production` for `summarize_weighted_production()` and
#'   `summarize_weighted_percent_change()`, respectively.
#'
#' @examplesIf rlang::is_installed("r2dii.data") && rlang::is_installed("r2dii.match", version = "0.1.0")
#' library(r2dii.data)
#' library(r2dii.match)
#'
#' loanbook <- head(loanbook_demo, 150)
#' abcd <- head(abcd_demo, 100)
#' master <- loanbook %>%
#'   match_name(abcd) %>%
#'   prioritize() %>%
#'   join_abcd_scenario(
#'     abcd = abcd,
#'     scenario = scenario_demo_2020,
#'     region_isos = region_isos_demo
#'     ) %>%
#'   dplyr::filter(production != 0)
#'
#' summarize_weighted_production(master)
#'
#' summarize_weighted_production(master, use_credit_limit = TRUE)
#'
#' summarize_weighted_percent_change(master)
#'
#' summarize_weighted_percent_change(master, use_credit_limit = TRUE)
summarize_weighted_production <- function(data, ..., use_credit_limit = FALSE) {
  lifecycle::deprecate_soft(when = "0.5.0", what = "summarize_weighted_production()")
  summarize_weighted_production_(data, ..., use_credit_limit = use_credit_limit, with_targets = FALSE)
}

summarize_weighted_production_ <- function(data, ..., use_credit_limit = FALSE, with_targets = FALSE) {
  stopifnot(is.data.frame(data))

  old_groups <- dplyr::groups(data)

  crucial <- c("sector_abcd", "year", "technology")

  if (with_targets) {
    crucial <- c(crucial, "production_target")
  }

  check_crucial_names(data, c("production", crucial))
  walk_(crucial, ~ check_no_value_is_missing(data, .x))

  data <- data %>%
    ungroup() %>%
    add_loan_weight(use_credit_limit = use_credit_limit) %>%
    add_technology_share()

  if (with_targets) {
    data %>%
      add_technology_share_target() %>%
      calculate_weighted_loan_metric("production") %>%
      calculate_weighted_loan_metric("technology_share") %>%
      calculate_weighted_loan_metric("production_target") %>%
      calculate_weighted_loan_metric("technology_share_target") %>%
      group_by(
        .data$sector_abcd,
        .data$technology,
        .data$year,
        ...
      ) %>%
      summarize(
        weighted_production = sum(.data$weighted_loan_production),
        weighted_technology_share = sum(.data$weighted_loan_technology_share),
        weighted_production_target = sum(.data$weighted_loan_production_target),
        weighted_technology_share_target = sum(.data$weighted_loan_technology_share_target)
      ) %>%
      # Restore old groups
      group_by(!!!old_groups)
  } else {
    data %>%
      calculate_weighted_loan_metric("production") %>%
      calculate_weighted_loan_metric("technology_share") %>%
      group_by(.data$sector_abcd, .data$technology, .data$year, ...) %>%
      summarize(
        weighted_production = sum(.data$weighted_loan_production),
        weighted_technology_share = sum(.data$weighted_loan_technology_share)
      ) %>%
      # Restore old groups
      group_by(!!!old_groups)
  }
}

summarize_unweighted_production <- function(data, ..., with_targets = FALSE) {
  old_groups <- dplyr::groups(data)

  data <- data %>%
    select(
      -all_of(c("id_loan", "loan_size_credit_limit", "loan_size_outstanding"))
    ) %>%
    distinct() %>%
    group_by(.data$sector_abcd, .data$technology, .data$year, ...)

  # FIXME: Though production here is unweighted, we still name the variables
  # `weighted_*`. This is to allow easier reshaping of the output data at the
  # end of `target_market_share()`.
  if (with_targets) {
    data %>%
      summarize(
        weighted_production = .data$production,
        weighted_production_target = .data$production_target,
        .groups = "keep"
      ) %>%
      ungroup("technology") %>%
      mutate(
        weighted_technology_share = .data$weighted_production / sum(.data$weighted_production),
        weighted_technology_share_target = .data$weighted_production_target / sum(.data$weighted_production_target)
      ) %>%
      group_by(!!!old_groups)
  } else {
    data %>%
      summarize(weighted_production = .data$production, .groups = "keep") %>%
      ungroup(all_of(c("technology", "tmsr","smsp"))) %>%
      mutate(weighted_technology_share = .data$weighted_production / sum(.data$weighted_production)) %>%
      group_by(!!!old_groups)
  }
}

#' @rdname summarize_weighted_production
#' @export
summarize_weighted_percent_change <- function(data, ..., use_credit_limit = FALSE) {
  lifecycle::deprecate_soft(when = "0.5.0", what = "summarize_weighted_percent_change()")
  stopifnot(is.data.frame(data))

  data %>%
    ungroup() %>%
    add_loan_weight(use_credit_limit = use_credit_limit) %>%
    add_percent_change() %>%
    calculate_weighted_loan_metric("percent_change") %>%
    group_by(.data$sector_abcd, .data$technology, .data$year, ...) %>%
    summarize(
      weighted_percent_change = mean(.data$weighted_loan_percent_change)
    ) %>%
    # Restore old groups
    group_by(!!!dplyr::groups(data))
}

summarize_weighted_emission_factor <- function(data, ..., use_credit_limit = FALSE) {
  stopifnot(is.data.frame(data))

  data %>%
    ungroup() %>%
    add_loan_weight(use_credit_limit = use_credit_limit) %>%
    calculate_weighted_loan_metric("emission_factor") %>%
    group_by(.data$sector_abcd, .data$year, ...) %>%
    summarize(
      emission_factor_projected = sum(.data$weighted_loan_emission_factor)
    ) %>%
    ungroup()
}

summarize_unweighted_emission_factor <- function(data, ...) {

  data <- data %>%
    select(
      -all_of(c("id_loan", "loan_size_credit_limit", "loan_size_outstanding"))
      ) %>%
    distinct() %>%
    group_by(.data$sector_abcd, .data$year, ...) %>%
    summarize(emission_factor_projected = mean(.data$emission_factor)) %>%
    ungroup()
}

calculate_weighted_loan_metric <- function(data, metric) {

  check_crucial_names(data, c(metric, "loan_weight"))

  allowed_missing_vals <- c(
    "production",
    "production_target",
    "technology_share",
    "technology_share_target",
    "percent_change"
  )

  if (metric %in% allowed_missing_vals) {
    no_missing_vals <- "loan_weight"
  } else {
    no_missing_vals <- c("loan_weight", metric)
  }

  walk_(no_missing_vals, ~ check_no_value_is_missing(data, .x))

  data %>%
    mutate(
      weighted_loan_metric = .data[[metric]] * .data$loan_weight
    ) %>%
    rename_metric(metric)
}

add_loan_weight <- function(data, use_credit_limit) {
  stopifnot(isTRUE(use_credit_limit) || isFALSE(use_credit_limit))

  type <- ifelse(use_credit_limit, "credit_limit", "outstanding")
  loan_size <- paste0("loan_size_", type)

  currency <- paste0(loan_size, "_currency") %>%
    check_single_currency(data)

  crucial <- c(
    "id_loan", "sector_abcd", "year", loan_size, currency
  )

  check_crucial_names(data, crucial)
  walk_(crucial, ~ check_no_value_is_missing(data, .x))

  old_groups <- dplyr::groups(data)
  data <- ungroup(data)

  distinct_loans_by_sector <- data %>%
    group_by(.data$sector_abcd) %>%
    distinct(.data$id_loan, .data[[loan_size]]) %>%
    check_unique_loan_size_values_per_id_loan()

  total_size_by_sector <- distinct_loans_by_sector %>%
    summarize(total_size = sum(.data[[loan_size]]))

  data %>%
    left_join(total_size_by_sector, by = "sector_abcd") %>%
    mutate(
      loan_weight = .data[[loan_size]] / .data$total_size
    )
}

add_percent_change <- function(data) {
  crucial <- c("sector_abcd", "year", "technology", "scenario")

  check_crucial_names(data, c("production", crucial))
  walk_(crucial, ~ check_no_value_is_missing(data, .x))

  check_zero_initial_production(data)

  increasing_or_decreasing <- r2dii.data::increasing_or_decreasing

  data %>%
    inner_join(increasing_or_decreasing, by = c(
      sector_abcd = "sector",
      technology = "technology"
    )) %>%
    group_by(.data$sector_abcd, .data$year, .data$scenario) %>%
    mutate(sector_production = sum(.data$production)) %>%
    group_by(.data$sector_abcd, .data$name_abcd) %>%
    arrange(.data$name_abcd, .data$year) %>%
    mutate(
      decreasing_percent_change =
        (.data$production - first(.data$production)) /
          first(.data$production) * 100,
      increasing_percent_change = (.data$production - first(.data$production)) /
        first(.data$sector_production) * 100
    ) %>%
    mutate(percent_change = dplyr::case_when(
      increasing_or_decreasing == "increasing" ~ increasing_percent_change,
      increasing_or_decreasing == "decreasing" ~ decreasing_percent_change
    )) %>%
    select(one_of(c(names(data), "percent_change"))) %>%
    ungroup()
}

add_technology_share <- function(data) {
  data %>%
    group_by(
      .data$sector_abcd,
      .data$year,
      .data$scenario,
      .data$name_abcd,
      .data$region
    ) %>%
    mutate(
      .x = sum(.data$production),
      technology_share = ifelse(.data$.x == 0, 0, .data$production / .data$.x),
      .x = NULL
    ) %>%
    group_by(!!!dplyr::groups(data))
}

add_technology_share_target <- function(data) {
  data %>%
    group_by(
      .data$sector_abcd,
      .data$year,
      .data$scenario,
      .data$name_abcd,
      .data$region
    ) %>%
    mutate(
      .x = sum(.data$production_target),
      technology_share_target = ifelse(.data$.x == 0, 0, .data$production_target / .data$.x),
      .x = NULL
    ) %>%
    group_by(!!!dplyr::groups(data))
}

check_zero_initial_production <- function(data) {
  companies_with_zero_initial_production <- data %>%
    group_by(.data$technology, .data$name_abcd) %>%
    arrange(.data$year) %>%
    filter(.data$year == first(.data$year)) %>%
    summarize(production_at_start_year = sum(.data$production)) %>%
    filter(.data$production_at_start_year == 0)

  if (nrow(companies_with_zero_initial_production) > 0L) {
    abort(
      class = "zero_initial_production",
      "No `name_abcd` by `technology` can have initial `production` values of 0."
    )
  }

  invisible(data)
}

check_unique_loan_size_values_per_id_loan <- function(data) {
  dups <- data %>%
    group_by(.data$sector_abcd, .data$id_loan) %>%
    mutate(is_duplicated = any(duplicated(.data$id_loan))) %>%
    ungroup() %>%
    filter(.data$is_duplicated)

  if (nrow(dups) > 0L) {
    abort(
      class = "multiple_loan_size_values_by_id_loan",
      "Every `id_loan` by `sector` must have unique `loan_size*` values."
    )
  }

  invisible(data)
}

check_single_currency <- function(currency, data) {
  if (n_distinct(data[[currency]]) > 1L) {
    msg <- glue("Column `{currency}` must contain a single currency.")
    abort(msg, class = "multiple_currencies")
  }

  invisible(currency)
}

rename_metric <- function(out, metric) {
  new_name <- paste0("weighted_loan_", metric)
  newnames <- sub("weighted_loan_metric", new_name, names(out))
  rlang::set_names(out, newnames)
}

# We can remove this once we depend on R >= 3.5. See ?backports::isTRUE
isTRUE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}

# We can remove this once we depend on R >= 3.5. See ?backports::isFALSE
isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}
