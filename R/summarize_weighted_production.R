#' Summaries based on the weight of each loan per sector per year
#'
#' Based on on the weight of each loan per sector per year,
#' `summarize_weighted_production()` and `summarize_weighted_percent_change()`
#' summarize the production and percent-change, respectively.
#'
#' @param data A data frame like the output of [join_ald_scenario()].
#' @param use_credit_limit Logical vector of length 1. `FALSE` defaults to using
#'   the column `loan_size_outstanding`. Set to `TRUE` to instead use the column
#'   `loan_size_credit_limit`.
#' @param ... Variables to group by.
#'
#' @seealso [join_ald_scenario()].
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
#'   join_ald_scenario(
#'     ald = ald_demo,
#'     scenario = scenario_demo_2020,
#'     region_isos = region_isos_demo
#'   )
#'
#' summarize_weighted_production(master)
#'
#' summarize_weighted_production(master, use_credit_limit = TRUE)
#'
#' summarize_weighted_percent_change(master)
#'
#' summarize_weighted_percent_change(master, use_credit_limit = TRUE)
summarize_weighted_production <- function(data, ..., use_credit_limit = FALSE) {
  summarize_weighted_metric(
    data = data,
    group_dots = rlang::enquos(...),
    use_credit_limit = use_credit_limit,
    .f = add_weighted_loan_production,
    weighted_production = sum(.data$weighted_loan_production),
    weighted_technology_share = sum(.data$weighted_technology_share)
  )
}

summarize_unweighted_production <- function(data, ...) {
  data %>%
    select(-c(.data$id_loan, .data$level)) %>%
    distinct() %>%
    group_by(...) %>%
    summarize(weighted_production = sum(.data$production), .groups = "keep") %>%
    ungroup(.data$technology) %>%
    mutate(weighted_technology_share = .data$weighted_production / sum(.data$weighted_production)) %>%
    group_by(!!!dplyr::groups(data))
}

#' @rdname summarize_weighted_production
#' @export
summarize_weighted_percent_change <- function(data, ..., use_credit_limit = FALSE) {
  summarize_weighted_metric(
    data = data,
    group_dots = rlang::enquos(...),
    use_credit_limit = use_credit_limit,
    .f = add_weighted_loan_percent_change,
    weighted_percent_change = mean(.data$weighted_loan_percent_change)
  )
}

summarize_weighted_metric <- function(data,
                                      group_dots,
                                      use_credit_limit,
                                      .f,
                                      ...) {
  data %>%
    .f(use_credit_limit = use_credit_limit) %>%
    group_by(.data$sector_ald, .data$technology, .data$year, !!!group_dots) %>%
    summarize(...) %>%
    # Restore old groups
    group_by(!!!dplyr::groups(data))
}

add_weighted_loan_percent_change <- function(data, use_credit_limit = FALSE) {
  add_weighted_loan_metric(data, use_credit_limit, metric = "percent_change")
}

add_weighted_loan_production <- function(data, use_credit_limit = FALSE) {
  add_weighted_loan_metric(data, use_credit_limit, metric = "production")
}

add_weighted_loan_emission_factor <- function(data, use_credit_limit = FALSE) {
  add_weighted_loan_metric(data, use_credit_limit, metric = "emission_factor")
}

add_weighted_loan_metric <- function(data,
                                     use_credit_limit,
                                     metric = c(
                                       "production",
                                       "percent_change",
                                       "emission_factor"
                                     )) {
  stopifnot(
    is.data.frame(data), isTRUE(use_credit_limit) || isFALSE(use_credit_limit)
  )

  type <- ifelse(use_credit_limit, "credit_limit", "outstanding")
  loan_size <- paste0("loan_size_", type)

  currency <- paste0(loan_size, "_currency") %>%
    check_single_currency(data)

  metrics <- c("production", "percent_change", "emission_factor")
  metric <- rlang::arg_match(metric, metrics)

  crucial <- c(
    "id_loan", "production", "sector_ald", "year", loan_size, currency
  )
  if (metric %in% c("production", "percent_change")) {
    crucial <- c(crucial, "technology")
  }
  if (metric == "percent_change") {
    crucial <- c(crucial, "scenario")
  }
  check_crucial_names(data, crucial)
  walk_(crucial, ~ check_no_value_is_missing(data, .x))

  if (metric == "percent_change") {
    check_zero_initial_production(data)
  }

  old_groups <- dplyr::groups(data)
  data <- ungroup(data)

  distinct_loans_by_sector <- data %>%
    ungroup() %>%
    group_by(.data$sector_ald) %>%
    distinct(.data$id_loan, .data[[loan_size]]) %>%
    check_unique_loan_size_values_per_id_loan()

  total_size_by_sector <- distinct_loans_by_sector %>%
    summarize(total_size = sum(.data[[loan_size]]))

  out <- data

  if (metric == "percent_change") {
    out <- add_percent_change(out)
  }

  if (metric == "production") {
    out <- add_technology_share(out)
  }

  out <- out %>%
    left_join(total_size_by_sector, by = "sector_ald") %>%
    mutate(
      loan_weight = .data[[loan_size]] / .data$total_size,
      weighted_loan_metric = .data[[metric]] * .data$loan_weight
    )

  if (metric == "production") {
    out <- out %>%
      mutate(
        weighted_technology_share = .data$technology_share * .data$loan_weight
      )
  }

  out %>%
    group_by(!!!old_groups) %>%
    rename_metric(metric)
}

add_percent_change <- function(data) {
  green_or_brown <- r2dii.data::green_or_brown

  data %>%
    inner_join(green_or_brown, by = c(
      sector_ald = "sector",
      technology = "technology"
    )) %>%
    group_by(.data$sector_ald, .data$year, .data$scenario) %>%
    mutate(sector_production = sum(.data$production)) %>%
    group_by(.data$sector_ald, .data$name_ald) %>%
    arrange(.data$name_ald, .data$year) %>%
    mutate(
      brown_percent_change =
        (.data$production - first(.data$production)) /
          first(.data$production) * 100,
      green_percent_change = (.data$production - first(.data$production)) /
        first(.data$sector_production) * 100
    ) %>%
    mutate(percent_change = dplyr::case_when(
      green_or_brown == "green" ~ green_percent_change,
      green_or_brown == "brown" ~ brown_percent_change
    )) %>%
    select(one_of(c(names(data), "percent_change"))) %>%
    ungroup()
}

add_technology_share <- function(data) {
  data %>%
    group_by(.data$sector_ald, .data$year, .data$scenario, .data$name_ald) %>%
    mutate(technology_share = .data$production / sum(.data$production)) %>%
    group_by(!!!dplyr::groups(data))
}

check_zero_initial_production <- function(data) {
  companies_with_zero_initial_production <- data %>%
    group_by(.data$technology, .data$name_ald, .data$year) %>%
    arrange(.data$year) %>%
    filter(.data$year == first(.data$year)) %>%
    summarize(production_at_start_year = sum(.data$production)) %>%
    filter(.data$production_at_start_year == 0)

  if (nrow(companies_with_zero_initial_production) > 0L) {
    abort(
      class = "zero_initial_production",
      "No `name_ald` by `technology` can have initial `production` values of 0."
    )
  }

  invisible(data)
}

check_unique_loan_size_values_per_id_loan <- function(data) {
  dups <- data %>%
    group_by(.data$sector_ald, .data$id_loan) %>%
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
