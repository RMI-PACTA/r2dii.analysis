#' Calculate portfolio targets using the Sectoral Decarbonisation Approach (SDA)
#'
#' The Sectoral Decarbonisation Approach (SDA) is a method for setting corporate
#' emission reduction targets in line with climate science.
#'
#' @param market A dataframe like [r2dii.analysis::market].
#' @param portfolio A dataframe like [r2dii.analysis::portfolio].
#' @param ref_scenario A character vector giving one or more scenarios to use as
#'   the SDA target.
#' @param ref_geography A character vector giving one or more scenario
#'   geographies for each scenario.
#' @param ref_sector A character vector giving one or more sectors present in
#'   both `market` and `portfolio` data. `NULL` defaults to all expected sectors
#'   (see section See Also).
#' @param start_year A length-1 numeric or character vector giving the start
#'   year used in the SDA calculation. `NULL` defaults to extracting the year
#'   from a configuration file (see section See Also).
#' @param target_year A length-1 numeric or character vector giving the end year
#'   used in the SDA calculation. It must be a year present in all `ref_sectors`
#'   of `market`. `NULL` defaults to the latest year shared across all sectors
#'   given by `ref_sector` and found in `market`.
#'
#' @seealso [r2dii.utils::get_config()], [r2dii.utils::START.YEAR()],
#'   [get_sectors()].
#'
#' @return Returns a dataframe where the     `Scen.Sec.EmissionsFactor` column
#'   holds the result of the SDA calculation.
#'
#' @export
#'
#' @examples
#' # Use example configuration-files
#' library(r2dii.utils)
#'
#' # Use a toy configuration file
#' restore_options <- options(r2dii_config = example_config("config_demo.yml"))
#' on.exit(restore_options)
#'
#' # Use `start_year` from the configuration file
#' START.YEAR()
#' sda_portfolio_target(market, portfolio)
#'
#' sda_portfolio_target(market, portfolio, ref_sector = "Steel")
#'
#' # This configuration file lacks `start_year`
#' options(r2dii_config = example_config("config-toy.yml"))
#' START.YEAR()
#'
#' # Fails
#' try(sda_portfolio_target(market, portfolio))
#'
#' # Passes
#' sda_portfolio_target(market, portfolio, start_year = "2019")
sda_portfolio_target <- function(market,
                                 portfolio,
                                 ref_scenario = "B2DS",
                                 ref_geography = "Global",
                                 ref_sector = NULL,
                                 start_year = NULL,
                                 target_year = NULL) {
  check_market_and_portfolio(market, portfolio, crucial = get_crucial_vars())
  check_ref(market, portfolio, ref = ref_scenario, col = "Scenario")
  check_ref(market, portfolio, ref = ref_geography, col = "ScenarioGeography")

  ref_sector <- validate_ref_sector(market, portfolio, ref_sector = ref_sector)
  message("* Using `ref_sector`:", paste0(ref_sector, collapse = ", "), ".")

  start_year <- validate_start_year(market, portfolio, start_year)
  message("* Using `start_year`:", start_year, ".")

  target_year <- validate_target_year(
    target_year, find_useful_years_by_sector(market, ref_sector = ref_sector)
  )
  message("* Using `target_year`:", target_year, ".")



  distinct_vars <- c(get_common_vars(), "Scen.Sec.EmissionsFactor", "Year")

  port_to_market <- create_port_to_market(
    market = market,
    portfolio = portfolio,
    distinct_vars = distinct_vars,
    ref_scenario = ref_scenario,
    ref_sector = ref_sector,
    ref_geography = ref_geography
  )

  distance <- create_distance(
    market = market,
    portfolio = portfolio,
    ref_scenario = ref_scenario,
    ref_sector = ref_sector,
    ref_geography = ref_geography,
    start_year = start_year,
    target_year = target_year
  )

  porttomarket_distance <- inner_join(
    port_to_market, distance,
    by = c(
      get_common_by(),
      "Investor.Name" = "Investor.Name_port",
      "Portfolio.Name" = "Portfolio.Name_port"
    )
  ) %>%
    mutate(
      P_market = (.data$Scen.Sec.EmissionsFactor_market - .data$SI) /
        (.data$CI_market - .data$SI),
      Scen.Sec.EmissionsFactor = (.data$D_port * 1 * .data$P_market) + .data$SI
    ) %>%
    select(!!! distinct_vars)

  right_join(
    porttomarket_distance, portfolio,
    by = c(get_common_by(), "Investor.Name", "Portfolio.Name", "Year"),
    suffix = c("", "_no_sda")
  ) %>%
    mutate(
      Scen.Sec.EmissionsFactor =
        if_else(
          !is.na(.data$Scen.Sec.EmissionsFactor),
          .data$Scen.Sec.EmissionsFactor,
          .data$Scen.Sec.EmissionsFactor_no_sda
        )
    ) %>%
    select(-.data$Scen.Sec.EmissionsFactor_no_sda)
}

check_market_and_portfolio <- function(market, portfolio, crucial) {
  stopifnot(is.data.frame(market), is.data.frame(portfolio))
  r2dii.utils::check_crucial_names(market, crucial)
  r2dii.utils::check_crucial_names(portfolio, crucial)
}

check_ref <- function(market, portfolio, ref, col) {
  ref_has_length_1 <- identical(length(ref), 1L)
  stopifnot(ref_has_length_1)

  valid <- sort(unique(c(market[[col]], portfolio[[col]])))
  is_valid <- any(ref %in% valid)
  if (!is_valid) {
    stop(
      "Wrong 'ref_*' argument (", ref, "). Must be one of:\n",
      paste0(valid, collapse = ", "),
      call. = FALSE
    )
  }
}

validate_ref_sector <- function(market, portfolio, ref_sector) {
  ref_sector <- ref_sector %||% get_sectors()
  useful <- intersect(market$Sector, portfolio$Sector)

  ref_sector_in_data <- intersect(ref_sector, useful)
  using <- abort_cant_find(ref_sector_in_data)

  warn_unused_sector(setdiff(ref_sector, useful))
  using
}

abort_cant_find <- function(using) {
  is_found <- length(using) > 0L
  stopifnot(is_found)
  invisible(using)
}

warn_unused_sector <- function(unused) {
  if (length(unused) > 0L) {
    warning(
      "Skipping sectors not present in both `market` and `portfolio`:\n",
      paste0(unused, collapse = ", "), ".",
      call. = FALSE
    )
  }

  invisible(unused)
}

validate_start_year <- function(market, portfolio, start_year) {
  start_year <- start_year %||% r2dii.utils::START.YEAR()
  abort_null_start_year(start_year)
  abort_bad_year(market, start_year)
  start_year
}

abort_null_start_year <- function(start_year) {
  if (is.null(start_year)) {
    stop(
      "`start_year` can't be NULL.\n",
      "Did you forget `start_year` in a configuration file or as an argument?",
      call. = FALSE
    )
  }

  invisible(start_year)
}

find_year_shared_across_sectors <- function(market, target_year, ref_sector) {
  useful_years <- find_useful_years_by_sector(market, ref_sector = ref_sector)
  validate_target_year(target_year, useful_years)
}

find_useful_years_by_sector <- function(market, ref_sector) {
  picked_sectors <- filter(market, .data$Sector %in% ref_sector)
  years_by_sector <- split(picked_sectors$Year, picked_sectors$Sector)
  purrr::reduce(years_by_sector, intersect)
}

validate_target_year <- function(target_year, useful_years) {
  if (is.null(target_year)) {
    return(max(useful_years))
  }

  target_year_has_length_1 <- identical(length(target_year), 1L)
  stopifnot(target_year_has_length_1)

  best_target_year <- intersect(target_year, useful_years)
  is_target_year_shared_across_sectors <- length(best_target_year) > 0L
  stopifnot(is_target_year_shared_across_sectors)

  best_target_year
}

abort_bad_year <- function(data, year) {
  year_has_length_1 <- identical(length(year), 1L)
  stopifnot(year_has_length_1, is.character(year) || is.numeric(year))

  is_year_in_data <- any(year %in% unique(data$Year))
  stopifnot(is_year_in_data)

  invisible(year)
}

create_distance <- function(market,
                            portfolio,
                            ref_scenario,
                            ref_sector,
                            ref_geography,
                            start_year,
                            target_year) {
  distinct_vars <- dplyr::vars(!!! syms(get_common_vars()), .data$CI)

  ci_port <- portfolio %>%
    pick_ref_scenario_sector_and_geography(ref_scenario, ref_sector, ref_geography) %>%
    filter(as.character(.data$Year) == as.character(start_year)) %>%
    filter(!is.na(.data[["Plan.Sec.EmissionsFactor"]])) %>%
    rename(CI = .data$Plan.Sec.EmissionsFactor) %>%
    distinct(!!! distinct_vars)

  ci_market <- market %>%
    filter(as.character(.data$Year) == as.character(start_year)) %>%
    filter(!is.na(.data[["Scen.Sec.EmissionsFactor"]])) %>%
    rename(CI = .data$Scen.Sec.EmissionsFactor) %>%
    distinct(!!! distinct_vars)

  si <- market %>%
    filter(as.character(.data$Year) == as.character(target_year)) %>%
    filter(!is.na(.data[["Scen.Sec.EmissionsFactor"]])) %>%
    rename(CI = .data$Scen.Sec.EmissionsFactor) %>%
    distinct(!!! distinct_vars) %>%
    rename(SI = .data$CI)

  cimarket_si <- inner_join(
    ci_market, si,
    by = c(get_common_by(), "Investor.Name", "Portfolio.Name")
  )

  inner_join(
    cimarket_si, ci_port,
    by = get_common_by(), suffix = c("_market", "_port")
  ) %>%
    mutate(D_port = .data$CI_port - .data$SI)
}

create_port_to_market <- function(market,
                                  portfolio,
                                  distinct_vars,
                                  ref_scenario,
                                  ref_sector,
                                  ref_geography) {
  lhs <- market %>%
    distinct(!!! syms(distinct_vars)) %>%
    pick_ref_scenario_sector_and_geography(ref_scenario, ref_sector, ref_geography) %>%
    select(-c(.data$Investor.Name, .data$Portfolio.Name))

  rhs <- portfolio %>%
    distinct(!!! syms(distinct_vars))

  port_to_market <- inner_join(
    lhs, rhs, by = c(get_common_by(), "Year"), suffix = c("_port", "_market")
  )
}

#' Default value for the `ref_sector` argument to [sda_portfolio_target()]
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' get_sectors()
get_sectors <- function() {
  c(
    "Cement",
    "Steel",
    "Power",
    "Oil&Gas",
    "Coal",
    "Aviation",
    "FossilFuels",
    "Shipping",
    "Automotive"
  )
}

get_common_by <- function() {
  c(
    "Allocation",
    "Sector",
    "Scenario",
    "ScenarioGeography"
  )
}

get_common_vars <- function() {
  c(
    get_common_by(),
    "Investor.Name",
    "Portfolio.Name"
  )
}

get_crucial_vars <- function() {
  crucial <- c(
    get_common_vars(),
    "Plan.Sec.EmissionsFactor",
    "Scen.Sec.EmissionsFactor",
    "Year"
  )
}

pick_ref_scenario_sector_and_geography <- function(data,
                                                   ref_scenario,
                                                   ref_sector,
                                                   ref_geography) {
  data %>%
    filter(
      .data$Scenario %in% ref_scenario &
        .data$Sector %in% ref_sector &
        .data$ScenarioGeography %in% ref_geography
    )
}
