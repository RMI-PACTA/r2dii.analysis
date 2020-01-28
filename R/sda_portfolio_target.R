#' Calculate portfolio targets using the Sectoral Decarbonisation Approach (SDA)
#'
#' The Sectoral Decarbonisation Approach (SDA) is a method for setting corporate
#' emission reduction targets in line with climate science.
#'
#' @param market A dataframe like [r2dii.analysis::market].
#' @param portfolio A dataframe like [r2dii.analysis::portfolio].
#' @param scenario A character vector giving one or more scenarios to use as
#'   the SDA target.
#' @param geography A character vector giving one or more scenario
#'   geographies for each scenario.
#' @param sector A character vector giving one or more sectors present in
#'   both `market` and `portfolio` data. `NULL` defaults to all expected sectors
#'   (see section See Also).
#' @param start_year A length-1 numeric or character vector giving the start
#'   year used in the SDA calculation. `NULL` defaults to extracting the year
#'   from a configuration file (see section See Also).
#' @param target_year A length-1 numeric or character vector giving the end year
#'   used in the SDA calculation. It must be a year present in all `sectors`
#'   of `market`. `NULL` defaults to the latest year shared across all sectors
#'   given by `sector` and found in `market`.
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
#' sda_portfolio_target(market, portfolio, sector = "Steel")
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
                                 scenario = "B2DS",
                                 geography = "Global",
                                 sector = NULL,
                                 start_year = NULL,
                                 target_year = NULL) {
  check_market_portfolio(market, portfolio, crucial = get_sda_crucial_vars())
  check_ref(market, portfolio, ref = scenario, col = "Scenario")
  check_ref(market, portfolio, ref = geography, col = "ScenarioGeography")

  sector <- validate_sector(market, portfolio, sector = sector)
  message("* Using `sector`:", paste0(sector, collapse = ", "), ".")
  start_year <- validate_start_year(market, portfolio, start_year)
  message("* Using `start_year`:", start_year, ".")
  target_year <- validate_target_year_by_sector(market, target_year, sector)
  message("* Using `target_year`:", target_year, ".")

  distinct_vars <- c(get_sda_common_vars(), "Scen.Sec.EmissionsFactor", "Year")

  port_to_market <- create_port_to_market(
    market = market,
    portfolio = portfolio,
    distinct_vars = distinct_vars,
    scenario = scenario,
    sector = sector,
    geography = geography
  )

  distance <- create_distance(
    market = market,
    portfolio = portfolio,
    scenario = scenario,
    sector = sector,
    geography = geography,
    start_year = start_year,
    target_year = target_year
  )

  right_join(
    create_porttomarket_distance(port_to_market, distance, distinct_vars),
    portfolio,
    by = c(get_sda_common_by(), "Investor.Name", "Portfolio.Name", "Year"),
    suffix = c("", "_no_sda")
  ) %>%
    select(-.data$Scen.Sec.EmissionsFactor_no_sda)
}

check_market_portfolio <- function(market, portfolio, crucial) {
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

validate_sector <- function(market, portfolio, sector) {
  sector <- sector %||% get_sectors()
  useful <- intersect(market$Sector, portfolio$Sector)

  sector_in_data <- intersect(sector, useful)
  using <- abort_cant_find(sector_in_data)

  warn_unused_sector(setdiff(sector, useful))
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

find_years_by_sector <- function(market, sector) {
  picked_sectors <- filter(market, .data$Sector %in% sector)
  years_by_sector <- split(picked_sectors$Year, picked_sectors$Sector)
  purrr::reduce(years_by_sector, intersect)
}

validate_target_year_by_sector <- function(market, target_year, sector) {
  useful_years <- find_years_by_sector(market, sector = sector)

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
                            scenario,
                            sector,
                            geography,
                            start_year,
                            target_year) {
  distinct_vars <- dplyr::vars(!!! syms(get_sda_common_vars()), .data$CI)

  ci_port <- portfolio %>%
    pick_scenario_sector_and_geography(scenario, sector, geography) %>%
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
    by = c(get_sda_common_by(), "Investor.Name", "Portfolio.Name")
  )

  inner_join(
    cimarket_si, ci_port,
    by = get_sda_common_by(), suffix = c("_market", "_port")
  ) %>%
    mutate(D_port = .data$CI_port - .data$SI)
}

create_port_to_market <- function(market,
                                  portfolio,
                                  distinct_vars,
                                  scenario,
                                  sector,
                                  geography) {
  lhs <- market %>%
    distinct(!!! syms(distinct_vars)) %>%
    filter(
      as.character(.data$Year) >= as.character(start_year) &
        as.character(.data$Year) <= as.character(target_year)
      ) %>%
    pick_scenario_sector_and_geography(scenario, sector, geography) %>%
    select(-c(.data$Investor.Name, .data$Portfolio.Name))

  rhs <- portfolio %>%
    distinct(!!! syms(distinct_vars))

  port_to_market <- inner_join(
    lhs, rhs, by = c(get_sda_common_by(), "Year"), suffix = c("_port", "_market")
  )
}

create_porttomarket_distance <- function(port_to_market, distance, distinct_vars) {
  porttomarket_distance <- inner_join(
    port_to_market, distance,
    by = c(
      get_sda_common_by(),
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
}

#' Default value for the `sector` argument to [sda_portfolio_target()]
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

get_sda_common_by <- function() {
  c(
    "Allocation",
    "Sector",
    "Scenario",
    "ScenarioGeography"
  )
}

get_sda_common_vars <- function() {
  c(
    get_sda_common_by(),
    "Investor.Name",
    "Portfolio.Name"
  )
}

get_sda_crucial_vars <- function() {
  crucial <- c(
    get_sda_common_vars(),
    "Plan.Sec.EmissionsFactor",
    "Scen.Sec.EmissionsFactor",
    "Year"
  )
}

pick_scenario_sector_and_geography <- function(data,
                                                   scenario,
                                                   sector,
                                                   geography) {
  data %>%
    filter(
      .data$Scenario %in% scenario &
        .data$Sector %in% sector &
        .data$ScenarioGeography %in% geography
    )
}

remove_plan_sec_emissionsfactor_after_target_year <- function(out, target_year) {
  out[out$Year > target_year, "Plan.Sec.EmissionsFactor"] <- NA_real_
  out
}
