#' Calculate portfolio targets using the Sectoral Decarbonisation Approach (SDA)
#'
#' The Sectoral Decarbonisation Approach (SDA) is a method for setting corporate
#' emission reduction targets in line with climate science.
#'
#' @param market_data A dataframe like [r2dii.analysis::market].
#' @param port_data A dataframe like [r2dii.analysis::portfolio].
#' @param ref_scenario A character vector giving one or more scenarios to use as
#'   the SDA target.
#' @param ref_geography A character vector giving one or more scenario
#'   geographies for each scenario.
#' @param ref_sector A character vector giving one or more sectors (with
#'   emissions factors) to calculate the SDA. `NULL` defaults to all expected
#'   sectors (see section See Also).
#' @param start_year A length-1 numeric or character vector giving the start
#'   year used in the SDA calculation. `NULL` defaults to extracting the year
#'   from a configuration file (see section See Also).
#' @param target_year A length-1 numeric or character vector giving the end year
#'   used in the SDA calculation. `NULL` defaults to the latest year found in the
#'   `Year` column of `market_data` and `port_data`.
#'
#' @seealso [r2dii.utils::get_config()], [r2dii.utils::START.YEAR()],
#'   [get_ref_sector()].
#'
#' @return Returns a dataframe where the `Scen.Sec.EmissionsFactor` column
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
sda_portfolio_target <- function(market_data,
                                 port_data,
                                 ref_scenario = "B2DS",
                                 ref_geography = "Global",
                                 ref_sector = NULL,
                                 start_year = NULL,
                                 target_year = NULL) {
  stopifnot(is.data.frame(market_data), is.data.frame(port_data))

  ref_sector <- ref_sector %||% get_ref_sector()
  start_year <- start_year %||% r2dii.utils::START.YEAR()
  target_year <- target_year %||% guess_target_year(market_data, port_data)

  check_sda_portfolio_target(
    market_data = market_data,
    port_data = port_data,
    ref_scenario = ref_scenario,
    ref_geography = ref_geography,
    ref_sector = ref_sector,
    start_year = start_year,
    target_year = target_year
  )

  # Prefill common arguments
  startender2 <- purrr::partial(
    startender,
    ref_scenario = ref_scenario,
    ref_sector = ref_sector,
    ref_geography = ref_geography
  )
  ci_port <- port_data %>%
    startender2(var = "Plan.Sec.EmissionsFactor", year = start_year)
  ci_market <- market_data %>%
    startender2(var = "Scen.Sec.EmissionsFactor", year = start_year)
  si <- market_data %>%
    startender2(var = "Scen.Sec.EmissionsFactor", year = target_year) %>%
    rename(SI = .data$CI)

  # Prefill common arguments
  view3 <- purrr::partial(
    view2,
    ref_scenario = ref_scenario,
    ref_sector = ref_sector,
    ref_geography = ref_geography
  )
  port_to_market <- view3(market_data) %>%
    select(-c(.data$Investor.Name, .data$Portfolio.Name)) %>%
    inner_join(
      view3(port_data),
      by = c(get_common_by(), "Year"),
      suffix = c("_port", "_market")
    )

  distance <- ci_market %>%
    inner_join(
      si,
      by = c(get_common_by(), "Investor.Name", "Portfolio.Name")
    ) %>%
    inner_join(
      ci_port,
      by = get_common_by(), suffix = c("_market", "_port")
    ) %>%
    mutate(D_port = .data$CI_port - .data$SI)

  port_to_market %>%
    inner_join(
      distance,
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
    select(
      .data$Investor.Name,
      .data$Portfolio.Name,
      .data$Allocation,
      .data$Sector,
      .data$Scenario,
      .data$ScenarioGeography,
      .data$Year,
      .data$Scen.Sec.EmissionsFactor
    ) %>%
    right_join(
      port_data,
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

check_sda_portfolio_target <- function(market_data,
                                  port_data,
                                  ref_scenario,
                                  ref_geography,
                                  ref_sector,
                                  start_year,
                                  target_year) {
  crucial <- c(
    "Allocation",
    "Investor.Name",
    "Plan.Sec.EmissionsFactor",
    "Portfolio.Name",
    "Scen.Sec.EmissionsFactor",
    "Scenario",
    "ScenarioGeography",
    "Sector",
    "Year"
  )
  r2dii.utils::check_crucial_names(market_data, crucial)
  r2dii.utils::check_crucial_names(port_data, crucial)

  check_ref(market_data, port_data, ref = ref_scenario, col = "Scenario")
  check_ref(
    market_data, port_data,
    ref = ref_geography, col = "ScenarioGeography"
  )

  abort_null_start_year(start_year)

  abort_bad_year(start_year)
  abort_bad_year(target_year)

  warn_missing_sectors(port_data, ref_sector)
}

check_ref <- function(market_data, port_data, ref, col) {
  ref_has_length_1 <- identical(length(ref), 1L)
  stopifnot(ref_has_length_1)

  valid <- sort(unique(c(market_data[[col]], port_data[[col]])))
  is_valid <- any(ref %in% valid)
  if (!is_valid) {
    stop(
      "Wrong 'ref_*' argument (", ref, "). Must be one of:\n",
      paste0(valid, collapse = ", "),
      call. = FALSE
    )
  }
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

guess_target_year <- function(market_data, port_data) {
  max(max(as.integer(market_data$Year)), max(as.integer(port_data$Year)))
}

abort_bad_year <- function(year) {
  stopifnot(
    is.character(year) || is.numeric(year), identical(length(year), 1L)
  )

  invisible(year)
}

warn_missing_sectors <- function(port_data, ref_sector) {
  missing_ref_sector <- sort(setdiff(ref_sector, port_data$Sector))

  if (length(missing_ref_sector) > 0L) {
    warning(
      "Can't calculate SDA for `ref_sector` values missing from `port_data`:\n",
      paste0(missing_ref_sector, collapse = ", "), ".",
      call. = FALSE
    )
  }

  invisible(port_data)
}

#' Default value for the `ref_sector` argument to [sda_portfolio_target()]
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' get_ref_sector()
get_ref_sector <- function() {
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

startender <- function(data,
                       var,
                       year,
                       ref_scenario,
                       ref_sector,
                       ref_geography) {
  data %>%
    filter(
      !is.na(!!sym(var)) &
        as.character(.data$Year) == as.character(year) &
        .data$Scenario %in% ref_scenario &
        .data$Sector %in% ref_sector &
        .data$ScenarioGeography %in% ref_geography
    ) %>%
    rename(CI = !!sym(var)) %>%
    distinct(
      .data$Investor.Name,
      .data$Portfolio.Name,
      .data$CI,
      .data$Scenario,
      .data$ScenarioGeography,
      .data$Sector,
      .data$Allocation
    )
}

view2 <- function(data, ref_scenario, ref_sector, ref_geography) {
  data %>%
    filter(
      .data$Scenario %in% ref_scenario &
        .data$Sector %in% ref_sector &
        .data$ScenarioGeography %in% ref_geography
    ) %>%
    distinct(
      .data$Investor.Name,
      .data$Portfolio.Name,
      .data$Allocation,
      .data$Sector,
      .data$Scenario,
      .data$ScenarioGeography,
      .data$Year,
      .data$Scen.Sec.EmissionsFactor
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
