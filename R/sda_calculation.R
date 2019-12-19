#' TODO \@vintented
#'
#' @param market_data A dataframe with market results.
#' @param port_data A dataframe with portfolio results data.
#' @param ref_sector A character vector giving one or more sectors (with
#'   emissions factors) to calculate the SDA.
#' @param ref_scenario A character vector giving one or more scenarios to use as
#'   the SDA target.
#' @param ref_geography A character vector giving one or more scenario
#'   geographies for each scenario.
#' @param start_year A length-1 numeric or character vector giving the start
#'   year used in the SDA calculation as a numeric (i.e. start_year == 2020).
#' @param target_year A length-1 numeric or character vector giving the end year
#'   used in the SDA calculation as a numeric (i.e. target_year == 2045). By
#'   default the function will use the last year in the current market data.
#'
#' @seealso [r2dii.utils::get_config()], [r2dii.utils::START.YEAR()]
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
#' sda_calculation(market, portfolio)
#'
#' sda_calculation(market, portfolio, ref_sector = "Steel")
#'
#' # This configuration file lacks `start_year`
#' options(r2dii_config = example_config("config-toy.yml"))
#' START.YEAR()
#'
#' # Fails
#' try(sda_calculation(market, portfolio))
#'
#' # Passes
#' sda_calculation(market, portfolio, start_year = "2019")
sda_calculation <- function(market_data,
                            port_data,
                            ref_sector = get_ref_sector(),
                            ref_scenario = "B2DS",
                            ref_geography = "Global",
                            start_year = r2dii.utils::START.YEAR(),
                            target_year = 2040) {
  abort_null_start_year(start_year)
  abort_bad_year(start_year)
  abort_bad_year(target_year)
  warn_if_missing_sectors(port_data, ref_sector)

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
      si, by = c(get_common_by(), "Investor.Name", "Portfolio.Name")
    ) %>%
    inner_join(
      ci_port, by = get_common_by(), suffix = c("_market", "_port")
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
      P_market  = (.data$Scen.Sec.EmissionsFactor_market - .data$SI) /
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

abort_bad_year <- function(year) {
  stopifnot(
    is.character(year) || is.numeric(year), identical(length(year), 1L)
  )

  invisible(year)
}

warn_if_missing_sectors <- function(port_data, ref_sector) {
  missing_ref_sector <- sort(setdiff(ref_sector, port_data$Sector))

  if(length(missing_ref_sector) > 0L) {
    warning(
      "Can't calculate SDA for `ref_sector` values missing from `port_data`:\n",
      paste0(missing_ref_sector, collapse = ", "), ".",
      call. = FALSE
    )
  }

  invisible(port_data)
}

#' Default value for the `ref_sector` argument to [sda_calculation()]
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



