#' TODO \@vintented
#'
#' @param market_data TODO \@vintented
#' @param port_data TODO \@vintented
#' @param ref_sector TODO \@vintented
#' @param ref_scenario TODO \@vintented
#' @param ref_geography TODO \@vintented
#' @param start_year TODO \@vintented
#' @param target_year TODO \@vintented
#'
#' @return TODO \@vintented
#'
#' @export
#'
#' @examples
#' sda_calculation(
#'   r2dii.analysis::market,
#'   r2dii.analysis::portfolio
#' )
sda_calculation <- function(market_data,
                            port_data,
                            ref_sector = c("Cement", "Steel"),
                            ref_scenario = "B2DS",
                            ref_geography = "Global",
                            start_year = get_current_year(),
                            target_year = 2040) {
  # Prefill with common arguments
  startender2 <- purrr::partial(
    startender,
    ref_scenario = ref_scenario,
    ref_sector = ref_sector,
    ref_geography = ref_geography
  )
  ci_port <- port_data %>%
    startender2(var = "Plan.Sec.EmissionsFactor", year = start_year)
  ci_market <- market_data %>%
    startender2(var = "Plan.Sec.EmissionsFactor", year = start_year)
  si <- market_data %>%
    startender2(var = "Scen.Sec.EmissionsFactor", year = target_year) %>%
    rename(SI = .data$CI)

  # Prefill with common arguments
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

  port_to_distance <- port_to_market %>%
    inner_join(
      distance,
      by = c(
        get_common_by(),
        "Investor.Name" = "Investor.Name_port",
        "Portfolio.Name" = "Portfolio.Name_port"
        )
    )

  port_calculation <- port_to_distance %>%
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
    )

  port_calculation %>%
    right_join(
      port_data,
      by = c(get_common_by(), "Investor.Name", "Portfolio.Name", "Year"),
      suffix = c("", "_sda")
    ) %>%
    mutate(
      Scen.Sec.EmissionsFactor =
        if_else(
          !is.na(.data$Scen.Sec.EmissionsFactor_sda),
          .data$Scen.Sec.EmissionsFactor_sda,
          .data$Scen.Sec.EmissionsFactor
        )
    ) %>%
    select(-.data$Scen.Sec.EmissionsFactor_sda)
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
        .data$Year == year &
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
