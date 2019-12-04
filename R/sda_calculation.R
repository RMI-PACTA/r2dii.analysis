#' @export
sda_calculation <- function(market, port, ref_sectors = c("Cement", "Steel"), ref_scenario = "SDS", start_year = 2019, target_year = 2050)  {

  startender <- function(input, var = Plan.Sec.EmissionsFactor, sectors = ref_sectors, year = start_year, scenario = ref_scenario) {
    var <- enquo(var)

    output <- input %>%
      dplyr::filter(!is.nan(!!var) & Year == year & Scenario %in% scenario & Sector %in% sectors & ScenarioGeography == "Global") %>%
      dplyr::rename(CI = !!var) %>%
      dplyr::distinct(Investor.Name, Portfolio.Name, CI, Scenario, Sector, Allocation)


    return(output)
  }


  CI_port <- startender(input = port)
  CI_market <- startender(input = market) %>%
    dplyr::filter(Portfolio.Name == "GlobalMarket")
  SI <- startender(input = market, year = target_year, var = Scen.Sec.EmissionsFactor) %>%
    dplyr::filter(Portfolio.Name == "GlobalMarket") %>%
    dplyr::rename(SI = CI)

  Distance <- CI_port %>%
    dplyr::inner_join(CI_market, by = c("Sector", "Scenario", "Allocation"), suffix = c("_port", "_market")) %>%
    dplyr::inner_join(SI, by = c("Sector", "Scenario", "Allocation", "Portfolio.Name_market" = "Portfolio.Name", "Investor.Name_market" = "Investor.Name")) %>%
    dplyr::mutate(D_port = CI_port - SI)

  view <- function(input = port, scenario = ref_scenario, sectors = ref_sectors) {
    output <- input %>%
      dplyr::filter(Scenario == scenario & Sector %in% ref_sectors) %>%
      dplyr::distinct(Investor.Name, Portfolio.Name, Allocation, Sector, Year, Scen.Sec.EmissionsFactor)
  }

  market <- view(input = market, scenario = ref_scenario, sectors = ref_sectors)
  port <- view(input = port)


  port_to_market <- port %>%
    dplyr::inner_join(market, by = c("Sector", "Year", "Allocation"), suffix = c("_port", "_market")) %>%
    dplyr::inner_join(Distance, by = c("Sector", "Investor.Name_port", "Portfolio.Name_port",  "Investor.Name_market", "Portfolio.Name_market", "Allocation")) %>%
    dplyr::mutate(P_market  = (Scen.Sec.EmissionsFactor_market - SI)/(CI_market - SI)) %>%
    dplyr::mutate(Scen.Sec.EmissionsFactor_port = (D_port*1*P_market)+SI)

  return(port_to_market)

}
