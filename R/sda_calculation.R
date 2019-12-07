sda_calculation <- function(market_data, port_data, ref_sector = c("Cement", "Steel"), ref_scenario = "B2DS", ref_geography = "Global", start_year = 2019, target_year = 2040)  {

  startender <- function(input_data, var = Plan.Sec.EmissionsFactor, year = start_year) {

    var <- enquo(var)

    output_data <- input_data %>%
      filter(!is.nan(!!var) & Year == year & Scenario %in% ref_scenario & Sector %in% ref_sector & ScenarioGeography %in% ref_geography) %>%
      rename(CI = !!var) %>%
      distinct(Investor.Name, Portfolio.Name, CI, Scenario, ScenarioGeography, Sector, Allocation)

    return(output_data)
  }


  CI_port <- startender(input_data = port_data)
  CI_market <- startender(input_data = market_data)
  SI <- startender(input_data = market_data, year = target_year, var = Scen.Sec.EmissionsFactor) %>%
    rename(SI = CI)

  Distance <- CI_market %>%
    inner_join(SI, by = c("Sector", "Scenario", "Allocation", "Portfolio.Name",  "Investor.Name", "ScenarioGeography")) %>%
    inner_join(CI_port, by = c("Sector", "Scenario", "Allocation", "ScenarioGeography"), suffix = c("_market", "_port")) %>%
    mutate(D_port = CI_port - SI)

  view <- function(input_data = port_data) {

    output_data <- input_data %>%
      filter(Scenario %in% ref_scenario & Sector %in% ref_sector & ScenarioGeography %in% ref_geography) %>%
      distinct(Investor.Name, Portfolio.Name, Allocation, Sector, Scenario, ScenarioGeography, Year, Scen.Sec.EmissionsFactor)

    return(output_data)
  }

  market_view <- view(input_data = market_data)
  port_view <- view(input_data = port_data)


  port_to_market <- market_view %>%
    select(-c(Investor.Name, Portfolio.Name)) %>%
    inner_join(port_view, by = c("Sector", "Year", "Allocation", "ScenarioGeography", "Scenario"), suffix = c("_port", "_market"))

  port_to_distance <- port_to_market %>%
    inner_join(Distance, by = c("Scenario", "Sector", "Investor.Name" = "Investor.Name_port", "Portfolio.Name" = "Portfolio.Name_port", "Allocation", "ScenarioGeography"))

  port_calculation <- port_to_distance %>%
    mutate(P_market  = (Scen.Sec.EmissionsFactor_market - SI)/(CI_market - SI),
           Scen.Sec.EmissionsFactor = (D_port*1*P_market)+SI)

  port_calculation <- port_calculation %>%
    select(Investor.Name, Portfolio.Name, Allocation, Sector, Scenario, ScenarioGeography, Year, Scen.Sec.EmissionsFactor)

  port_data <- port_calculation %>%
    right_join(port_data, by = c("Investor.Name", "Portfolio.Name", "Allocation", "Scenario", "Sector", "ScenarioGeography", "Year"), suffix = c("", "_sda"))

  port_data <- port_data %>%
    mutate(Scen.Sec.EmissionsFactor = if_else(!is.na(Scen.Sec.EmissionsFactor_sda), Scen.Sec.EmissionsFactor_sda, Scen.Sec.EmissionsFactor)) %>%
    select(-Scen.Sec.EmissionsFactor_sda)

  return(port_data)

}
