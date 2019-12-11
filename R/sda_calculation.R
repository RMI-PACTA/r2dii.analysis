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
#' @export

library(tidyverse)
library(reprex)
library(datapasta)

sample_portfolio <- tibble::tribble(
                                       ~Investor.Name, ~Portfolio.Name, ~Scenario, ~ScenarioGeography,       ~Allocation, ~Year, ~Sector, ~Plan.Sec.EmissionsFactor, ~Scen.Sec.EmissionsFactor,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2019, "Steel",                        1.8,                  1.1063532,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2020, "Steel",                        1.8,                  1.0608054,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2021, "Steel",                        1.8,                  1.0152576,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2022, "Steel",                        1.8,                  0.9697098,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2023, "Steel",                        1.8,                   0.924162,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2024, "Steel",                        1.8,                  0.8786143,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2025, "Steel",                        1.8,                  0.8330665,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2026, "Steel",                         NA,                  0.7955433,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2027, "Steel",                         NA,                  0.7580202,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2028, "Steel",                         NA,                  0.7204971,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2029, "Steel",                         NA,                   0.682974,
                                          "Investor1",    "Portfolio1",    "B2DS",            "Global", "PortfolioWeight",  2040, "Steel",                         NA,                  0.3663936
                                       )



sample_market <- tibble::tribble(
                   ~Investor.Name, ~Portfolio.Name, ~Scenario, ~ScenarioGeography,       ~Allocation, ~Year, ~Sector, ~Plan.Sec.EmissionsFactor, ~Scen.Sec.EmissionsFactor,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2019, "Steel",                       1.11,                  1.1063532,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2020, "Steel",                       1.11,                  1.0608054,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2021, "Steel",                       1.11,                  1.0152576,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2022, "Steel",                       1.11,                  0.9697098,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2023, "Steel",                       1.11,                   0.924162,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2024, "Steel",                       1.11,                  0.8786143,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2025, "Steel",                       1.11,                  0.8330665,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2026, "Steel",                         NA,                  0.7955433,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2027, "Steel",                         NA,                  0.7580202,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2028, "Steel",                         NA,                  0.7204971,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2029, "Steel",                         NA,                   0.682974,
                         "Market",  "GlobalMarket",    "B2DS",            "Global", "PortfolioWeight",  2040, "Steel",                         NA,                  0.3663936
                   )


sda_calculation <- function(market_data, port_data, ref_sector = c("Cement", "Steel"), ref_scenario = "B2DS", ref_geography = "Global", start_year = 2019, target_year = 2040)  {

  startender <- function(input_data, var = Plan.Sec.EmissionsFactor, year = start_year) {

    var <- enquo(var)

    output_data <- input_data %>%
      filter(
        !is.na(!!var) &
          .data$Year == year &
          .data$Scenario %in% ref_scenario &
          .data$Sector %in% ref_sector &
          .data$ScenarioGeography %in% ref_geography
      ) %>%
      rename(CI = !!var) %>%
      distinct(
        .data$Investor.Name,
        .data$Portfolio.Name,
        .data$CI,
        .data$Scenario,
        .data$ScenarioGeography,
        .data$Sector,
        .data$Allocation
      )

    return(output_data)
  }


  CI_port <- startender(input_data = port_data)
  CI_market <- startender(input_data = market_data)
  SI <- startender(input_data = market_data, year = target_year, var = Scen.Sec.EmissionsFactor) %>%
    rename(SI = .data$CI)

  Distance <- CI_market %>%
    inner_join(SI, by = c("Sector", "Scenario", "Allocation", "Portfolio.Name",  "Investor.Name", "ScenarioGeography")) %>%
    inner_join(CI_port, by = c("Sector", "Scenario", "Allocation", "ScenarioGeography"), suffix = c("_market", "_port")) %>%
    mutate(D_port = .data$CI_port - .data$SI)

  view <- function(input_data = port_data) {

    output_data <- input_data %>%
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

    return(output_data)
  }

  market_view <- view(input_data = market_data)
  port_view <- view(input_data = port_data)


  port_to_market <- market_view %>%
    select(-c(.data$Investor.Name, .data$Portfolio.Name)) %>%
    inner_join(port_view, by = c("Sector", "Year", "Allocation", "ScenarioGeography", "Scenario"), suffix = c("_port", "_market"))

  port_to_distance <- port_to_market %>%
    inner_join(Distance, by = c("Scenario", "Sector", "Investor.Name" = "Investor.Name_port", "Portfolio.Name" = "Portfolio.Name_port", "Allocation", "ScenarioGeography"))

  port_calculation <- port_to_distance %>%
    mutate(
      P_market  = (.data$Scen.Sec.EmissionsFactor_market - SI)/(CI_market - SI),
      Scen.Sec.EmissionsFactor = (.data$D_port * 1 * .data$P_market) + .data$SI
    )

  port_calculation <- port_calculation %>%
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

  port_data <- port_calculation %>%
    right_join(port_data, by = c("Investor.Name", "Portfolio.Name", "Allocation", "Scenario", "Sector", "ScenarioGeography", "Year"), suffix = c("", "_sda"))

  port_data <- port_data %>%
    mutate(
      Scen.Sec.EmissionsFactor =
        if_else(
          !is.na(.data$Scen.Sec.EmissionsFactor_sda),
          .data$Scen.Sec.EmissionsFactor_sda,
          .data$Scen.Sec.EmissionsFactor
        )
      ) %>%
    select(-.data$Scen.Sec.EmissionsFactor_sda)

  return(port_data)

}

sda_calculation(market_data = sample_market,
                port_data = sample_portfolio,
                ref_sector = "Steel",
                ref_scenario = "B2DS",
                ref_geography = "Global",
                start_year = 2019,
                target_year = 2040)


