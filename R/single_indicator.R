library(tidyverse)
library(readxl)


# TO DO
# seperate function for influence map roll-up
# exclude technology roll-up
# finish sample data

#################################################################
# load sample data
#################################################################
file <- "/Users/vincentjerosch-herold/Desktop/untitled folder/single_indicator_sample_inputs.xlsx"

input_results <- read_xlsx(file, sheet = "sample_results")
input_audit <- read_xlsx(file, sheet = "sample_audit")
scenarios <- read_xlsx(file, sheet = "scenario_relationships")
sector_weightings <- read_xlsx(file, sheet = "tech_sector_weighting")

singel_indicator <- function(input_results = input_results, upper_temp_threshold = 10, lower_temp_threshold = 1, start_year = 2019, allocation = "PortfolioWeight") {


  #################################################################
  # define things
  #################################################################

  brown_technologies <- c("Oil", "Gas", "Coal", "CoalCap", "OilCap", "GasCap", "ICE")
  `%not in%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))

  #################################################################
  # prepare and filter input data
  #################################################################


  temp <- input_results %>%
    filter(Allocation == allocation &
      ((ScenarioGeography == "GlobalAggregate" & Sector == "Power") | (ScenarioGeography == "Global" & Sector != "Power")) &
      Year >= start_year & Year <= start_year + 5 &
      Plan.Alloc.WtTechProd > 0)

  temp <- temp %>%
    distinct(Investor.Name, Portfolio.Name, Scenario, Sector, Technology, asset_class, Year, Scen.Alloc.WtTechProd, Plan.Alloc.WtTechProd)

  #################################################################
  # calculating the integral of delta
  #################################################################

  temp <- temp %>%
    group_by(Investor.Name, Portfolio.Name, Scenario, Sector, Technology, asset_class) %>%
    mutate(
      delta_plan_tech_prod = lead(Plan.Alloc.WtTechProd, n = 1L) - Plan.Alloc.WtTechProd, # first step is to calculate the integral of the delta over the 5 year time horizon
      delta_scen_tech_prod = lead(Scen.Alloc.WtTechProd, n = 1L) - Scen.Alloc.WtTechProd # for both the portfolio and the scenario aligned production
    )

  temp <- temp %>%
    group_by(Investor.Name, Portfolio.Name, Scenario, Sector, Technology, asset_class) %>%
    summarise(
      integral_delta_plan_tech_prod = sum(delta_plan_tech_prod, na.rm = T),
      integral_delta_scen_tech_prod = sum(delta_scen_tech_prod, na.rm = T)
    )

  temp <- temp %>%
    rename(
      scen_tech_prod = integral_delta_scen_tech_prod,
      plan_tech_prod = integral_delta_plan_tech_prod
    )

  #################################################################
  # adding scenario data
  #################################################################

  scenario_relationships <- temp %>%
    inner_join(scenarios, by = c("Sector", "Scenario")) %>%
    group_by(Investor.Name, Portfolio.Name, Sector) %>%
    filter(n_distinct(Scenario) == 3) %>% # should alway have three scenarios.
    distinct(relation, temp, asset_class, scen_tech_prod, Sector, Technology)


  temp <- scenario_relationships %>%
    pivot_wider(names_from = relation, values_from = c(scen_tech_prod, temp)) %>% # spreading out the different relations.
    inner_join(temp, by = c("Sector", "Technology", "asset_class", "Investor.Name", "Portfolio.Name")) %>%
    distinct(Investor.Name, Portfolio.Name, Sector, Technology, asset_class, temp_lower, temp_reference, temp_upper, scen_tech_prod_lower, scen_tech_prod_reference, scen_tech_prod_upper, plan_tech_prod)

  #################################################################
  # calculating the range between temperatures.
  #################################################################

  temp <- temp %>%
    group_by(Sector) %>%
    mutate(
      temp_lower_range = temp_reference - temp_lower,
      temp_upper_range = temp_upper - temp_reference
    ) %>%
    ungroup()

  #################################################################
  # reversing the scenarios in case reference is more ambitious
  #################################################################

  temp <- temp %>%
    mutate(
      scen_tech_prod_reference_temp = scen_tech_prod_reference,
      scen_tech_prod_lower_temp = scen_tech_prod_lower
    )

  temp <- temp %>%
    mutate(
      scen_tech_prod_reference = ifelse(
        scen_tech_prod_reference_temp > scen_tech_prod_lower_temp & Technology %not in% brown_technologies,
        scen_tech_prod_reference_temp,
        scen_tech_prod_reference
      )
    )

  temp <- temp %>%
    mutate(
      scen_tech_prod_lower = ifelse(
        scen_tech_prod_reference_temp > scen_tech_prod_lower_temp & Technology %not in% brown_technologies,
        scen_tech_prod_reference_temp,
        scen_tech_prod_lower
      )
    )

  temp <- temp %>%
    select(-c(scen_tech_prod_reference_temp, scen_tech_prod_lower_temp))

  #################################################################
  # finding the production range between scenarios for each
  # technology
  #################################################################

  temp <- temp %>%
    mutate(scen_tech_prod_upper_bound = scen_tech_prod_reference - scen_tech_prod_upper)

  temp <- temp %>%
    mutate(scen_tech_prod_lower_bound = scen_tech_prod_reference - scen_tech_prod_lower)

  #################################################################
  # finding the production difference of the portfolio relative to
  # the reference (median) scenario
  #################################################################

  temp <- temp %>%
    mutate(scen_plan_prod_diff = scen_tech_prod_reference - plan_tech_prod)

  #################################################################
  # finding the factor of the difference and the scenario production
  # range.
  #################################################################

  scenario_port_relation <- function(input = temp, metric_name = "factor", calculation_upper = scen_plan_prod_diff / scen_tech_prod_upper_bound, calculation_lower = scen_plan_prod_diff / scen_tech_prod_lower_bound) {



    # brown technologies
    #################################

    input <- input %>%
      mutate(
        metric = case_when(
          # brown technologies
          plan_tech_prod > scen_tech_prod_reference & Technology %in% brown_technologies ~ {{ calculation_upper }},
          plan_tech_prod < scen_tech_prod_reference & Technology %in% brown_technologies ~ {{ calculation_lower }},
          # green technologies
          plan_tech_prod < scen_tech_prod_reference & Technology %not in% brown_technologies ~ {{ calculation_upper }},
          plan_tech_prod > scen_tech_prod_reference & Technology %not in% brown_technologies ~ {{ calculation_lower }}
        )
      )


    input <- input %>%
      rename({{ metric_name }} := metric)
  }

  temp <- scenario_port_relation(
    input = temp,
    metric_name = "factor",
    calculation_upper = scen_plan_prod_diff / scen_tech_prod_upper_bound,
    calculation_lower = scen_plan_prod_diff / scen_tech_prod_lower_bound
  )


  #################################################################
  # calculating temperature
  #################################################################

  temp <- scenario_port_relation(
    input = temp,
    metric_name = "temperature",
    calculation_upper = temp_reference + (temp_upper_range * factor),
    calculation_lower = temp_reference - (temp_lower_range * factor)
  )

  #################################################################
  # the last step is to filter temperature above a certain threshold
  #################################################################
  temp <- temp %>%
    mutate(
      temperature = case_when(
        temperature > upper_temp_threshold ~ upper_temp_threshold,
        temperature < lower_temp_threshold ~ lower_temp_threshold,
        TRUE ~ temperature
      )
    )
}


temp <- singel_indicator(
  input_results = input_results,
  upper_temp_threshold = 10,
  lower_temp_threshold = 1,
  start_year = 2019,
  allocation = "PortfolioWeight"
)


influencemap_weighting_methodology <- function(input_results = temp, input_audit = input_audit, metric = temperature) {


  #################################################################
  # preparing audit file to calculate $ sector exposure
  #################################################################

  input_results <- input_results %>%
    inner_join(sector_weightings, by = c("Sector", "Technology"))

  sector_exposure <- input_audit %>%
    rename(Sector = mapped_sector) %>%
    group_by(Investor.Name, Portfolio.Name, Sector) %>%
    summarise(value_usd_sector = sum(ValueUSD, na.rm = T))

  input_results <- sector_exposure %>%
    filter(Sector != "Other" & !is.na(Sector)) %>%
    inner_join(input_results, by = c("Sector", "Investor.Name", "Portfolio.Name"))

  #################################################################
  # rolling everything up to the portfolio level using the
  # InfluenceMap Methdology
  #################################################################

  input_results_technology <- input_results %>%
    filter(!is.na(technology_weight)) %>%
    group_by(Investor.Name, Portfolio.Name, Sector) %>%
    mutate(
      metric_sector = weighted.mean({{ metric }}, technology_weight, na.rm = T)
    )

  input_results_sector <- input_results %>%
    filter(is.na(technology_weight)) %>%
    mutate(
      metric_sector = {{ metric }}
    )

  input_results_sector <- bind_rows(input_results_technology, input_results_sector)

  input_results_port <- input_results_sector %>%
    group_by(Portfolio.Name, Investor.Name) %>%
    mutate(
      sector_value_weight = value_usd_sector * sector_weight,
      metric_port = weighted.mean(metric_sector, sector_value_weight, na.rm = T)
    )

  output_results_port <- input_results_port %>%
    select(-c({{ metric }})) %>%
    rename({{ metric }} := metric_port)
}

temp_port <- influencemap_weighting_methodology(
  input_results = temp,
  input_audit = input_audit,
  metric = temperature
)


mapped_sector_exposure <- function(input_audit = input_audit) {

  #################################################################
  # coverage assessment for the single indicator metric
  #################################################################

  coverage <- input_audit %>%
    mutate(
      climate_rel_cat = ifelse(mapped_sector != "Other", T, F)
    )

  coverage <- coverage %>%
    group_by(Investor.Name, Portfolio.Name) %>%
    mutate(
      ValueUSD_port = sum(ValueUSD, na.rm = T)
    )

  coverage <- coverage %>%
    group_by(Investor.Name, Portfolio.Name, climate_rel_cat) %>%
    mutate(
      exposure_climate_sectors = sum(ValueUSD, na.rm = T) / ValueUSD_port
    ) %>%
    ungroup()


  coverage <- coverage %>%
    filter(climate_rel_cat == T) %>%
    distinct(Investor.Name, Portfolio.Name, exposure_climate_sectors)
}

coverage <- mapped_sector_exposure(input_audit = input_audit)


#################################################################
# connecting all of the dots
#################################################################

temp_metric <- temp_port %>%
  distinct(Investor.Name, Portfolio.Name, temperature) %>%
  inner_join(coverage, by = c("Investor.Name", "Portfolio.Name"))
