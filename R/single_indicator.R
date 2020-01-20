# FIXME: How to use package in a package?  :)
# * Don't use tidyverse in packages but the specific packages you need
# * Don't use library(pkg), instead use usethis::use_package("pkg")
library(tidyverse)
# FIXME: Avoid i/o. Write functions that expect dataframes.
library(readxl)


# TO DO
# seperate function for influence map roll-up
# exclude technology roll-up
# finish sample data

#################################################################
# load sample data
#################################################################
# TO DO:
# integrate the files to the repo to avoid loading it from a dropbox folder &
# then remove the following lines: I, Klaus moved the file in the repo as an
# intermediate step to ensure version control!
# FIXME: Move to data-raw/. From there move to data/ with usethis::use_data()
file <- "./data/single_indicator_sample_inputs.xlsx"

input_results <- read_xlsx(file, sheet = "sample_results")
input_audit <- read_xlsx(file, sheet = "sample_audit")
scenario_relationships <- read_xlsx(file, sheet = "scenario_relationships")
sector_weightings <- read_xlsx(file, sheet = "tech_sector_weighting")


# FIXME: `input_results` is a data argument and should have no default
# Defaults are appropriate for detail arguments only.
# Also, where would the object `results` come from?
single_indicator <- function(input_results = results,
                             upper_temp_threshold = 6,
                             lower_temp_threshold = 1.5,
                             # FIXME: For how long will this be a good default?
                             start_year = 2019,
                             time_horizon = 5,
                             allocation = "PortfolioWeight",
                             # FIXME: Work with clean column names
                             group_vars = c(
                               "Investor.Name",
                               "Portfolio.Name",
                               "Id"
                             )) {
  # TODO: Check inputs here
  # TODO: Clean column names
  # TODO: Clean grouping variables

  brown_technologies <- c(
    "Oil", "Gas", "Coal", "CoalCap", "OilCap", "GasCap", "ICE"
  )

  #################################################################
  # a bit messy, but basically how we add "Ids" in
  #################################################################
  # TODO: Extract into a function with a name that shows intent,e.g. add_id()
  # FIXME: Refactor. Extract each condition as an object named to show intent
  # FIXME: Can you use identical() instead `==`? (identical() gives single T/F)
  # FIXME: Is `1` okay or you need `1L`?
  if (length(intersect("Id", group_vars)) == 1 &
    length(
      intersect(
        c("bloomberg_id", "CorpBondTicker", "Asset.Type"),
        # FIXME: `temp` is undefined. Where should it come from?
        # devtools::load_all(".") throws "Error ... object 'temp' not found"
        names(temp)
      )
    ) == 3
  ) {
    temp <- temp %>%
      mutate(
        Id = case_when(
          # FIXME: Clean data at the top, then refer to clean column names
          # e.g. not Asset.Type but asset_type
          # This applies to all code. Won't mention this again for simplicity.
          # FIXME: Use the .data to refer to all columns in the data mask
          # e.g. `.data$asset_type`
          !is.na(bloomberg_id) & Asset.Type == "Equity" ~ bloomberg_id,
          !is.na(CorpBondTicker) & Asset.Type == "Bonds" ~ CorpBondTicker
        )
      ) %>%
      filter(!is.na(Id)) %>%
      select(-c(bloomberg_id, CorpBondTicker))
  } else {
    group_vars <- setdiff(group_vars, "Id")
  }

  #################################################################
  # prepare and filter input data
  #################################################################
  # FIXME: Rename `temp` each time with a name that more clearly reflect
  # the contents of the object or your intent. It's really difficult to debug
  # code that keeps overwriting the same object because you can't go back
  # and inspect previous state without re-running large pieces of code
  temp <- input_results %>%
    ungroup() %>%
    # TODO: Extract each condition as an object with a name showing intent
    # e.g. `if (is_raining && is_cold) { wear_thick_raincoat() }`
    # `is_raining`, `is_cold`, and `wear_thick_raincoat` may be 100 linees each
    # but their name clearly express what those lines should do
    filter(Allocation %in% allocation &
      ((ScenarioGeography == "GlobalAggregate" & Sector == "Power") | (ScenarioGeography == "Global" & Sector != "Power")) &
      Year >= start_year & Year <= start_year + time_horizon &
      Plan.Alloc.WtTechProd > 0)

  temp <- check_group_vars(
    temp,
    group_vars
  )

  temp <- temp %>%
    distinct(!!!syms(group_vars), Allocation, Scenario, Sector, Technology, Asset.Type, Year, Scen.Alloc.WtTechProd, Plan.Alloc.WtTechProd)

  #################################################################
  # calculating the integral of delta
  #################################################################
  # TODO: Any section you can write a heading for should probabbly be extracted
  # in its own, meaningfully named, function. e.g. calculate_integral_of_delta()
  temp <- temp %>%
    group_by(!!!syms(group_vars), Allocation, Scenario, Sector, Technology, Asset.Type) %>%
    mutate(
      delta_plan_tech_prod = lead(Plan.Alloc.WtTechProd, n = 1L) - Plan.Alloc.WtTechProd, # first step is to calculate the integral of the delta over the 5 year time horizon
      delta_scen_tech_prod = lead(Scen.Alloc.WtTechProd, n = 1L) - Scen.Alloc.WtTechProd # for both the portfolio and the scenario aligned production
    )

  temp <- temp %>%
    group_by(!!!syms(group_vars), Allocation, Scenario, Sector, Technology, Asset.Type) %>%
    summarise(
      # FIXME: Use the full form TRUE and FALSE (style guideline)
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
    inner_join(scenario_relationships, by = c("Sector", "Scenario")) %>%
    group_by(!!!syms(group_vars), Sector) %>%
    # TODO: Capture expectations in assertions (e.g. stopifnot()) or tests
    filter(n_distinct(Scenario) == 3) %>% # should alway have three scenarios.
    distinct(relation, temp, Asset.Type, scen_tech_prod, Sector, Technology)


  temp <- scenario_relationships %>%
    # spreading out the different relations.
    pivot_wider(names_from = relation, values_from = c(scen_tech_prod, temp)) %>%
    inner_join(temp, by = c("Sector", "Technology", "Asset.Type", group_vars)) %>%
    distinct(
      !!!syms(group_vars),
      Sector,
      Allocation,
      Technology,
      Asset.Type,
      temp_lower,
      temp_reference,
      temp_upper,
      scen_tech_prod_lower,
      scen_tech_prod_reference,
      scen_tech_prod_upper,
      plan_tech_prod
    )

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
        scen_tech_prod_reference_temp > scen_tech_prod_lower_temp & !Technology %in% brown_technologies,
        scen_tech_prod_reference_temp,
        scen_tech_prod_reference
      )
    )

  temp <- temp %>%
    mutate(
      scen_tech_prod_lower = ifelse(
        scen_tech_prod_reference_temp > scen_tech_prod_lower_temp & !Technology %in% brown_technologies,
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
  # FIXME: Extract scenario_port_relation out (to the top level)
  # FIXME: `input` is a data argument and should not have a default.
  scenario_port_relation <- function(input = temp,
                                     metric_name = "factor",
                                     # FIXME: This variables won't be in scope
                                     # once you extract this function out to
                                     # the top level. Thus, this should not be
                                     # defaults
                                     calculation_upper = scen_plan_prod_diff / scen_tech_prod_upper_bound,
                                     calculation_lower = scen_plan_prod_diff / scen_tech_prod_lower_bound) {



    # brown technologies
    #################################

    input <- input %>%
      mutate(
        metric = case_when(
          # brown technologies
          plan_tech_prod > scen_tech_prod_reference & Technology %in% brown_technologies ~ {{ calculation_upper }},
          plan_tech_prod < scen_tech_prod_reference & Technology %in% brown_technologies ~ {{ calculation_lower }},
          # green technologies
          plan_tech_prod < scen_tech_prod_reference & !Technology %in% brown_technologies ~ {{ calculation_upper }},
          plan_tech_prod > scen_tech_prod_reference & !Technology %in% brown_technologies ~ {{ calculation_lower }}
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
  # next filter temperature above a certain threshold
  #################################################################
  temp <- temp %>%
    mutate(
      temperature = case_when(
        temperature > upper_temp_threshold ~ upper_temp_threshold,
        temperature < lower_temp_threshold ~ lower_temp_threshold,
        TRUE ~ temperature
      )
    )

  #################################################################
  # add back generic scenario and scenario geography column
  #################################################################
  temp <- temp %>%
    mutate(
      ScenarioGeography = "Global",
      Scenario = "Aggregate"
    )
}


temp <- single_indicator(
  input_results = input_results,
  upper_temp_threshold = 10,
  lower_temp_threshold = 1.5,
  start_year = 2019,
  time_horizon = 5,
  allocation = "PortfolioWeight",
  group_vars = c("Investor.Name", "Portfolio.Name", "Id")
)

# FIXME: input_results is a data argument and should not have a default. `temp`
# is out of scope
influencemap_weighting_methodology <- function(input_results = temp,
                                               input_audit = input_audit,
                                               metric = temperature) {


  #################################################################
  # preparing audit file to calculate $ sector exposure
  #################################################################

  input_results <- input_results %>%
    inner_join(sector_weightings, by = c("Sector", "Technology"))

  sector_exposure <- input_audit %>%
    rename(Sector = mapped_sector) %>%
    group_by(Investor.Name, Portfolio.Name, Sector, Asset.Type) %>%
    summarise(value_usd_sector = sum(ValueUSD, na.rm = T))

  sector_exposure <- sector_exposure %>%
    group_by(Investor.Name, Portfolio.Name, Asset.Type) %>%
    mutate(value_usd_Asset.Type = sum(value_usd_sector, na.rm = T))

  input_results <- sector_exposure %>%
    filter(Sector != "Other" & !is.na(Sector)) %>%
    inner_join(input_results, by = c("Sector", "Investor.Name", "Portfolio.Name", "Asset.Type"))

  #################################################################
  # rolling everything up to the portfolio level using the
  # InfluenceMap Methdology
  #################################################################

  input_results_technology <- input_results %>%
    filter(!is.na(technology_weight)) %>%
    group_by(Investor.Name, Portfolio.Name, Asset.Type, Sector, Allocation, ScenarioGeography, Scenario) %>%
    mutate(
      metric_sector = weighted.mean({{ metric }}, technology_weight, na.rm = T)
    )

  input_results_sector <- input_results %>%
    filter(is.na(technology_weight)) %>%
    mutate(
      metric_sector = {{ metric }}
    )

  input_results_sector <- bind_rows(input_results_technology, input_results_sector)

  input_results_Asset.Type <- input_results_sector %>%
    group_by(Portfolio.Name, Investor.Name, Asset.Type, Allocation, ScenarioGeography, Scenario) %>%
    mutate(
      sector_value_weight = value_usd_sector * sector_weight,
      metric_Asset.Type = weighted.mean(metric_sector, sector_value_weight, na.rm = T)
    )


  input_results_port <- input_results_Asset.Type %>%
    group_by(Portfolio.Name, Investor.Name, Allocation, ScenarioGeography, Scenario) %>%
    mutate(
      financial_instument_value_weight = value_usd_Asset.Type,
      metric_port = weighted.mean(metric_Asset.Type, financial_instument_value_weight, na.rm = T)
    )
  output_results_port <- input_results_port %>%
    select(-c({{ metric }})) %>%
    rename({{ metric }} := metric_port)
  # FIXME: Reserve return() for early returns (style guideline)
  return(output_results_port)
}

# FIXME: Remove `temp``
temp_port <- influencemap_weighting_methodology(
  input_results = temp,
  input_audit = input_audit,
  metric = temperature
)

# FIXME: Remove default `input_audit`; it's out of scope
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

  return(coverage)
}

coverage <- mapped_sector_exposure(
  input_audit = input_audit
)


#################################################################
# connecting all of the dots
#################################################################

temp_metric <- temp_port %>%
  distinct(Investor.Name, Portfolio.Name, Allocation, temperature) %>%
  inner_join(coverage, by = c("Investor.Name", "Portfolio.Name"))

# FIXME: If possible, rename to use the imperative-verb tidyverse style. e.g.
# find_range()
range_finder <- function(input_temp = temp_metric, range = c(1.75, 2, 2.75, 3.5)) {


  #################################################################
  # find the lower value in the interval range
  #################################################################
  input_temp <- input_temp %>%
    mutate(
      interval = as.numeric(cut(temperature, breaks = range))
    )


  output <- input_temp %>%
    mutate(
      temperature_range =
        ifelse(!is.na(interval),
          paste0(range[[interval]] + 0.01, "-", range[[interval + 1]]),
          NA
        ),
      temperature_range =
        ifelse(is.na(interval) & temperature > max(range),
          paste0(">", max(range)),
          temperature_range
        ),
      temperature_range =
        ifelse(is.na(interval) & temperature < min(range),
          paste0("<", min(range)),
          temperature_range
        )
    ) %>%
    select(-c(interval))

  return(output)
}

# FIXME: Remove. Should probably be defined inside a function
temp_metric <- range_finder(
  input_temp = temp_metric,
  range = c(1.75, 2, 2.75, 3.5)
)
