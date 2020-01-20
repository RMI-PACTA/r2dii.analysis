# TO DO
# seperate function for influence map roll-up
# exclude technology roll-up
# finish sample data

#################################################################
# load sample data
#################################################################

#' Find the factor of the difference and the scenario production range
#'
#' TO DO:
#' integrate the files to the repo to avoid loading it from a dropbox folder &
#' then remove the following lines: I, Klaus moved the file in the repo as an
#' intermediate step to ensure version control!
#'
#' @examples
#' temp <- single_indicator(
#'   input_results = input_results,
#'   upper_temp_threshold = 10,
#'   lower_temp_threshold = 1.5,
#'   start_year = 2019,
#'   time_horizon = 5,
#'   allocation = "PortfolioWeight",
#'   group_vars = c("Investor.Name", "Portfolio.Name", "Asset.Type")
#' )
#'
#' temp_port <- influencemap_weighting_methodology(
#'   input_results = temp,
#'   input_audit = input_audit,
#'   metric_name = "temperature",
#'   group_vars = c("Investor.Name", "Portfolio.Name")
#' )
#'
#' coverage <- mapped_sector_exposure(
#'   input_audit = input_audit
#' )
#'
#' connecting all of the dots
#' temp_metric <- temp_port %>%
#'   distinct(Investor.Name, Portfolio.Name, Allocation, temperature) %>%
#'   inner_join(coverage, by = c("Investor.Name", "Portfolio.Name"))
#'
#' # FIXME: Remove. Should probably be defined inside a function
#' temp_metric <- find_range(
#'   input_temp = temp_metric,
#'   range = c(1.75, 2, 2.75, 3.5)
#' )
#' @noRd
find_scenario_relation <- function(input,
                                   metric_name,
                                   calculation_upper,
                                   calculation_lower) {

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

#calculate relative or absolute production
calculate_production <- function(temp,
                                 method = "absolute") {

  if (method == "relative" & method != "absolute") {
    # calculating the integral of delta
    temp <- temp %>%
      group_by(!!! syms(group_vars), Allocation, Scenario, Sector, Technology) %>%
      mutate(
        Plan.Alloc.WtTechProd = dplyr::lead(Plan.Alloc.WtTechProd, n = 1L) - Plan.Alloc.WtTechProd, # first step is to calculate the integral of the delta over the 5 year time horizon
        Scen.Alloc.WtTechProd = dplyr::lead(Scen.Alloc.WtTechProd, n = 1L) - Scen.Alloc.WtTechProd # for both the portfolio and the scenario aligned production
      )
  }

  temp <- temp %>%
    group_by(!!! syms(group_vars), Allocation, Scenario, Sector, Technology) %>%
    summarise(
      Plan.Alloc.WtTechProd = sum(Plan.Alloc.WtTechProd, na.rm = TRUE),
      Scen.Alloc.WtTechProd = sum(Scen.Alloc.WtTechProd, na.rm = TRUE)
    )

  temp <- temp %>%
    rename(
      scen_tech_prod = Scen.Alloc.WtTechProd,
      plan_tech_prod = Plan.Alloc.WtTechProd
    )

}

#' @export
single_indicator <- function(input_results,
                             upper_temp_threshold = 6,
                             lower_temp_threshold = 1.5,
                             # FIXME: For how long will this be a good default?
                             start_year = 2019,
                             time_horizon = 5,
                             allocation = "PortfolioWeight",
                             production_type = "absolute",
                             # FIXME: Work with clean column names
                             group_vars = c(
                               "Investor.Name",
                               "Portfolio.Name",
                               "Asset.Type"
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
  # TODO @mauro I think this over complicates. It is probably just easiest to add
  # bloomberg_id and corp_bond_ticker as a grouping variable, which achieves the same result.
  # if (length(intersect("Id", group_vars)) == 1 &
  #   length(
  #     intersect(
  #       c("bloomberg_id", "CorpBondTicker", "Asset.Type"),
  #       # FIXME: `temp` is undefined. Where should it come from?
  #       names(temp)
  #     )
  #   ) == 3
  # ) {
  #   temp <- temp %>%
  #     mutate(
  #       Id = case_when(
  #         # FIXME: Clean data at the top, then refer to clean column names
  #         # e.g. not Asset.Type but asset_type
  #         # This applies to all code. Won't mention this again for simplicity.
  #         # FIXME: Use the .data to refer to all columns in the data mask
  #         # e.g. `.data$asset_type`
  #         !is.na(bloomberg_id) & Asset.Type == "Equity" ~ bloomberg_id,
  #         !is.na(CorpBondTicker) & Asset.Type == "Bonds" ~ CorpBondTicker
  #       )
  #     ) %>%
  #     filter(!is.na(Id)) %>%
  #     select(-c(bloomberg_id, CorpBondTicker))
  # } else {
  #   group_vars <- setdiff(group_vars, "Id")
  # }

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


  # temp <- check_group_vars(
  #   temp,
  #   group_vars
  # )

  temp <- temp %>%
    distinct(!!! syms(group_vars), Allocation, Scenario, Sector, Technology, Year, Scen.Alloc.WtTechProd, Plan.Alloc.WtTechProd)

  temp <- calculate_production(
    temp = temp,
    method = production_type #use absolute or relative
  )

  # adding scenario data
  scenario_relationships <- temp %>%
    inner_join(scenario_relationships, by = c("Sector", "Scenario")) %>%
    ungroup()

  scenario_relationships <- scenario_relationships %>%
    distinct(!!! syms(group_vars), relation, temp, scen_tech_prod, Sector, Technology) %>%
    filter(scen_tech_prod > 0)
  # TODO: Capture expectations in assertions (e.g. stopifnot()) or tests




  temp <- scenario_relationships %>%
    # spreading out the different relations.
    tidyr::pivot_wider(names_from = relation, values_from = c(scen_tech_prod, temp)) %>%
    inner_join(temp, by = c("Sector", "Technology", group_vars)) %>%
    distinct(
      !!!syms(group_vars),
      Sector,
      Allocation,
      Technology,
      temp_lower,
      temp_reference,
      temp_upper,
      scen_tech_prod_lower,
      scen_tech_prod_reference,
      scen_tech_prod_upper,
      plan_tech_prod
    )

  # calculating the range between temperatures.
  temp <- temp %>%
    group_by(Sector) %>%
    mutate(
      temp_lower_range = temp_reference - temp_lower,
      temp_upper_range = temp_upper - temp_reference
    ) %>%
    ungroup()

  # finding the production range between scenarios for each technology
  temp <- temp %>%
    mutate(scen_tech_prod_upper_bound = scen_tech_prod_reference - scen_tech_prod_upper)

  temp <- temp %>%
    mutate(scen_tech_prod_lower_bound = scen_tech_prod_reference - scen_tech_prod_lower)

  # finding the production difference of the portfolio relative to the reference (median) scenario
  temp <- temp %>%
    mutate(scen_plan_prod_diff = scen_tech_prod_reference - plan_tech_prod)

  # finding the factor of the difference and the scenario production range.
  temp <- find_scenario_relation(
    input = temp,
    metric_name = "factor",
    calculation_upper = scen_plan_prod_diff / scen_tech_prod_upper_bound,
    calculation_lower = scen_plan_prod_diff / scen_tech_prod_lower_bound
  )

  # calculating temperature
  temp <- find_scenario_relation(
    input = temp,
    metric_name = "temperature",
    calculation_upper = temp_reference + (temp_upper_range * factor),
    calculation_lower = temp_reference - (temp_lower_range * factor)
  )

  # next filter temperature above a certain threshold
  temp <- temp %>%
    mutate(
      temperature = case_when(
        temperature > upper_temp_threshold ~ upper_temp_threshold,
        temperature < lower_temp_threshold ~ lower_temp_threshold,
        TRUE ~ temperature
      )
    )

  # add back generic scenario and scenario geography column
  temp <- temp %>%
    mutate(
      ScenarioGeography = "Global",
      Scenario = "Aggregate"
    )
}

# FIXME: input_results is a data argument and should not have a default. `temp`
# is out of scope
#' @export
influencemap_weighting_methodology <- function(input_results,
                                               input_audit,
                                               metric_name = "temperature",
                                               group_vars = c("Investor.Name", "Portfolio.Name")) {


  # preparing audit file to calculate $ sector exposure
  input_results <- input_results %>%
    inner_join(sector_weightings, by = c("Sector", "Technology"))

  sector_exposure <- input_audit %>%
    rename(Sector = mapped_sector) %>%
    group_by(Investor.Name, Portfolio.Name, Sector, Asset.Type) %>%
    summarise(value_usd_sector = sum(ValueUSD, na.rm = TRUE))

  sector_exposure <- sector_exposure %>%
    group_by(Investor.Name, Portfolio.Name, Asset.Type) %>%
    mutate(value_usd_Asset.Type = sum(value_usd_sector, na.rm = TRUE))

  input_results <- sector_exposure %>%
    filter(Sector != "Other" & !is.na(Sector)) %>%
    inner_join(input_results, by = c("Sector", "Investor.Name", "Portfolio.Name", "Asset.Type"))

  # rolling everything up to the portfolio level using the InfluenceMap Methdology
  input_results_technology <- input_results %>%
    filter(!is.na(technology_weight)) %>%
    group_by(!!! syms(group_vars), Asset.Type, Sector, Allocation, ScenarioGeography, Scenario) %>%
    mutate(metric_sector = stats::weighted.mean(.data[[metric_name]], technology_weight, na.rm = TRUE))

  input_results_sector <- input_results %>%
    filter(is.na(technology_weight)) %>%
    mutate(metric_sector = .data[[metric_name]])

  input_results_sector <- bind_rows(input_results_technology, input_results_sector)

  input_results_Asset.Type <- input_results_sector %>%
    group_by(!!! syms(group_vars), Asset.Type, Allocation, ScenarioGeography, Scenario) %>%
    mutate(
      sector_value_weight = value_usd_sector * sector_weight,
      metric_Asset.Type = stats::weighted.mean(metric_sector, sector_value_weight, na.rm = TRUE)
    )


  input_results_port <- input_results_Asset.Type %>%
    group_by(!!! syms(group_vars), Allocation, ScenarioGeography, Scenario) %>%
    mutate(
      financial_instument_value_weight = value_usd_Asset.Type,
      metric_port = stats::weighted.mean(metric_Asset.Type, financial_instument_value_weight, na.rm = TRUE)
    )

  input_results_port <- input_results_port %>%
    select(-c(.data[[metric_name]])) %>%
    rename({{ metric_name }} := metric_port)
}

# FIXME: Remove default `input_audit`; it's out of scope
#' @export
mapped_sector_exposure <- function(input_audit) {

  # coverage assessment for the single indicator metric
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

# FIXME: If possible, rename to use the imperative-verb tidyverse style. e.g.
# find_range()
#' @export
find_range <- function(input_temp,
                         range) {

  # find the lower value in the interval range
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

