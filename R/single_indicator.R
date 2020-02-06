# Notes that maybe outdated:
# TODO
# * seperate function for influence map roll-up
# * exclude technology roll-up
# * finish sample data

#' TODO vintented
#'
#' @param input_results A dataframe like [r2dii.analysis::sample_results].
#' @param input_audit A dataframe like [r2dii.analysis::sample_audit].
#' @param metric_name The name of the ouput metric (i.e. deviation or temperature).
#' @param group_vars A set of grouping variables that define the granularity of the metric.
#'
#' @return Returns a dataframe with the the sector, group_vars, and portfolio reweighted metrics.
#' @export
apply_influencemap_portfolio_weighting <- function(input_results,
                                                   input_audit,
                                                   metric_name = "temperature",
                                                   group_vars = c(
                                                     "Investor.Name",
                                                     "Portfolio.Name",
                                                     "Asset.Type")
                                                   ) {

  r2dii.analysis::tech_sector_weighting

  results_sector_weightings <- input_results %>%
    inner_join(
      tech_sector_weighting,
      by = c("Sector", "Technology")
      )

  # preparing audit file to calculate $ sector exposure
  sector_exposure <- input_audit %>%
    rename(Sector = mapped_sector) %>%
    group_by(
      !!! syms(group_vars),
      Sector
    ) %>%
    summarise(value_usd_sector = sum(ValueUSD, na.rm = TRUE))

  sector_exposure <- sector_exposure %>%
    group_by(!!! syms(group_vars)) %>%
    mutate(group_vars_weight = sum(value_usd_sector, na.rm = TRUE))

  results_audit <- sector_exposure %>%
    filter(Sector != "Other" & !is.na(Sector)) %>%
    inner_join(
      results_sector_weightings,
      by = c("Sector", group_vars)
      )

  #for sectors with meaningful technology breakdowns
  results_audit <- results_audit %>%
    mutate(technology_production_weight = plan_tech_prod * technology_weight)

  results_technology <- results_audit %>%
    filter(!is.na(technology_weight)) %>%
    group_by(
      !!! syms(group_vars),
      Sector,
      Allocation,
      ScenarioGeography,
      Scenario
      ) %>%
    mutate(metric_sector = stats::weighted.mean(.data[[metric_name]], technology_production_weight, na.rm = TRUE))

  #for sectors without meaningful technology breakdowns
  results_sector <- results_audit %>%
    filter(is.na(technology_weight)) %>%
    group_by(
      !!! syms(group_vars),
      Sector,
      Allocation,
      ScenarioGeography,
      Scenario
      ) %>%
    mutate(metric_sector = stats::weighted.mean(.data[[metric_name]], plan_tech_prod, na.rm = TRUE))

  # join different approaches together
  results_sector <- bind_rows(results_technology, results_sector)

  # weight by sectors to grouping vars level
  results_sector <- results_sector %>%
    mutate(sector_value_weight = value_usd_sector * sector_weight)

  results_group_vars <- results_sector %>%
    group_by(
      !!! syms(group_vars),
      Allocation,
      ScenarioGeography,
      Scenario
      ) %>%
    mutate(metric_group_vars = stats::weighted.mean(metric_sector, sector_value_weight, na.rm = TRUE))

  # finally calculate weighting mean at portfolio level
  results_port <- results_group_vars %>%
    group_by(
      Investor.Name,
      Portfolio.Name,
      Allocation,
      ScenarioGeography,
      Scenario
      ) %>%
    mutate(metric_port = stats::weighted.mean(metric_group_vars, group_vars_weight, na.rm = TRUE))

  results_port %>%
    select(-c(.data[[metric_name]])) %>%
    rename_at(vars(metric_port, metric_group_vars, metric_sector), funs(paste0("temperature_", .)))

}

#' TODO vintented
#'
#' @param input_audit A dataframe like [r2dii.analysis::sample_audit].
#'
#' @return Returns a dataframe with the value_usd invested in each sector within a portfolio.
#' @export
map_sector_exposure <- function(input_audit) {

  # coverage assessment for the single indicator metric
  coverage <- input_audit %>%
    mutate(climate_rel_cat = ifelse(mapped_sector != "Other", T, F))

  coverage <- coverage %>%
    group_by(
      Investor.Name,
      Portfolio.Name
      ) %>%
    mutate(ValueUSD_port = sum(ValueUSD, na.rm = T))

  coverage <- coverage %>%
    group_by(
      Investor.Name,
      Portfolio.Name,
      climate_rel_cat
      ) %>%
    mutate(exposure_climate_sectors = sum(ValueUSD, na.rm = T) / ValueUSD_port) %>%
    ungroup()

  coverage %>%
    filter(climate_rel_cat == T) %>%
    distinct(
      Investor.Name,
      Portfolio.Name,
      exposure_climate_sectors
      )
}

#' TODO vintented
#'
#' @param input_temp A dataframe with a column with a column that provides
#' a numeric temperature indicator at varying granularities.
#' @param metric_name A character name that specifies a dataframe column with numeric values.
#' @param range A list of numeric values that define the range that a
#' temperature indicators lies within.
#'
#' @return
#' @export
find_range <- function(input_results,
                       metric_name,
                       range) {

  # find the lower value in the interval range
  results_range <- input_results %>%
    mutate(interval = as.numeric(cut(.data[[metric_name]], breaks = range)))

  results_range %>%
    mutate(
      temperature_range =
        ifelse(!is.na(interval),
               paste0(range[interval] + 0.01, "-", range[(interval + 1)]),
               NA
        ),
      temperature_range =
        ifelse(is.na(interval) & .data[[metric_name]] > max(range),
               paste0(">", max(range)),
               temperature_range
        ),
      temperature_range =
        ifelse(is.na(interval) & .data[[metric_name]] < min(range),
               paste0("<", min(range)),
               temperature_range
        )
    ) %>%
    select(-c(interval))
}

#' TODO Add title
#'
#' @param input_results A dataframe like [r2dii.analysis::sample_results].
#' @param upper_temp_threshold A numeric value that defines the upper temperature
#' threshold to cut outlier values.
#' @param lower_temp_threshold A numeric value that defines the lower temperature
#' threshold to cut outlier values.
#' @param start_year A numeric value that defines the start year of the analysis.
#' @param time_horizon A numeric values that defines the time horizon of the analysis.
#' @param allocation A character value or list of characters that define the portfolio approach to use.
#' @param production_type A character value (absolute or reletive) that defines
#'  whether to use the stock or flow production for a technology.
#' @param group_vars A set of grouping variables that define the granularity of the metric.
#'
#' @return
#' @export
calculate_temperature_indicator <- function(input_results,
                                            upper_temp_threshold = 6,
                                            lower_temp_threshold = 1.5,
                                            start_year = NULL,
                                            time_horizon = NULL,
                                            allocation = NULL,
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

  r2dii.analysis::scenario_relationships

  brown_technologies <- c(
    "Oil", "Gas", "Coal", "CoalCap", "OilCap", "GasCap", "ICE"
  )

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
    filter(
      Allocation %in% allocation &
        ((ScenarioGeography == "GlobalAggregate" & Sector == "Power") |
           (ScenarioGeography == "Global" & Sector != "Power")) &
        Year >= start_year & Year <= start_year + time_horizon &
        Plan.Alloc.WtTechProd > 0
    )


  # temp <- check_group_vars(
  #   temp,
  #   group_vars
  # )

  temp <- temp %>%
    distinct(
      !!! syms(group_vars),
      Allocation, Scenario,
      Sector, Technology,
      Year,
      Scen.Alloc.WtTechProd,
      Plan.Alloc.WtTechProd
    )

  temp <- calculate_production(
    temp = temp,
    method = production_type, #use absolute or relative
    group_vars = group_vars
  )

  # adding scenario data
  scenario_temp <- temp %>%
    inner_join(scenario_relationships, by = c("Sector", "Scenario")) %>%
    ungroup()

  scenario_temp <- scenario_temp %>%
    distinct(
      !!! syms(group_vars),
      relation,
      temp,
      scen_tech_prod,
      Sector,
      Technology
    )

  scenario_temp <- scenario_temp %>%
    group_by(
      !!! syms(group_vars),
      Sector
    ) %>%
    filter(n_distinct(relation) == 3)

  temp <- scenario_temp %>%
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
    calculation_lower = scen_plan_prod_diff / scen_tech_prod_lower_bound,
    brown_technologies = brown_technologies
  )

  # calculating temperature
  temp <- find_scenario_relation(
    input = temp,
    metric_name = "temperature",
    calculation_upper = temp_reference + (temp_upper_range * factor),
    calculation_lower = temp_reference - (temp_lower_range * factor),
    brown_technologies = brown_technologies
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
  temp %>%
    mutate(
      ScenarioGeography = "Global",
      Scenario = "Aggregate"
    )
}

# TODO vintented please document params
#' Calculate relative or absolute production
#'
#' @param temp A dataframe like [r2dii.analysis::sample_results].
#' @param method A character value (absolute or reletive) that defines
#' whether to use the stock or flow production for a technology.
#' @param group_vars A set of grouping variables that define the granularity of the metric.
#'
#' @keywords internal
#' @noRd
calculate_production <- function(temp,
                                 method = "absolute",
                                 group_vars = group_vars
                                 ) {

  if (method == "relative" & method != "absolute") {
    # calculating the integral of delta
    temp <- temp %>%
      group_by(
        !!! syms(group_vars),
        Allocation,
        Scenario,
        Sector,
        Technology
      ) %>%
      mutate(
        Plan.Alloc.WtTechProd = dplyr::lead(Plan.Alloc.WtTechProd, n = 1L) - Plan.Alloc.WtTechProd, # first step is to calculate the integral of the delta over the 5 year time horizon
        Scen.Alloc.WtTechProd = dplyr::lead(Scen.Alloc.WtTechProd, n = 1L) - Scen.Alloc.WtTechProd # for both the portfolio and the scenario aligned production
      )
  }

  temp <- temp %>%
    group_by(
      !!! syms(group_vars),
      Allocation,
      Scenario,
      Sector,
      Technology
    ) %>%
    summarise(
      Plan.Alloc.WtTechProd = sum(Plan.Alloc.WtTechProd, na.rm = TRUE),
      Scen.Alloc.WtTechProd = sum(Scen.Alloc.WtTechProd, na.rm = TRUE)
    )

  temp %>%
    rename(
      scen_tech_prod = Scen.Alloc.WtTechProd,
      plan_tech_prod = Plan.Alloc.WtTechProd
    )

}

#' @noRd
find_scenario_relation <- function(input,
                                   metric_name,
                                   calculation_upper,
                                   calculation_lower,
                                   brown_technologies) {

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

  input %>%
    rename({{ metric_name }} := metric)
}



