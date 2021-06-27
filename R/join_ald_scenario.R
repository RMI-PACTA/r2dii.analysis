#' Join a data-loanbook object to the ald and scenario
#'
#' `join_ald_scenario()` is a simple wrapper of several calls to
#' `dplyr::join_*()`, forming the master dataset to be used in later steps of
#' the analysis.
#'
#' @param data A data frame like the output of
#'   [r2dii.match::prioritize()].
#' @param ald An asset level data frame like [r2dii.data::ald_demo].
#' @param scenario A scenario data frame like [r2dii.data::scenario_demo_2020].
#' @param region_isos A data frame like [r2dii.data::region_isos] (default).
#'
#' @return Returns a fully joined data frame, linking portfolio, ald and
#'   scenario.
#' @export
#'
#' @family utility functions
#'
#' @examples
#' installed <- requireNamespace("r2dii.data", quietly = TRUE) &&
#'   requireNamespace("r2dii.match", quietly = TRUE)
#' if (!installed) stop("Please install r2dii.match and r2dii.data")
#'
#' library(r2dii.data)
#' library(r2dii.match)
#'
#' valid_matches <- match_name(loanbook_demo, ald_demo) %>%
#'   # WARNING: Remember to validate matches (see `?prioritize`)
#'   prioritize()
#'
#' valid_matches %>%
#'   join_ald_scenario(
#'     ald = ald_demo,
#'     scenario = scenario_demo_2020,
#'     region_isos = region_isos_demo
#'   )
join_ald_scenario <- function(data,
                              ald,
                              scenario,
                              region_isos = r2dii.data::region_isos) {
  check_portfolio_ald_scenario(data, ald, scenario)

  # Track provenance to avoid clash in the column name "source"
  region_isos <- region_isos %>%
    rename(scenario_source = .data$source)

  ald <- modify_at_(ald, "sector", tolower)
  ald <- modify_at_(ald, "technology", tolower)
  out <- data %>%
    left_join(ald, by = ald_columns()) %>%
    mutate(plant_location = tolower(plant_location)) %>%
    left_join(region_isos, by = c("plant_location" = "isos")) %>%
    warn_if_has_zero_rows("Joining `region_isos` outputs 0 rows.")
  all_company <- unique(out$name_ald)
  all_region <- unique(out$region)
  for (one_region in all_region) {
    # print(one_region)
    for (company in all_company) {
      filtered_data <- out %>%
        filter(
          name_ald == company
        )
      sector_company <- filtered_data$sector_ald[1]
      all_tech <- green_or_brown %>%
        filter(sector == sector_company)
      all_tech <- all_tech$technology
      green_tech <- green_or_brown %>%
        filter(
          sector == sector_company,
          green_or_brown == "green"
        )
      green_tech <- green_tech$technology
      if (sector_company == "automotive") {
        green_tech <- c()
        if (filtered_data %>%
          filter(
            technology == "ice",
            region == one_region
          ) %>%
          nrow() > 0) {
          green_tech <- c(green_tech, "electric", "hybrid", "fuelcell")
        }
        if (filtered_data %>%
          filter(
            technology == "ice_hdv",
            region == one_region
          ) %>%
          nrow() > 0) {
          green_tech <- c(green_tech, "electric_hdv", "hybrid_hdv", "fuelcell_hdv")
        }
      }
      main_tech <- 0
      max <- 0
      for (i in all_tech) {
        if (filtered_data %>%
          filter(
            technology == i,
            region == one_region
          ) %>%
          nrow() > max) {
          max <- filtered_data %>%
            filter(
              technology == i,
              region == one_region
            ) %>%
            nrow()
          main_tech <- i
        }
      }
      if (max > 0) {
        for (tech in green_tech) {
          if (out %>%
            filter(
              technology == tech,
              name_ald == company,
              region == one_region
            ) %>%
            nrow() == 0) {
            out <- rbind(out, out %>%
              filter(
                name_ald == company,
                technology == main_tech,
                region == one_region
              ) %>%
              mutate(
                technology = tech,
                production = 0
              ))
          }
        }
      }
    }
  }
  out <- out %>%
    inner_join(scenario, by = c(scenario_columns(), "scenario_source", "region")) %>%
    warn_if_has_zero_rows("Joining `scenario` outputs 0 rows.") %>%
    mutate(plant_location = tolower(.data$plant_location)) # %>%
  out
}

warn_if_has_zero_rows <- function(data, message) {
  if (nrow(data) == 0L) warn(message = message, class = "has_zero_rows")
  invisible(data)
}

check_portfolio_ald_scenario <- function(valid_matches, ald, scenario) {
  check_crucial_names(valid_matches, names(ald_columns()))
  walk_(names(ald_columns()), ~ check_no_value_is_missing(valid_matches, .x))

  check_crucial_names(
    ald, c("name_company", "plant_location", unname(scenario_columns()))
  )
  walk_(
    c("name_company", unname(scenario_columns())),
    ~ check_no_value_is_missing(ald, .x)
  )


  check_crucial_names(scenario, c(scenario_columns(), "scenario_source", "region"))
  walk_(
    c(scenario_columns(), "scenario_source", "region"),
    ~ check_no_value_is_missing(scenario, .x)
  )

  invisible(valid_matches)
}

ald_columns <- function() {
  c(
    name_ald = "name_company",
    sector_ald = "sector"
  )
}

scenario_columns <- function() {
  c(
    sector_ald = "sector",
    technology = "technology",
    year = "year"
  )
}
