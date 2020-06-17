#' Minimal explicit loanbook and ald datasets that allow overwriting values
#'
#' These funtions are developer-oriented. They all call [tibble()] so
#' you can expect all the goodies that come with that.
#' * `fake_matched()` fakes the ouput of `match_name()`.
#' * `fake_portfolio` is a minimal output of [r2dii.analysis::portfolio]
#'
#' @section Params
#' The arguments are the column names of the datasets being faked. They all have
#' a default and it can be overwritten.
#'
#' @section Pros and cons
#' These functions help you to avoid duplicating test code, and help
#' the reader of your code to focus on the one thing you want to test, instead
#' of burring that thing in the much longer code you need to create a fake
#' object from scratch.
#'
#' But `fake_*()` functions hide the explicit content. If the reader of your
#' code wants to inspect the data being tested, they need to jump to the
#' function definition or call them interactively.
#'
#' @seealso [r2dii.analysis::portfolio ], [r2dii.data::loanbook_demo]
#'
#' @return A data.frame
#'
#' @examples
#' fake_matched()
#'
#' fake_matched(id = c("a", "a"), sector = c("coal", "automotive"))
#'
#' # Support for trailing commas
#' fake_matched(id = "a", )
#' @noRd
fake_matched <- function(id_loan = NULL,
                         loan_size_outstanding = NULL,
                         loan_size_credit_limit = NULL,
                         id_2dii = NULL,
                         level = NULL,
                         score = NULL,
                         sector = NULL,
                         name_ald = NULL,
                         sector_ald = NULL,
                         ...) {
  tibble(
    id_loan = id_loan %||% "L162",
    loan_size_outstanding =  loan_size_outstanding %||% 1,
    loan_size_credit_limit =  loan_size_credit_limit %||% 2,
    id_2dii = id_2dii %||% "UP1",
    level = level %||% "ultimate_parent",
    score = score %||% 1,
    sector = sector %||% "automotive",
    name_ald = name_ald %||% "shaanxi auto",
    sector_ald = sector_ald %||% "automotive",
    ...
  )
}

#' See `fake_matched()`
#' @noRd
fake_ald <- function(name_company = NULL,
                     sector = NULL,
                     technology = NULL,
                     year = NULL,
                     production = NULL,
                     emission_factor = NULL,
                     plant_location = NULL,
                     is_ultimate_owner = NULL,
                     ...) {
  tibble(
    name_company = name_company %||% "shaanxi auto",
    sector = sector %||% "automotive",
    technology = technology %||% "ice",
    year = year %||% 2025,
    production = production %||% 1,
    emission_factor = emission_factor %||% 1,
    plant_location = plant_location %||% "BF",
    is_ultimate_owner = is_ultimate_owner %||% TRUE,
    ...
  )
}

#' See `fake_matched()`
#' @noRd
fake_scenario <- function(scenario = NULL,
                          sector = NULL,
                          technology = NULL,
                          region = NULL,
                          year = NULL,
                          tmsr = NULL,
                          smsp = NULL,
                          scenario_source = NULL,
                          ...) {
  tibble(
    scenario = scenario %||% "sds",
    sector = sector %||% "automotive",
    technology = technology %||% "ice",
    region = region %||% "global",
    year = year %||% 2025,
    tmsr = tmsr %||% 0.5,
    smsp = smsp %||% -0.08,
    scenario_source = scenario_source %||% "demo_2020",
    ...
  )
}

fake_co2_scenario <- function(scenario = NULL,
                              sector = NULL,
                              region = NULL,
                              year = NULL,
                              emission_factor = NULL,
                              emission_factor_unit = NULL,
                              scenario_source = NULL,
                              ...) {
  tibble(
    scenario = scenario %||% "b2ds",
    sector = sector %||% "cement",
    region = region %||% "global",
    year = year %||% 2025,
    emission_factor = emission_factor %||% 0.6,
    emission_factor_unit = emission_factor_unit %||% "tons of CO2 per ton of cement",
    scenario_source = scenario_source %||% "demo_2020",
    ...
  )
}

#' See `fake_matched()`
#' @noRd
fake_master <- function(id_loan = NULL,
                        loan_size_outstanding = NULL,
                        loan_size_credit_limit = NULL,
                        sector = NULL,
                        name_ald = NULL,
                        technology = NULL,
                        year = NULL,
                        production = NULL,
                        plant_location = NULL,
                        scenario = NULL,
                        region = NULL,
                        tmsr = NULL,
                        smsp = NULL,
                        ...) {
  tibble(
    id_loan =   id_loan %||% "L162",
    loan_size_outstanding =  loan_size_outstanding %||% 1,
    loan_size_credit_limit =  loan_size_credit_limit %||% 2,
    sector =   sector %||% "automotive",
    name_ald = name_ald %||% "shaanxi auto",
    technology =   technology %||% "ice",
    year =  year %||% 2025,
    production =  production %||% 1,
    plant_location = plant_location %||% "BF",
    scenario = scenario %||% "sds",
    region = region %||% "global",
    tmsr = tmsr %||% 0.5,
    smsp = smsp %||% -0.08,
    ...
  )
}
