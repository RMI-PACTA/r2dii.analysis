#' Minimal explicit loanbook and ald datasets that allow overwriting values
#'
#' These funtions are developer-oriented. They all call [tibble::tibble()] so
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
#' @seealso [[r2dii.analysis::portfolio ], [r2dii.dataraw::loanbook_demo]]
#'
#' @return A dataframe
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
                         id_2dii = NULL,
                         level = NULL,
                         score = NULL,
                         sector = NULL,
                         name_ald = NULL,
                         sector_ald = NULL,
                         ...) {
  tibble::tibble(
    id_loan = id_loan %||% "L162",
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
                     ...) {
  tibble::tibble(
    name_company = name_company %||% "shaanxi auto",
    sector = sector %||% "automotive",
    technology = technology %||% "ice",
    year = year %||% 2025,
    ...
  )
}

#' See `fake_matched()`
#' @noRd
fake_scenario <- function(scenario = NULL,
                          sector = NULL,
                          technology = NULL,
                          region = NULL,
                          value = NULL,
                          units = NULL,
                          year = NULL,
                          ...) {
  tibble::tibble(
    scenario = scenario %||% "sds",
    sector = sector %||% "automotive",
    technology = technology %||% "ice",
    region = region %||% "global",
    value = value %||% c(2, 1),
    units = units %||% "cars produced",
    year = year %||% c(2020, 2025),
    ...
  )
}

#' See `fake_matched()`
#' @noRd
fake_portfolio <- function(year = NULL,
                           scenario_geography = NULL,
                           scenario = NULL,
                           sector = NULL,
                           investor_name = NULL,
                           portfolio_name = NULL,
                           allocation = NULL,
                           plan_sec_emissions_factor = NULL,
                           scen_sec_emissions_factor = NULL,
                           ...) {
  tibble::tibble(
    scenario_geography = scenario_geography %||% "Global",
    scenario = scenario %||% "B2DS",
    sector = sector %||% "Steel",
    year = year %||% 2021L,
    investor_name = investor_name %||% "Market",
    portfolio_name = portfolio_name %||% "GlobalMarket",
    allocation = allocation %||% "PortfolioWeight",
    plan_sec_emissions_factor = plan_sec_emissions_factor %||% 1.0,
    scen_sec_emissions_factor = scen_sec_emissions_factor %||% 1.0,
    ...
  )
}

#' See `fake_matched()`
#' @noRd
fake_master <- function(sector = NULL,
                        id_loan = NULL,
                        loan_size_outstanding = NULL,
                        loan_size_credit_limit = NULL,
                        production = NULL,
                        year = NULL,
                        technology = NULL,
                        ...) {
  tibble::tibble(
    sector =   sector %||% "automotive",
    id_loan =   id_loan %||% "L151",
    loan_size_outstanding =  loan_size_outstanding %||% 1,
    loan_size_credit_limit =  loan_size_credit_limit %||% 2,
    production =  production %||% 1,
    year =  year %||% 2020,
    technology =   technology %||% "ice",
    ...
  )
}
