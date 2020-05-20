#' A prepared co2 intensity climate scenario dataset for demonstration
#'
#' @description
#' Fake co2 intensity climate scenario dataset, prepared for the software PACTA (Paris
#' Agreement Capital Transition Assessment). It imitates climate scenario data
#' (e.g. from the International Energy Agency (IEA)) including the change
#' through time in production across industrial sectors (calculated by
#' [2DII](https://2degrees-investing.org/)).
#'
#' @family demo datasets
#' @seealso [data_dictionary]
#'
#' @format
#' `co2_intensity_scenario` is a [data.frame] with columns:
#' * `region` (character): The region to which the pathway is relevant.
#' * `scenario` (character): The name of the scenario.
#' * `scenario_source` (character): The source publication from which the
#' scenario was taken.
#' * `sector` (character): The sector to which the scenario prescribes a
#' pathway.
#' * `smsp` (double): Sector market share percentage of the pathway calculated
#' in 2020.
#' * `technology` (character): The technology within the sector to which the
#' scenario prescribes a pathway.
#' * `tmsr` (double): Technology market share ratio of the pathway calculated in
#' 2020.
#' * `year` (integer): The year at which the pathway value is prescribed.
#'
#' @examples
#' head(co2_intensity_scenario)
"co2_intensity_scenario"
