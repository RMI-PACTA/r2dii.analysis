#' Join a data-loanbook object to the ald and scenario
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated in favour of [join_abcd_scenario()]. See [here](https://2degreesinvesting.github.io/posts/2022-03-02-ald-becomes-abcd/)
#' for more information.
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' installed <- requireNamespace("r2dii.data", quietly = TRUE) &&
#'   requireNamespace("r2dii.match", quietly = TRUE) &&
#'   packageVersion("r2dii.match") >= "0.1.0"
#'
#' if (installed) {
#'   library(r2dii.data)
#'   library(r2dii.match)
#'
#'   valid_matches <- match_name(loanbook_demo, ald_demo) %>%
#'     prioritize()
#'
#'   valid_matches %>%
#'     join_ald_scenario(
#'       ald = ald_demo,
#'       scenario = scenario_demo_2020,
#'       region_isos = region_isos_demo
#'     )
#'
#'    # ->
#'
#'   valid_matches %>%
#'     join_abcd_scenario(
#'       abcd = abcd_demo,
#'       scenario = scenario_demo_2020,
#'       region_isos = region_isos_demo
#'     )
#' }
join_ald_scenario <- function(data,
                              ald,
                              scenario,
                              region_isos = r2dii.data::region_isos,
                              add_green_technologies = FALSE) {
  lifecycle::deprecate_warn(
    "0.2.0",
    "join_ald_scenario()",
    "join_abcd_scenario()"
    )

  if ("name_ald" %in% names(data)) {
    data <- rename(data, name_abcd = .data$name_ald)
  }

  if ("sector_ald" %in% names(data)) {
    data <- rename(data, sector_abcd = .data$sector_ald)
  }

  join_abcd_scenario(data, ald, scenario, region_isos, add_green_technologies)

}
