#' Get the current year from the system time
#'
#' @inherit lubridate::year
#' @export
#'
#' @examples
#' get_current_year()
get_current_year <- function(x) {
  lubridate::year(Sys.time())
}
