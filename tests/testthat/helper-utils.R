#' A copy of utils::hasName()
#'
#' utils::hasName() exists since R 3.4 so checks with older R versions fail. An
#' alternative solution is to use the backports package, but seems an overkill
#' at this stage -- as this tiny code is all we need.
#' @noRd
has_name <- function(x, name) {
  match(name, names(x), nomatch = 0L) > 0L
}
