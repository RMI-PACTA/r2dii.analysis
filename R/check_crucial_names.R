#' Check if a named object contains expected names
#'
#' Based on fgeo.tool::check_crucial_names()
#'
#' @param x A named object.
#' @param expected_names String; expected names of `x`.
#'
#' @return Invisible `x`, or an error with informative message.
#'
#' @examples
#' x <- c(a = 1)
#' check_crucial_names(x, "a")
#' try(check_crucial_names(x, "bad"))
#' @noRd
check_crucial_names <- function(x, expected_names) {
  stopifnot(rlang::is_named(x))
  stopifnot(is.character(expected_names))

  ok <- all(unique(expected_names) %in% names(x))
  if (!ok) {
    abort_missing_names(sort(setdiff(expected_names, names(x))))
  }

  invisible(x)
}

abort_missing_names <- function(missing_names) {
  nms <- paste0(sprintf("`%s`", missing_names), collapse = ", ")
  msg <- paste0("Must have missing names:\n", nms, ".")
  abort(msg, class = "missing_names")
}
