`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

commas <- function(...) paste0(..., collapse = ", ")
