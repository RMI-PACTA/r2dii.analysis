project_meta_investor_name <- function(inc_project_metaportfolio,
                                       file = r2dii.utils::get_config()) {
  if (is.logical(inc_project_metaportfolio) && inc_project_metaportfolio) {
    paste0("Project ", meta_investor_name(file = file))
  } else {
    NULL
  }
}

project_meta_portfolio_name <- function(inc_project_metaportfolio,
                                        file = r2dii.utils::get_config()) {
  if (is.logical(inc_project_metaportfolio) && inc_project_metaportfolio) {
    paste0("Project ", meta_portfolio_name(file = file))
  } else {
    NULL
  }
}

# Checks whether a variable is a dataframe. Considers also logicals and null values.
is_dataframe_with_some_row <- function(data) {
  is.data.frame(data) && nrow(data) > 0L
}

is_na_or_empty_string <- function(x) {
  has_length_1 <- identical(length(x), 1L)
  stopifnot(has_length_1)

  is.na(x) || identical(x, "")
}
