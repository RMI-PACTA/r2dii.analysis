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

path_project <- function(project, parent = NULL) {
  out <- with_path_in_10_projects(project)()
  if (!is.null(parent)) {
    out <- fs::path(parent, project)
  }

  out
}

convert_special_characters <- function(x) {
  out <- x

  out <- gsub("\u00F3", "o", out)
  out <- gsub("&", " and ", out)
  out <- gsub("\u00E1", "a", out)
  out <- gsub("/", " ", out)
  out <- gsub("\u00E4", "ae", out)
  out <- gsub("\u00F6", "oe", out)
  out <- gsub("\u00FC", "ue", out)
  out <- gsub("\u00C4", "Ae", out)
  out <- gsub("\u00D6", "Oe", out)
  out <- gsub("\u00DC", "Ue", out)

  out
}

# Checks whether a variable is a dataframe. Considers also logicals and null values.
is_dataframe_with_some_row <- function(df) {
  if (is.data.frame(df)) {
    if (nrow(df) > 0) {
      check <- TRUE
    } else {
      check <- FALSE
    }
  } else {
    check <- FALSE
  }

  return(check)
}

# Checks whether a value is null or blank
is_blank_na <- function(x) {
  if (is.na(x) | x == "") {
    flag <- TRUE
  } else {
    flag <- FALSE
  }
  flag
}
