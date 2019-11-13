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

first_char_up <- function(x) {
  x <- paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
  x
}

clean_punctuation <- function(x) {
  x <- gsub("\u00F3", "o", x)
  x <- gsub("&", " and ", x)
  x <- gsub("\u00E1", "a", x)
  x <- gsub("/", " ", x)
  x <- gsub("\u00E4", "ae", x)
  x <- gsub("\u00F6", "oe", x)
  x <- gsub("\u00FC", "ue", x)
  x <- gsub("\u00C4", "Ae", x)
  x <- gsub("\u00D6", "Oe", x)
  x <- gsub("\u00DC", "Ue", x)

  x
}

# Checks whether a variable is a dataframe. Considers also logicals and null values.
data_check <- function(df) {
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

path_proj <- function(project, parent = NULL) {
  out <- with_path_in_10_projects(project)()
  if (!is.null(parent)) {
    out <- fs::path(parent, project)
  }

  out
}
