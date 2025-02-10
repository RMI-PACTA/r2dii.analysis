library(readr)
library(usethis)

paths <- list.files("data-raw/data_dictionary", full.names = TRUE)

out <- readr::read_csv(file = paths, show_col_types = FALSE)
data_dictionary <- out[order(out$dataset, out$column), , drop = FALSE]

usethis::use_data(data_dictionary, overwrite = TRUE)
