# Source: Added by @Clare2 via commit 8242575
path <- here::here("data-raw/Fin-Sector-Overrides.csv")
fin_data <- dplyr::as_tibble(readr::read_csv(path))
usethis::use_data(fin_data, overwrite = TRUE)

