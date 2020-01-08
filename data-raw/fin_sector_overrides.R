# Source: Added by @Clare2 via commit 8242575
path <- here::here("data-raw/fin_sector_overrides.csv")
fin_sector_overrides <- tibble::as_tibble(readr::read_csv(path))
usethis::use_data(fin_sector_overrides, overwrite = TRUE)
