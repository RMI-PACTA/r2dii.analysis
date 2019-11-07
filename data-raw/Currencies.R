# Source: Added by @Clare2 via commit 8242575
path <- here::here("data-raw/Currencies.rda")
# FIXME: Rename to lowercase "currencies"
Currencies <- tibble::as_tibble(readr::read_rds(path))
usethis::use_data(Currencies, overwrite = TRUE)
