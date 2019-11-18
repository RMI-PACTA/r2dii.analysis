# Source: Added by @Clare2 via commit 8242575
path <- here::here("data-raw/Fund_ISINs_No_BBG_Data.csv")
fund_data <- dplyr::as_tibble(readr::read_csv(path))
usethis::use_data(fund_data, overwrite = TRUE)
