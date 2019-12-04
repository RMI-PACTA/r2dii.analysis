# Source: @vintented via https://github.com/2DegreesInvesting/r2dii.analysis/
#   issues/6#issuecomment-561543097
path <- here::here("data-raw", "SampleSDA_data.xlsx")

portfolio <- readxl::read_excel(path, sheet = "Portfolio")
usethis::use_data(portfolio, overwrite = TRUE)

market <- readxl::read_excel(path, sheet = "Market")
usethis::use_data(market, overwrite = TRUE)
