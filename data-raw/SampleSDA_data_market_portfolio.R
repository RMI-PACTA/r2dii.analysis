# Source: @vintented via https://github.com/2DegreesInvesting/r2dii.analysis/
#   issues/6#issuecomment-561543097

read_and_sanitize <- function(path, sheet) {
  out <- readxl::read_excel(path, sheet = sheet, na = c("", "NaN"))
  out$Year <- as.integer(out$Year)
  out
}

path <- here::here("data-raw", "SampleSDA_data.xlsx")

portfolio <- read_and_sanitize(path, sheet = "Portfolio")
usethis::use_data(portfolio, overwrite = TRUE)

market <- read_and_sanitize(path, sheet = "Market")
usethis::use_data(market, overwrite = TRUE)
