
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/3jITMq8.png" align="right" height=40 /> TODO Add description

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.analysis)](https://CRAN.R-project.org/package=r2dii.analysis)
<!-- badges: end -->

The goal of r2dii.analysis is to TODO add gaol.

## Installation

Install the development version of r2dii.analysis with something like
this:

``` r
# install.packages("devtools")

# To install from a private repo, see ?usethis::browse_github_token()
devtools::install_github("2DegreesInvesting/r2dii.analysis", auth_token = "abc")
```

## Example

``` r
## TODO: Add example
library(r2dii.analysis)

sda_calculation(
  market = market,
  port = portfolio,
  ref_sectors = c("Cement", "Steel"),
  ref_scenario = "B2DS",
  start_year = 2019,
  target_year = 2040
)
#> Warning in sda_calculation(market = market, port = portfolio, ref_sectors =
#> c("Cement", : partial argument match of 'market' to 'market_data'
#> Warning in sda_calculation(market = market, port = portfolio, ref_sectors =
#> c("Cement", : partial argument match of 'port' to 'port_data'
#> Error in sda_calculation(market = market, port = portfolio, ref_sectors = c("Cement", : unused argument (ref_sectors = c("Cement", "Steel"))
```
