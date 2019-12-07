
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
library(r2dii.analysis)
packageVersion("r2dii.analysis")
#> [1] '0.0.0.9001'

market
#> # A tibble: 24 x 9
#>    Investor.Name Portfolio.Name Scenario ScenarioGeograp~ Allocation  Year
#>    <chr>         <chr>          <chr>    <chr>            <chr>      <int>
#>  1 Market        GlobalMarket   B2DS     Global           Portfolio~  2019
#>  2 Market        GlobalMarket   B2DS     Global           Portfolio~  2020
#>  3 Market        GlobalMarket   B2DS     Global           Portfolio~  2021
#>  4 Market        GlobalMarket   B2DS     Global           Portfolio~  2022
#>  5 Market        GlobalMarket   B2DS     Global           Portfolio~  2023
#>  6 Market        GlobalMarket   B2DS     Global           Portfolio~  2024
#>  7 Market        GlobalMarket   B2DS     Global           Portfolio~  2025
#>  8 Market        GlobalMarket   B2DS     Global           Portfolio~  2026
#>  9 Market        GlobalMarket   B2DS     Global           Portfolio~  2027
#> 10 Market        GlobalMarket   B2DS     Global           Portfolio~  2028
#> # ... with 14 more rows, and 3 more variables: Sector <chr>,
#> #   Plan.Sec.EmissionsFactor <dbl>, Scen.Sec.EmissionsFactor <dbl>

portfolio
#> # A tibble: 24 x 9
#>    Investor.Name Portfolio.Name Scenario ScenarioGeograp~ Allocation  Year
#>    <chr>         <chr>          <chr>    <chr>            <chr>      <int>
#>  1 Investor1     Portfolio1     B2DS     Global           Portfolio~  2019
#>  2 Investor1     Portfolio1     B2DS     Global           Portfolio~  2020
#>  3 Investor1     Portfolio1     B2DS     Global           Portfolio~  2021
#>  4 Investor1     Portfolio1     B2DS     Global           Portfolio~  2022
#>  5 Investor1     Portfolio1     B2DS     Global           Portfolio~  2023
#>  6 Investor1     Portfolio1     B2DS     Global           Portfolio~  2024
#>  7 Investor1     Portfolio1     B2DS     Global           Portfolio~  2025
#>  8 Investor1     Portfolio1     B2DS     Global           Portfolio~  2026
#>  9 Investor1     Portfolio1     B2DS     Global           Portfolio~  2027
#> 10 Investor1     Portfolio1     B2DS     Global           Portfolio~  2028
#> # ... with 14 more rows, and 3 more variables: Sector <chr>,
#> #   Plan.Sec.EmissionsFactor <dbl>, Scen.Sec.EmissionsFactor <dbl>

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
