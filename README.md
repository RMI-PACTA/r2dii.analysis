
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/3jITMq8.png" align="right" height=40 /> Tools for Climate Scenario Analysis

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.analysis)](https://CRAN.R-project.org/package=r2dii.analysis)
[![R build
status](https://github.com/2DegreesInvesting/r2dii.analysis/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.analysis/actions)
<!-- badges: end -->

The goal of r2dii.analysis is to provide a suite of metrics and analysis
tools commonly used for climate scenario analysis.

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
# Use example configuration-files
library(r2dii.utils)
library(r2dii.analysis)
packageVersion("r2dii.analysis")
#> [1] '0.0.0.9004'

# Use a toy configuration file
restore_options <- options(r2dii_config = example_config("config_demo.yml"))
on.exit(restore_options)

# Use `start_year` from the configuration file
START.YEAR()
#> [1] 2019

sda_portfolio_target(market, portfolio)
#> Warning: Skipping sectors not present in both `market` and `portfolio`:
#> Cement, Power, Oil&Gas, Coal, Aviation, FossilFuels, Shipping, Automotive.
#> * Using `sector`: Steel.
#> * Using `start_year`: 2019.
#> * Using `target_year`: 2040.
#> # A tibble: 24 x 9
#>    Allocation Sector Scenario ScenarioGeograp… Investor.Name Portfolio.Name
#>    <chr>      <chr>  <chr>    <chr>            <chr>         <chr>         
#>  1 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  2 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  3 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  4 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  5 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  6 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  7 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  8 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  9 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#> 10 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#> # … with 14 more rows, and 3 more variables: Scen.Sec.EmissionsFactor <dbl>,
#> #   Year <int>, Plan.Sec.EmissionsFactor <dbl>

sda_portfolio_target(market, portfolio, sector = "Steel")
#> * Using `sector`: Steel.
#> * Using `start_year`: 2019.
#> * Using `target_year`: 2040.
#> # A tibble: 24 x 9
#>    Allocation Sector Scenario ScenarioGeograp… Investor.Name Portfolio.Name
#>    <chr>      <chr>  <chr>    <chr>            <chr>         <chr>         
#>  1 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  2 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  3 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  4 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  5 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  6 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  7 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  8 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  9 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#> 10 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#> # … with 14 more rows, and 3 more variables: Scen.Sec.EmissionsFactor <dbl>,
#> #   Year <int>, Plan.Sec.EmissionsFactor <dbl>

# This configuration file lacks `start_year`
options(r2dii_config = example_config("config-toy.yml"))
START.YEAR()
#> NULL

# Fails
sda_portfolio_target(market, portfolio, sector = "Steel")
#> * Using `sector`: Steel.
#> Error: `start_year` can't be NULL.
#> Did you forget `start_year` in a configuration file or as an argument?

# Passes
sda_portfolio_target(market, portfolio, sector = "Steel", start_year = "2019")
#> * Using `sector`: Steel.
#> * Using `start_year`: 2019.
#> * Using `target_year`: 2040.
#> # A tibble: 24 x 9
#>    Allocation Sector Scenario ScenarioGeograp… Investor.Name Portfolio.Name
#>    <chr>      <chr>  <chr>    <chr>            <chr>         <chr>         
#>  1 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  2 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  3 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  4 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  5 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  6 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  7 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  8 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#>  9 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#> 10 Portfolio… Steel  B2DS     Global           Investor1     Portfolio1    
#> # … with 14 more rows, and 3 more variables: Scen.Sec.EmissionsFactor <dbl>,
#> #   Year <int>, Plan.Sec.EmissionsFactor <dbl>
```
