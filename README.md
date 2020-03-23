
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
#> Error in library(r2dii.analysis): there is no package called 'r2dii.analysis'
packageVersion("r2dii.analysis")
#> Error in packageVersion("r2dii.analysis"): there is no package called 'r2dii.analysis'

# Use a toy configuration file
restore_options <- options(r2dii_config = example_config("config_demo.yml"))
on.exit(restore_options)

# Use `start_year` from the configuration file
START.YEAR()
#> [1] 2019

sda_portfolio_target(market, portfolio)
#> Error in sda_portfolio_target(market, portfolio): could not find function "sda_portfolio_target"

sda_portfolio_target(market, portfolio, sector = "Steel")
#> Error in sda_portfolio_target(market, portfolio, sector = "Steel"): could not find function "sda_portfolio_target"

# This configuration file lacks `start_year`
options(r2dii_config = example_config("config-toy.yml"))
START.YEAR()
#> NULL

# Fails
sda_portfolio_target(market, portfolio, sector = "Steel")
#> Error in sda_portfolio_target(market, portfolio, sector = "Steel"): could not find function "sda_portfolio_target"

# Passes
sda_portfolio_target(market, portfolio, sector = "Steel", start_year = "2019")
#> Error in sda_portfolio_target(market, portfolio, sector = "Steel", start_year = "2019"): could not find function "sda_portfolio_target"
```
