
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.analysis <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.analysis)](https://CRAN.R-project.org/package=r2dii.analysis)
[![Codecov test
coverage](https://codecov.io/gh/RMI-PACTA/r2dii.analysis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/r2dii.analysis?branch=main)
[![R-CMD-check](https://github.com/RMI-PACTA/r2dii.analysis/actions/workflows/R.yml/badge.svg)](https://github.com/RMI-PACTA/r2dii.analysis/actions/workflows/R.yml)
<!-- badges: end -->

These tools help you to assess if a financial portfolio aligns with
climate goals. They summarize key metrics attributed to the portfolio
(e.g. production, emission factors), and calculate targets based on
climate scenarios. They implement in R the last step of the free
software ‘PACTA’ (Paris Agreement Capital Transition Assessment).
Financial institutions use ‘PACTA’ to study how their capital allocation
impacts the climate.

## Installation

Install the released version of r2dii.analysis from CRAN with:

``` r
install.packages("r2dii.analysis")
```

Or install the development version of r2dii.analysis from GitHub with:

``` r
# install.packages("pak")
pak::pak("RMI-PACTA/r2dii.analysis")
```

## Example

- Use `library()` to attach the packages you need. r2dii.analysis does
  not depend on the packages r2dii.data and r2dii.match; but we suggest
  you install them – with
  `install.packages(c("r2dii.data", "r2dii.match"))` – so you can
  reproduce our examples.

``` r
library(r2dii.data)
library(r2dii.match)
library(r2dii.analysis)
```

- Use `r2dii.match::match_name()` to identify matches between your
  loanbook and the asset level data.

``` r
matched <- match_name(loanbook_demo, abcd_demo) %>%
  prioritize()
```

### Add Scenario Targets

- Use `target_sda()` to calculate SDA targets of CO<sub>2</sub>
  emissions.

``` r
matched %>%
  target_sda(
    abcd = abcd_demo,
    co2_intensity_scenario = co2_intensity_scenario_demo,
    region_isos = region_isos_demo
  )
#> Warning: Removing rows in abcd where `emission_factor` is NA
#> # A tibble: 220 × 6
#>    sector  year region             scenario_source emission_factor_metric
#>    <chr>  <dbl> <chr>              <chr>           <chr>                 
#>  1 cement  2020 advanced economies demo_2020       projected             
#>  2 cement  2020 developing asia    demo_2020       projected             
#>  3 cement  2020 global             demo_2020       projected             
#>  4 cement  2021 advanced economies demo_2020       projected             
#>  5 cement  2021 developing asia    demo_2020       projected             
#>  6 cement  2021 global             demo_2020       projected             
#>  7 cement  2022 advanced economies demo_2020       projected             
#>  8 cement  2022 developing asia    demo_2020       projected             
#>  9 cement  2022 global             demo_2020       projected             
#> 10 cement  2023 advanced economies demo_2020       projected             
#> # ℹ 210 more rows
#> # ℹ 1 more variable: emission_factor_value <dbl>
```

- Use `target_market_share` to calculate market-share scenario targets
  at the portfolio level:

``` r
matched %>%
  target_market_share(
    abcd = abcd_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo
  )
#> # A tibble: 1,076 × 10
#>    sector     technology  year region scenario_source metric     production
#>    <chr>      <chr>      <int> <chr>  <chr>           <chr>           <dbl>
#>  1 automotive electric    2020 global demo_2020       projected     145649.
#>  2 automotive electric    2020 global demo_2020       target_cps    145649.
#>  3 automotive electric    2020 global demo_2020       target_sds    145649.
#>  4 automotive electric    2020 global demo_2020       target_sps    145649.
#>  5 automotive electric    2021 global demo_2020       projected     147480.
#>  6 automotive electric    2021 global demo_2020       target_cps    146915.
#>  7 automotive electric    2021 global demo_2020       target_sds    153332.
#>  8 automotive electric    2021 global demo_2020       target_sps    147258.
#>  9 automotive electric    2022 global demo_2020       projected     149310.
#> 10 automotive electric    2022 global demo_2020       target_cps    148155.
#> # ℹ 1,066 more rows
#> # ℹ 3 more variables: technology_share <dbl>, scope <chr>,
#> #   percentage_of_initial_production_by_scope <dbl>
```

- Or at the company level:

``` r
matched %>%
  target_market_share(
    abcd = abcd_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo,
    by_company = TRUE
  )
#> Warning: You've supplied `by_company = TRUE` and `weight_production = TRUE`.
#> This will result in company-level results, weighted by the portfolio
#> loan size, which is rarely useful. Did you mean to set one of these
#> arguments to `FALSE`?
#> # A tibble: 14,505 × 11
#>    sector    technology  year region scenario_source name_abcd metric production
#>    <chr>     <chr>      <int> <chr>  <chr>           <chr>     <chr>       <dbl>
#>  1 automoti… electric    2020 global demo_2020       Bernardi… proje…     17951.
#>  2 automoti… electric    2020 global demo_2020       Bernardi… targe…     17951.
#>  3 automoti… electric    2020 global demo_2020       Bernardi… targe…     17951.
#>  4 automoti… electric    2020 global demo_2020       Bernardi… targe…     17951.
#>  5 automoti… electric    2020 global demo_2020       Christia… proje…     11471.
#>  6 automoti… electric    2020 global demo_2020       Christia… targe…     11471.
#>  7 automoti… electric    2020 global demo_2020       Christia… targe…     11471.
#>  8 automoti… electric    2020 global demo_2020       Christia… targe…     11471.
#>  9 automoti… electric    2020 global demo_2020       Donati, … proje…      5611.
#> 10 automoti… electric    2020 global demo_2020       Donati, … targe…      5611.
#> # ℹ 14,495 more rows
#> # ℹ 3 more variables: technology_share <dbl>, scope <chr>,
#> #   percentage_of_initial_production_by_scope <dbl>
```

[Get
started](https://rmi-pacta.github.io/r2dii.analysis/articles/r2dii-analysis.html).

## Funding

This project has received funding from the [European Union LIFE
program](https://wayback.archive-it.org/12090/20210412123959/https://ec.europa.eu/easme/en/)
and the International Climate Initiative (IKI). The Federal Ministry for
the Environment, Nature Conservation and Nuclear Safety (BMU) supports
this initiative on the basis of a decision adopted by the German
Bundestag. The views expressed are the sole responsibility of the
authors and do not necessarily reflect the views of the funders. The
funders are not responsible for any use that may be made of the
information it contains.
