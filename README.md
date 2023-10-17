
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.analysis <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.analysis)](https://CRAN.R-project.org/package=r2dii.analysis)
[![Codecov test
coverage](https://codecov.io/gh/rmi-pacta/r2dii.analysis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rmi-pacta/r2dii.analysis?branch=main)
[![Codecov test
coverage](https://codecov.io/gh/RMI-PACTA/r2dii.analysis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/r2dii.analysis?branch=main)
[![R-CMD-check](https://github.com/RMI-PACTA/r2dii.analysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/r2dii.analysis/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

These tools help you to assess if a financial portfolio aligns with
climate goals. They summarize key metrics attributed to the portfolio
(e.g. production, emission factors), and calculate targets based on
climate scenarios. They implement in R the last step of the free
software ‘PACTA’ (Paris Agreement Capital Transition Assessment;
<https://www.transitionmonitor.com/>). Financial institutions use
‘PACTA’ to study how their capital allocation impacts the climate.

## Installation

Install the released version of r2dii.analysis from CRAN with:

``` r
install.packages("r2dii.analysis")
```

Or install the development version of r2dii.analysis from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("RMI-PACTA/r2dii.analysis")
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

- Use `target_sda()` to calculate SDA targets of CO2 emissions.

``` r
matched %>%
  target_sda(
    abcd = abcd_demo,
    co2_intensity_scenario = co2_intensity_scenario_demo,
    region_isos = region_isos_demo
  )
#> Warning: Removing rows in abcd where `emission_factor` is NA
#> # A tibble: 96 × 6
#>    sector  year region             scenario_source emission_factor_metric
#>    <chr>  <dbl> <chr>              <chr>           <chr>                 
#>  1 steel   2021 advanced economies demo_2020       projected             
#>  2 steel   2021 global             demo_2020       projected             
#>  3 steel   2022 advanced economies demo_2020       projected             
#>  4 steel   2022 global             demo_2020       projected             
#>  5 steel   2024 advanced economies demo_2020       projected             
#>  6 steel   2024 global             demo_2020       projected             
#>  7 steel   2025 advanced economies demo_2020       projected             
#>  8 steel   2025 global             demo_2020       projected             
#>  9 steel   2027 advanced economies demo_2020       projected             
#> 10 steel   2027 global             demo_2020       projected             
#> # ℹ 86 more rows
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
#> # A tibble: 1,232 × 10
#>    sector     technology  year region scenario_source metric     production
#>    <chr>      <chr>      <dbl> <chr>  <chr>           <chr>           <dbl>
#>  1 automotive electric    2020 global demo_2020       projected       3664.
#>  2 automotive electric    2020 global demo_2020       target_cps      3664.
#>  3 automotive electric    2020 global demo_2020       target_sds      3664.
#>  4 automotive electric    2020 global demo_2020       target_sps      3664.
#>  5 automotive electric    2021 global demo_2020       projected       8472.
#>  6 automotive electric    2021 global demo_2020       target_cps      3845.
#>  7 automotive electric    2021 global demo_2020       target_sds      4766.
#>  8 automotive electric    2021 global demo_2020       target_sps      3894.
#>  9 automotive electric    2022 global demo_2020       projected       8436.
#> 10 automotive electric    2022 global demo_2020       target_cps      4023.
#> # ℹ 1,222 more rows
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
#> # A tibble: 3,200 × 11
#>    sector    technology  year region scenario_source name_abcd metric production
#>    <chr>     <chr>      <dbl> <chr>  <chr>           <chr>     <chr>       <dbl>
#>  1 automoti… electric    2020 global demo_2020       large au… proje…       713.
#>  2 automoti… electric    2020 global demo_2020       large au… targe…       713.
#>  3 automoti… electric    2020 global demo_2020       large au… targe…       713.
#>  4 automoti… electric    2020 global demo_2020       large au… targe…       713.
#>  5 automoti… electric    2020 global demo_2020       large au… proje…       535.
#>  6 automoti… electric    2020 global demo_2020       large au… targe…       535.
#>  7 automoti… electric    2020 global demo_2020       large au… targe…       535.
#>  8 automoti… electric    2020 global demo_2020       large au… targe…       535.
#>  9 automoti… electric    2020 global demo_2020       large au… proje…       690.
#> 10 automoti… electric    2020 global demo_2020       large au… targe…       690.
#> # ℹ 3,190 more rows
#> # ℹ 3 more variables: technology_share <dbl>, scope <chr>,
#> #   percentage_of_initial_production_by_scope <dbl>
```

### Utility Functions

The `target_*()` functions provide shortcuts for common operations. They
wrap some utility functions that you may also use directly:

- Use `join_abcd_scenario()` to join a matched dataset to the relevant
  scenario data, and to pick assets in the relevant regions.

``` r
loanbook_joined_to_abcd_scenario <- matched %>%
  join_abcd_scenario(
    abcd = abcd_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo
  )
```

- Use `summarize_weighted_production()` with different grouping
  arguments to calculate scenario-targets:

``` r
# portfolio level
loanbook_joined_to_abcd_scenario %>%
  summarize_weighted_production(scenario, tmsr, smsp, region)
#> # A tibble: 558 × 9
#>    sector_abcd technology  year scenario  tmsr    smsp region
#>    <chr>       <chr>      <dbl> <chr>    <dbl>   <dbl> <chr> 
#>  1 automotive  electric    2020 cps       1    0       global
#>  2 automotive  electric    2020 sds       1    0       global
#>  3 automotive  electric    2020 sps       1    0       global
#>  4 automotive  electric    2021 cps       1.12 0.00108 global
#>  5 automotive  electric    2021 sds       1.16 0.00653 global
#>  6 automotive  electric    2021 sps       1.14 0.00137 global
#>  7 automotive  electric    2022 cps       1.24 0.00213 global
#>  8 automotive  electric    2022 sds       1.32 0.0131  global
#>  9 automotive  electric    2022 sps       1.29 0.00273 global
#> 10 automotive  electric    2023 cps       1.35 0.00316 global
#> # ℹ 548 more rows
#> # ℹ 2 more variables: weighted_production <dbl>,
#> #   weighted_technology_share <dbl>

# company level
loanbook_joined_to_abcd_scenario %>%
  summarize_weighted_production(scenario, tmsr, smsp, region, name_abcd)
#> # A tibble: 1,953 × 10
#>    sector_abcd technology  year scenario  tmsr  smsp region name_abcd           
#>    <chr>       <chr>      <dbl> <chr>    <dbl> <dbl> <chr>  <chr>               
#>  1 automotive  electric    2020 cps          1     0 global large automotive co…
#>  2 automotive  electric    2020 cps          1     0 global large automotive co…
#>  3 automotive  electric    2020 cps          1     0 global large automotive co…
#>  4 automotive  electric    2020 cps          1     0 global large hdv company t…
#>  5 automotive  electric    2020 sds          1     0 global large automotive co…
#>  6 automotive  electric    2020 sds          1     0 global large automotive co…
#>  7 automotive  electric    2020 sds          1     0 global large automotive co…
#>  8 automotive  electric    2020 sds          1     0 global large hdv company t…
#>  9 automotive  electric    2020 sps          1     0 global large automotive co…
#> 10 automotive  electric    2020 sps          1     0 global large automotive co…
#> # ℹ 1,943 more rows
#> # ℹ 2 more variables: weighted_production <dbl>,
#> #   weighted_technology_share <dbl>
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
