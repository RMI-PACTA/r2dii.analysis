
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.analysis <a href='https://github.com/2DegreesInvesting/r2dii.analysis'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.analysis)](https://CRAN.R-project.org/package=r2dii.analysis)
[![Codecov test
coverage](https://codecov.io/gh/2degreesinvesting/r2dii.analysis/branch/master/graph/badge.svg)](https://codecov.io/gh/2degreesinvesting/r2dii.analysis?branch=master)
[![R build
status](https://github.com/2DegreesInvesting/r2dii.analysis/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.analysis/actions)
[![R build
status](https://github.com/2degreesinvesting/r2dii.analysis/workflows/R-CMD-check/badge.svg)](https://github.com/2degreesinvesting/r2dii.analysis/actions)
<!-- badges: end -->

These tools help you to assess if a financial portfolio aligns with
climate goals. They summarize key metrics attributed to the portfolio
(e.g. production, emission factors), and calculate targets based on
climate scenarios. They implement in R the last step of the free
software ‘PACTA’ (Paris Agreement Capital Transition Assessment;
<https://2degrees-investing.org/>). Financial institutions use ‘PACTA’
to study how their capital allocation impacts the climate.

## Installation

Install the development version of r2dii.analysis with:

``` r
# install.packages("devtools")
devtools::install_github("2DegreesInvesting/r2dii.analysis")
```

## Example

  - Use `library()` to attach the packages you need. r2dii.analysis does
    not depend on the packages r2dii.data and r2dii.match; but we
    suggest you install them – with `install.packages(c("r2dii.data",
    "r2dii.match"))` – so you can reproduce our examples.

<!-- end list -->

``` r
library(r2dii.data)
library(r2dii.match)
library(r2dii.analysis)
```

  - Use `r2dii.match::match_name()` to identify matches between your
    loanbook and the asset level data.

<!-- end list -->

``` r
matched <- match_name(loanbook_demo, ald_demo) %>%
  prioritize()
```

### Add Scenario Targets

  - Use `target_sda()` to calculate SDA targets of CO2 emissions.

<!-- end list -->

``` r
matched %>%
  target_sda(
    ald = ald_demo,
    co2_intensity_scenario = co2_intensity_scenario_demo
  )
#> # A tibble: 28 x 4
#> # Groups:   sector [1]
#>    sector  year emission_factor_metric emission_factor_value
#>    <chr>  <dbl> <chr>                                  <dbl>
#>  1 cement  2020 projected                              0.664
#>  2 cement  2020 target                                 0.669
#>  3 cement  2020 scenario_benchmark                     0.7  
#>  4 cement  2021 projected                              0.665
#>  5 cement  2021 target                                 0.612
#>  6 cement  2021 scenario_benchmark                     0.64 
#>  7 cement  2022 projected                              0.666
#>  8 cement  2022 target                                 0.555
#>  9 cement  2022 scenario_benchmark                     0.580
#> 10 cement  2023 projected                              0.667
#> # … with 18 more rows
```

  - Use `target_market_share` to calculate market-share scenario targets
    at the portfolio level:

<!-- end list -->

``` r
matched %>%
  target_market_share(
    ald = ald_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo
  )
#> # A tibble: 1,170 x 7
#>    sector technology  year region scenario_source weighted_produc…
#>    <chr>  <chr>      <int> <chr>  <chr>           <chr>           
#>  1 autom… electric    2020 global demo_2020       projected       
#>  2 autom… electric    2020 global demo_2020       normalized_corp…
#>  3 autom… electric    2020 global demo_2020       target_cps      
#>  4 autom… electric    2020 global demo_2020       target_sds      
#>  5 autom… electric    2020 global demo_2020       target_sps      
#>  6 autom… hybrid      2020 global demo_2020       projected       
#>  7 autom… hybrid      2020 global demo_2020       normalized_corp…
#>  8 autom… hybrid      2020 global demo_2020       target_cps      
#>  9 autom… hybrid      2020 global demo_2020       target_sds      
#> 10 autom… hybrid      2020 global demo_2020       target_sps      
#> # … with 1,160 more rows, and 1 more variable: weighted_production_value <dbl>
```

  - Or at the company level:

<!-- end list -->

``` r
matched %>%
  target_market_share(
    ald = ald_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo,
    by_company = TRUE
  )
#> # A tibble: 15,945 x 8
#>    sector technology  year region scenario_source name_ald weighted_produc…
#>    <chr>  <chr>      <int> <chr>  <chr>           <chr>    <chr>           
#>  1 autom… electric    2020 global demo_2020       shangha… projected       
#>  2 autom… electric    2020 global demo_2020       shangha… normalized_corp…
#>  3 autom… electric    2020 global demo_2020       shangha… target_cps      
#>  4 autom… electric    2020 global demo_2020       shangha… target_sds      
#>  5 autom… electric    2020 global demo_2020       shangha… target_sps      
#>  6 autom… electric    2020 global demo_2020       sichuan… projected       
#>  7 autom… electric    2020 global demo_2020       sichuan… normalized_corp…
#>  8 autom… electric    2020 global demo_2020       sichuan… target_cps      
#>  9 autom… electric    2020 global demo_2020       sichuan… target_sds      
#> 10 autom… electric    2020 global demo_2020       sichuan… target_sps      
#> # … with 15,935 more rows, and 1 more variable: weighted_production_value <dbl>
```

### Utility Functions

The `target_*()` functions provide shortcuts for common operations. They
wrap some utility functions that you may also use directly:

  - Use `join_ald_scenario()` to join a matched dataset to the relevant
    scenario data, and to pick assets in the relevant regions.

<!-- end list -->

``` r
loanbook_joined_to_ald_scenario <- matched %>%
  join_ald_scenario(
    ald = ald_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo
  )
```

  - Use `summarize_weighted_production()` with different grouping
    arguments to calculate scenario-targets:

<!-- end list -->

``` r
# portfolio level
loanbook_joined_to_ald_scenario %>%
  summarize_weighted_production(scenario, tmsr, smsp, region)
#> # A tibble: 702 x 8
#>    sector     technology  year scenario  tmsr    smsp region weighted_production
#>    <chr>      <chr>      <int> <chr>    <dbl>   <dbl> <chr>                <dbl>
#>  1 automotive electric    2020 cps       1    0       global             148935.
#>  2 automotive electric    2020 sds       1    0       global             148935.
#>  3 automotive electric    2020 sps       1    0       global             148935.
#>  4 automotive electric    2021 cps       1.12 0.00108 global             150875.
#>  5 automotive electric    2021 sds       1.16 0.00653 global             150875.
#>  6 automotive electric    2021 sps       1.14 0.00137 global             150875.
#>  7 automotive electric    2022 cps       1.24 0.00213 global             152816.
#>  8 automotive electric    2022 sds       1.32 0.0131  global             152816.
#>  9 automotive electric    2022 sps       1.29 0.00273 global             152816.
#> 10 automotive electric    2023 cps       1.35 0.00316 global             154757.
#> # … with 692 more rows

# company level
loanbook_joined_to_ald_scenario %>%
  summarize_weighted_production(scenario, tmsr, smsp, region, name_ald)
#> # A tibble: 9,567 x 9
#>    sector technology  year scenario  tmsr  smsp region name_ald weighted_produc…
#>    <chr>  <chr>      <int> <chr>    <dbl> <dbl> <chr>  <chr>               <dbl>
#>  1 autom… electric    2020 cps          1     0 global shangha…            5140.
#>  2 autom… electric    2020 cps          1     0 global sichuan…            5985.
#>  3 autom… electric    2020 cps          1     0 global singula…            8674.
#>  4 autom… electric    2020 cps          1     0 global south-e…           14409.
#>  5 autom… electric    2020 cps          1     0 global suzuki …            6019.
#>  6 autom… electric    2020 cps          1     0 global tata gr…             876.
#>  7 autom… electric    2020 cps          1     0 global tesla i…            6208.
#>  8 autom… electric    2020 cps          1     0 global toyota …           19860.
#>  9 autom… electric    2020 cps          1     0 global volkswa…            9258.
#> 10 autom… electric    2020 cps          1     0 global wheego              9804.
#> # … with 9,557 more rows
```
