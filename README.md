
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.analysis <img src="man/figures/logo.svg" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.analysis)](https://CRAN.R-project.org/package=r2dii.analysis)
[![Codecov test
coverage](https://codecov.io/gh/2degreesinvesting/r2dii.analysis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/2degreesinvesting/r2dii.analysis?branch=main)
[![Codecov test
coverage](https://codecov.io/gh/2DegreesInvesting/r2dii.analysis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/2DegreesInvesting/r2dii.analysis?branch=main)
[![R-CMD-check](https://github.com/2DegreesInvesting/r2dii.analysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/2DegreesInvesting/r2dii.analysis/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

These tools help you to assess if a financial portfolio aligns with
climate goals. They summarize key metrics attributed to the portfolio
(e.g. production, emission factors), and calculate targets based on
climate scenarios. They implement in R the last step of the free
software ‘PACTA’ (Paris Agreement Capital Transition Assessment;
<https://2degrees-investing.org/>). Financial institutions use ‘PACTA’
to study how their capital allocation impacts the climate.

## Installation

Install the released version of r2dii.analysis from CRAN with:

``` r
install.packages("r2dii.analysis")
```

Or install the development version of r2dii.analysis from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("2DegreesInvesting/r2dii.analysis")
```

[How to raise an
issue?](https://2degreesinvesting.github.io/posts/2020-06-26-instructions-to-raise-an-issue/)

## Example

-   Use `library()` to attach the packages you need. r2dii.analysis does
    not depend on the packages r2dii.data and r2dii.match; but we
    suggest you install them – with
    `install.packages(c("r2dii.data", "r2dii.match"))` – so you can
    reproduce our examples.

``` r
library(r2dii.data)
library(r2dii.match)
library(r2dii.analysis)
```

-   Use `r2dii.match::match_name()` to identify matches between your
    loanbook and the asset level data.

``` r
matched <- match_name(loanbook_demo, abcd_demo) %>%
  prioritize()
```

### Add Scenario Targets

-   Use `target_sda()` to calculate SDA targets of CO2 emissions.

``` r
matched %>%
  target_sda(
    abcd = abcd_demo,
    co2_intensity_scenario = co2_intensity_scenario_demo,
    region_isos = region_isos_demo
  )
#> Warning: Column `name_ald` is deprecated as of r2dii.match 0.1.0, please use
#> `name_abcd` instead.
#> Warning: Column `sector_ald` is deprecated as of r2dii.analysis 0.2.0, please use
#> `sector_abcd` instead.
#> Warning: Removing abcd rows where `emission_factor` is NA
#> # A tibble: 166 × 6
#>    sector  year region         scenario_source emission_factor… emission_factor…
#>    <chr>  <dbl> <chr>          <chr>           <chr>                       <dbl>
#>  1 cement  2013 advanced econ… demo_2020       projected                  0.0217
#>  2 cement  2013 developing as… demo_2020       projected                  0.0606
#>  3 cement  2013 global         demo_2020       projected                  0.658 
#>  4 cement  2014 advanced econ… demo_2020       projected                  0.0219
#>  5 cement  2014 developing as… demo_2020       projected                  0.0604
#>  6 cement  2014 global         demo_2020       projected                  0.659 
#>  7 cement  2015 advanced econ… demo_2020       projected                  0.0221
#>  8 cement  2015 developing as… demo_2020       projected                  0.0603
#>  9 cement  2015 global         demo_2020       projected                  0.660 
#> 10 cement  2016 advanced econ… demo_2020       projected                  0.0223
#> # … with 156 more rows
```

-   Use `target_market_share` to calculate market-share scenario targets
    at the portfolio level:

``` r
matched %>%
  target_market_share(
    abcd = abcd_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo
  )
#> Warning: Column `name_ald` is deprecated as of r2dii.match 0.1.0, please use
#> `name_abcd` instead.
#> Warning: Column `sector_ald` is deprecated as of r2dii.analysis 0.2.0, please use
#> `sector_abcd` instead.
#> # A tibble: 1,790 × 10
#>    sector     technology  year region scenario_source metric     production
#>    <chr>      <chr>      <int> <chr>  <chr>           <chr>           <dbl>
#>  1 automotive electric    2020 global demo_2020       projected     324592.
#>  2 automotive electric    2020 global demo_2020       target_cps    324592.
#>  3 automotive electric    2020 global demo_2020       target_sds    324592.
#>  4 automotive electric    2020 global demo_2020       target_sps    324592.
#>  5 automotive electric    2021 global demo_2020       projected     339656.
#>  6 automotive electric    2021 global demo_2020       target_cps    329191.
#>  7 automotive electric    2021 global demo_2020       target_sds    352505.
#>  8 automotive electric    2021 global demo_2020       target_sps    330435.
#>  9 automotive electric    2022 global demo_2020       projected     354720.
#> 10 automotive electric    2022 global demo_2020       target_cps    333693.
#> # … with 1,780 more rows, and 3 more variables: technology_share <dbl>,
#> #   scope <chr>, percentage_of_initial_production_by_scope <dbl>
```

-   Or at the company level:

``` r
matched %>%
  target_market_share(
    abcd = abcd_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo,
    by_company = TRUE
  )
#> Warning: Column `name_ald` is deprecated as of r2dii.match 0.1.0, please use
#> `name_abcd` instead.
#> Warning: Column `sector_ald` is deprecated as of r2dii.analysis 0.2.0, please use
#> `sector_abcd` instead.
#> Warning: You've supplied `by_company = TRUE` and `weight_production = TRUE`.
#> This will result in company-level results, weighted by the portfolio
#> loan size, which is rarely useful. Did you mean to set one of these
#> arguments to `FALSE`?
#> # A tibble: 32,402 × 11
#>    sector    technology  year region scenario_source name_abcd metric production
#>    <chr>     <chr>      <int> <chr>  <chr>           <chr>     <chr>       <dbl>
#>  1 automoti… electric    2020 global demo_2020       toyota m… proje…    324592.
#>  2 automoti… electric    2020 global demo_2020       toyota m… targe…    324592.
#>  3 automoti… electric    2020 global demo_2020       toyota m… targe…    324592.
#>  4 automoti… electric    2020 global demo_2020       toyota m… targe…    324592.
#>  5 automoti… electric    2021 global demo_2020       toyota m… proje…    339656.
#>  6 automoti… electric    2021 global demo_2020       toyota m… targe…    329191.
#>  7 automoti… electric    2021 global demo_2020       toyota m… targe…    352505.
#>  8 automoti… electric    2021 global demo_2020       toyota m… targe…    330435.
#>  9 automoti… electric    2022 global demo_2020       toyota m… proje…    354720.
#> 10 automoti… electric    2022 global demo_2020       toyota m… targe…    333693.
#> # … with 32,392 more rows, and 3 more variables: technology_share <dbl>,
#> #   scope <chr>, percentage_of_initial_production_by_scope <dbl>
```

### Utility Functions

The `target_*()` functions provide shortcuts for common operations. They
wrap some utility functions that you may also use directly:

-   Use `join_abcd_scenario()` to join a matched dataset to the relevant
    scenario data, and to pick assets in the relevant regions.

``` r
loanbook_joined_to_abcd_scenario <- matched %>%
  join_abcd_scenario(
    abcd = abcd_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo
  )
#> Warning: Column `name_ald` is deprecated as of r2dii.match 0.1.0, please use
#> `name_abcd` instead.
#> Warning: Column `sector_ald` is deprecated as of r2dii.analysis 0.2.0, please use
#> `sector_abcd` instead.
```

-   Use `summarize_weighted_production()` with different grouping
    arguments to calculate scenario-targets:

``` r
# portfolio level
loanbook_joined_to_abcd_scenario %>%
  summarize_weighted_production(scenario, tmsr, smsp, region)
#> # A tibble: 702 × 9
#>    sector_abcd technology  year scenario  tmsr    smsp region weighted_producti…
#>    <chr>       <chr>      <int> <chr>    <dbl>   <dbl> <chr>               <dbl>
#>  1 automotive  electric    2020 cps       1    0       global            324592.
#>  2 automotive  electric    2020 sds       1    0       global            324592.
#>  3 automotive  electric    2020 sps       1    0       global            324592.
#>  4 automotive  electric    2021 cps       1.12 0.00108 global            339656.
#>  5 automotive  electric    2021 sds       1.16 0.00653 global            339656.
#>  6 automotive  electric    2021 sps       1.14 0.00137 global            339656.
#>  7 automotive  electric    2022 cps       1.24 0.00213 global            354720.
#>  8 automotive  electric    2022 sds       1.32 0.0131  global            354720.
#>  9 automotive  electric    2022 sps       1.29 0.00273 global            354720.
#> 10 automotive  electric    2023 cps       1.35 0.00316 global            369784.
#> # … with 692 more rows, and 1 more variable: weighted_technology_share <dbl>

# company level
loanbook_joined_to_abcd_scenario %>%
  summarize_weighted_production(scenario, tmsr, smsp, region, name_abcd)
#> # A tibble: 9,036 × 10
#>    sector_abcd technology  year scenario  tmsr    smsp region name_abcd        
#>    <chr>       <chr>      <int> <chr>    <dbl>   <dbl> <chr>  <chr>            
#>  1 automotive  electric    2020 cps       1    0       global toyota motor corp
#>  2 automotive  electric    2020 sds       1    0       global toyota motor corp
#>  3 automotive  electric    2020 sps       1    0       global toyota motor corp
#>  4 automotive  electric    2021 cps       1.12 0.00108 global toyota motor corp
#>  5 automotive  electric    2021 sds       1.16 0.00653 global toyota motor corp
#>  6 automotive  electric    2021 sps       1.14 0.00137 global toyota motor corp
#>  7 automotive  electric    2022 cps       1.24 0.00213 global toyota motor corp
#>  8 automotive  electric    2022 sds       1.32 0.0131  global toyota motor corp
#>  9 automotive  electric    2022 sps       1.29 0.00273 global toyota motor corp
#> 10 automotive  electric    2023 cps       1.35 0.00316 global toyota motor corp
#> # … with 9,026 more rows, and 2 more variables: weighted_production <dbl>,
#> #   weighted_technology_share <dbl>
```

[Get
started](https://2degreesinvesting.github.io/r2dii.analysis/articles/r2dii-analysis.html).

## Funding

This project has received funding from the [European Union LIFE
program](https://wayback.archive-it.org/12090/20210412123959/https://ec.europa.eu/easme/en/)
and the [International Climate Initiative
(IKI)](https://www.international-climate-initiative.com/en/search-project/).
The Federal Ministry for the Environment, Nature Conservation and
Nuclear Safety (BMU) supports this initiative on the basis of a decision
adopted by the German Bundestag. The views expressed are the sole
responsibility of the authors and do not necessarily reflect the views
of the funders. The funders are not responsible for any use that may be
made of the information it contains.
