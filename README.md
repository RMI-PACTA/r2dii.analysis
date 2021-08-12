
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.analysis <a href='https://github.com/2DegreesInvesting/r2dii.analysis'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.analysis)](https://CRAN.R-project.org/package=r2dii.analysis)
[![Codecov test
coverage](https://codecov.io/gh/2degreesinvesting/r2dii.analysis/branch/master/graph/badge.svg)](https://codecov.io/gh/2degreesinvesting/r2dii.analysis?branch=master)
[![R-CMD-check](https://github.com/2DegreesInvesting/r2dii.analysis/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.analysis/actions)
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
#> Warning: Removing ald rows where `emission_factor` is NA
#> # A tibble: 163 × 4
#>    sector  year emission_factor_metric emission_factor_value
#>    <chr>  <dbl> <chr>                                  <dbl>
#>  1 cement  2013 projected                              0.658
#>  2 cement  2014 projected                              0.659
#>  3 cement  2015 projected                              0.660
#>  4 cement  2016 projected                              0.661
#>  5 cement  2017 projected                              0.662
#>  6 cement  2018 projected                              0.662
#>  7 cement  2019 projected                              0.663
#>  8 cement  2020 projected                              0.664
#>  9 cement  2021 projected                              0.665
#> 10 cement  2022 projected                              0.666
#> # … with 153 more rows
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
#> # A tibble: 2,334 × 8
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
#> # … with 2,324 more rows, and 1 more variable: technology_share <dbl>
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
#> Warning: You've supplied `by_company = TRUE` and `weight_production = TRUE`.
#> This will result in company-level results, weighted by the portfolio
#> loan size, which is rarely useful. Did you mean to set one of these
#> arguments to `FALSE`?
#> # A tibble: 32,946 × 9
#>    sector     technology  year region scenario_source name_ald metric production
#>    <chr>      <chr>      <int> <chr>  <chr>           <chr>    <chr>       <dbl>
#>  1 automotive electric    2020 global demo_2020       toyota … proje…    324592.
#>  2 automotive electric    2020 global demo_2020       toyota … targe…    324592.
#>  3 automotive electric    2020 global demo_2020       toyota … targe…    324592.
#>  4 automotive electric    2020 global demo_2020       toyota … targe…    324592.
#>  5 automotive electric    2021 global demo_2020       toyota … proje…    339656.
#>  6 automotive electric    2021 global demo_2020       toyota … targe…    329191.
#>  7 automotive electric    2021 global demo_2020       toyota … targe…    352505.
#>  8 automotive electric    2021 global demo_2020       toyota … targe…    330435.
#>  9 automotive electric    2022 global demo_2020       toyota … proje…    354720.
#> 10 automotive electric    2022 global demo_2020       toyota … targe…    333693.
#> # … with 32,936 more rows, and 1 more variable: technology_share <dbl>
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
#> # A tibble: 702 × 9
#>    sector_ald technology  year scenario  tmsr    smsp region weighted_production
#>    <chr>      <chr>      <int> <chr>    <dbl>   <dbl> <chr>                <dbl>
#>  1 automotive electric    2020 cps       1    0       global             324592.
#>  2 automotive electric    2020 sds       1    0       global             324592.
#>  3 automotive electric    2020 sps       1    0       global             324592.
#>  4 automotive electric    2021 cps       1.12 0.00108 global             339656.
#>  5 automotive electric    2021 sds       1.16 0.00653 global             339656.
#>  6 automotive electric    2021 sps       1.14 0.00137 global             339656.
#>  7 automotive electric    2022 cps       1.24 0.00213 global             354720.
#>  8 automotive electric    2022 sds       1.32 0.0131  global             354720.
#>  9 automotive electric    2022 sps       1.29 0.00273 global             354720.
#> 10 automotive electric    2023 cps       1.35 0.00316 global             369784.
#> # … with 692 more rows, and 1 more variable: weighted_technology_share <dbl>

# company level
loanbook_joined_to_ald_scenario %>%
  summarize_weighted_production(scenario, tmsr, smsp, region, name_ald)
#> # A tibble: 9,036 × 10
#>    sector_ald technology  year scenario  tmsr    smsp region name_ald         
#>    <chr>      <chr>      <int> <chr>    <dbl>   <dbl> <chr>  <chr>            
#>  1 automotive electric    2020 cps       1    0       global toyota motor corp
#>  2 automotive electric    2020 sds       1    0       global toyota motor corp
#>  3 automotive electric    2020 sps       1    0       global toyota motor corp
#>  4 automotive electric    2021 cps       1.12 0.00108 global toyota motor corp
#>  5 automotive electric    2021 sds       1.16 0.00653 global toyota motor corp
#>  6 automotive electric    2021 sps       1.14 0.00137 global toyota motor corp
#>  7 automotive electric    2022 cps       1.24 0.00213 global toyota motor corp
#>  8 automotive electric    2022 sds       1.32 0.0131  global toyota motor corp
#>  9 automotive electric    2022 sps       1.29 0.00273 global toyota motor corp
#> 10 automotive electric    2023 cps       1.35 0.00316 global toyota motor corp
#> # … with 9,026 more rows, and 2 more variables: weighted_production <dbl>,
#> #   weighted_technology_share <dbl>
```

[Get
started](https://2degreesinvesting.github.io/r2dii.analysis/articles/r2dii-analysis.html).

## Funding

This project has received funding from the [European Union LIFE
program](https://wayback.archive-it.org/12090/20210412123959/https://ec.europa.eu/easme/en/)
and the [International Climate Initiative
(IKI)](https://www.international-climate-initiative.com/en/details/project/measuring-paris-agreement-alignment-and-financial-risk-in-financial-markets-18_I_351-2982).
The Federal Ministry for the Environment, Nature Conservation and
Nuclear Safety (BMU) supports this initiative on the basis of a decision
adopted by the German Bundestag. The views expressed are the sole
responsibility of the authors and do not necessarily reflect the views
of the funders. The funders are not responsible for any use that may be
made of the information it contains.
