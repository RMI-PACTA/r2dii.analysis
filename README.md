
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
<https://www.transitionmonitor.com/>). Financial institutions use ‘PACTA’ to
study how their capital allocation impacts the climate.

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
#> # A tibble: 166 × 6
#>    sector  year region             scenario_source emission_factor_met…¹ emiss…²
#>    <chr>  <dbl> <chr>              <chr>           <chr>                   <dbl>
#>  1 cement  2013 advanced economies demo_2020       projected              0.0217
#>  2 cement  2013 developing asia    demo_2020       projected              0.0606
#>  3 cement  2013 global             demo_2020       projected              0.658 
#>  4 cement  2014 advanced economies demo_2020       projected              0.0219
#>  5 cement  2014 developing asia    demo_2020       projected              0.0604
#>  6 cement  2014 global             demo_2020       projected              0.659 
#>  7 cement  2015 advanced economies demo_2020       projected              0.0221
#>  8 cement  2015 developing asia    demo_2020       projected              0.0603
#>  9 cement  2015 global             demo_2020       projected              0.660 
#> 10 cement  2016 advanced economies demo_2020       projected              0.0223
#> # … with 156 more rows, and abbreviated variable names ¹​emission_factor_metric,
#> #   ²​emission_factor_value
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
#> # A tibble: 1,790 × 10
#>    sector     techno…¹  year region scena…² metric produ…³ techn…⁴ scope perce…⁵
#>    <chr>      <chr>    <int> <chr>  <chr>   <chr>    <dbl>   <dbl> <chr>   <dbl>
#>  1 automotive electric  2020 global demo_2… proje… 324592.  0.0759 sect… 0      
#>  2 automotive electric  2020 global demo_2… targe… 324592.  0.0759 sect… 0      
#>  3 automotive electric  2020 global demo_2… targe… 324592.  0.0759 sect… 0      
#>  4 automotive electric  2020 global demo_2… targe… 324592.  0.0759 sect… 0      
#>  5 automotive electric  2021 global demo_2… proje… 339656.  0.0786 sect… 0.00352
#>  6 automotive electric  2021 global demo_2… targe… 329191.  0.0744 sect… 0.00108
#>  7 automotive electric  2021 global demo_2… targe… 352505.  0.0809 sect… 0.00653
#>  8 automotive electric  2021 global demo_2… targe… 330435.  0.0747 sect… 0.00137
#>  9 automotive electric  2022 global demo_2… proje… 354720.  0.0813 sect… 0.00705
#> 10 automotive electric  2022 global demo_2… targe… 333693.  0.0730 sect… 0.00213
#> # … with 1,780 more rows, and abbreviated variable names ¹​technology,
#> #   ²​scenario_source, ³​production, ⁴​technology_share,
#> #   ⁵​percentage_of_initial_production_by_scope
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
#> # A tibble: 32,402 × 11
#>    sector     techno…¹  year region scena…² name_…³ metric produ…⁴ techn…⁵ scope
#>    <chr>      <chr>    <int> <chr>  <chr>   <chr>   <chr>    <dbl>   <dbl> <chr>
#>  1 automotive electric  2020 global demo_2… toyota… proje… 324592.  0.0759 sect…
#>  2 automotive electric  2020 global demo_2… toyota… targe… 324592.  0.0759 sect…
#>  3 automotive electric  2020 global demo_2… toyota… targe… 324592.  0.0759 sect…
#>  4 automotive electric  2020 global demo_2… toyota… targe… 324592.  0.0759 sect…
#>  5 automotive electric  2021 global demo_2… toyota… proje… 339656.  0.0786 sect…
#>  6 automotive electric  2021 global demo_2… toyota… targe… 329191.  0.0744 sect…
#>  7 automotive electric  2021 global demo_2… toyota… targe… 352505.  0.0809 sect…
#>  8 automotive electric  2021 global demo_2… toyota… targe… 330435.  0.0747 sect…
#>  9 automotive electric  2022 global demo_2… toyota… proje… 354720.  0.0813 sect…
#> 10 automotive electric  2022 global demo_2… toyota… targe… 333693.  0.0730 sect…
#> # … with 32,392 more rows, 1 more variable:
#> #   percentage_of_initial_production_by_scope <dbl>, and abbreviated variable
#> #   names ¹​technology, ²​scenario_source, ³​name_abcd, ⁴​production,
#> #   ⁵​technology_share
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
#> # A tibble: 702 × 9
#>    sector_abcd technology  year scenario  tmsr    smsp region weighted…¹ weigh…²
#>    <chr>       <chr>      <int> <chr>    <dbl>   <dbl> <chr>       <dbl>   <dbl>
#>  1 automotive  electric    2020 cps       1    0       global    324592.  0.0380
#>  2 automotive  electric    2020 sds       1    0       global    324592.  0.0380
#>  3 automotive  electric    2020 sps       1    0       global    324592.  0.0380
#>  4 automotive  electric    2021 cps       1.12 0.00108 global    339656.  0.0393
#>  5 automotive  electric    2021 sds       1.16 0.00653 global    339656.  0.0393
#>  6 automotive  electric    2021 sps       1.14 0.00137 global    339656.  0.0393
#>  7 automotive  electric    2022 cps       1.24 0.00213 global    354720.  0.0406
#>  8 automotive  electric    2022 sds       1.32 0.0131  global    354720.  0.0406
#>  9 automotive  electric    2022 sps       1.29 0.00273 global    354720.  0.0406
#> 10 automotive  electric    2023 cps       1.35 0.00316 global    369784.  0.0419
#> # … with 692 more rows, and abbreviated variable names ¹​weighted_production,
#> #   ²​weighted_technology_share

# company level
loanbook_joined_to_abcd_scenario %>%
  summarize_weighted_production(scenario, tmsr, smsp, region, name_abcd)
#> # A tibble: 9,036 × 10
#>    sector_a…¹ techn…²  year scena…³  tmsr    smsp region name_…⁴ weigh…⁵ weigh…⁶
#>    <chr>      <chr>   <int> <chr>   <dbl>   <dbl> <chr>  <chr>     <dbl>   <dbl>
#>  1 automotive electr…  2020 cps      1    0       global toyota… 324592.  0.0380
#>  2 automotive electr…  2020 sds      1    0       global toyota… 324592.  0.0380
#>  3 automotive electr…  2020 sps      1    0       global toyota… 324592.  0.0380
#>  4 automotive electr…  2021 cps      1.12 0.00108 global toyota… 339656.  0.0393
#>  5 automotive electr…  2021 sds      1.16 0.00653 global toyota… 339656.  0.0393
#>  6 automotive electr…  2021 sps      1.14 0.00137 global toyota… 339656.  0.0393
#>  7 automotive electr…  2022 cps      1.24 0.00213 global toyota… 354720.  0.0406
#>  8 automotive electr…  2022 sds      1.32 0.0131  global toyota… 354720.  0.0406
#>  9 automotive electr…  2022 sps      1.29 0.00273 global toyota… 354720.  0.0406
#> 10 automotive electr…  2023 cps      1.35 0.00316 global toyota… 369784.  0.0419
#> # … with 9,026 more rows, and abbreviated variable names ¹​sector_abcd,
#> #   ²​technology, ³​scenario, ⁴​name_abcd, ⁵​weighted_production,
#> #   ⁶​weighted_technology_share
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
