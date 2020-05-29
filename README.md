
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.analysis <a href='https://github.com/2DegreesInvesting/r2dii.analysis'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.analysis)](https://CRAN.R-project.org/package=r2dii.analysis)
[![Codecov test
coverage](https://codecov.io/gh/2degreesinvesting/r2dii.analysis/branch/master/graph/badge.svg)](https://codecov.io/gh/2degreesinvesting/r2dii.analysis?branch=master)
[![R build
status](https://github.com/2DegreesInvesting/r2dii.analysis/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.analysis/actions)
[![R build
status](https://github.com/2degreesinvesting/r2dii.analysis/workflows/R-CMD-check/badge.svg)](https://github.com/2degreesinvesting/r2dii.analysis/actions)
<!-- badges: end -->

Warning:

> An experimental package is in the very early stages of development.
> The API will be changing frequently as we rapidly iterate and explore
> variations in search of the best fit. Experimental packages will make
> API breaking changes without deprecation, so you are generally best
> off waiting until the package is more mature before you use it.

– <https://www.tidyverse.org/lifecycle/#experimental>

These tools implement in R a fundamental part of the software PACTA
(Paris Agreement Capital Transition Assessment), which is a free tool
that calculates the alignment between financial portfolios and climate
scenarios (<https://2degrees-investing.org/>). Financial institutions
use PACTA to study how their capital allocation impacts the climate.
This package provides a suite of metrics and analysis tools commonly
used for climate scenario analysis. For more information visit
<https://2degrees-investing.org/>.

## Installation

Install the development version of r2dii.analysis with:

``` r
# install.packages("devtools")

devtools::install_github("2DegreesInvesting/r2dii.analysis")
```

## Example

  - Use `library()` to attach the packages you need.

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

  - Use `target_sda()` to calculate SDA targets of CO2 emissions.

<!-- end list -->

``` r
target_sda(matched, ald_demo, co2_intensity_scenario_demo)
#> # A tibble: 28 x 4
#> # Groups:   sector [1]
#>    sector  year emission_factor_name               emission_factor_value
#>    <chr>  <dbl> <chr>                                              <dbl>
#>  1 cement  2020 portfolio_weighted_emission_factor                 0.664
#>  2 cement  2020 portfolio_target_emission_factor                   0.669
#>  3 cement  2020 scenario_emission_factor                           0.7  
#>  4 cement  2021 portfolio_weighted_emission_factor                 0.665
#>  5 cement  2021 portfolio_target_emission_factor                   0.612
#>  6 cement  2021 scenario_emission_factor                           0.64 
#>  7 cement  2022 portfolio_weighted_emission_factor                 0.666
#>  8 cement  2022 portfolio_target_emission_factor                   0.555
#>  9 cement  2022 scenario_emission_factor                           0.580
#> 10 cement  2023 portfolio_weighted_emission_factor                 0.667
#> # … with 18 more rows
```

  - Use `join_ald_scenario()` to join the matched dataset to the
    relevant scenario data, and to pick assets in the relevant regions.

<!-- end list -->

``` r
loanbook_joined_to_ald_scenario <- matched %>% 
  join_ald_scenario(
    ald = ald_demo, 
    scenario = scenario_demo_2020, 
    region_isos = region_isos_demo
  )
```

  - Use `summarize_company_production()` then
    `target_market_share_company()` to calculate scenario-targets for
    each company.

<!-- end list -->

``` r
loanbook_joined_to_ald_scenario %>% 
  summarize_company_production() %>% 
  target_market_share_company()
#> # A tibble: 28,332 x 7
#>    sector  technology  year name_ald  scenario production_name  production_value
#>    <chr>   <chr>      <int> <chr>     <chr>    <chr>                       <dbl>
#>  1 automo… electric    2020 shanghai… cps      weighted_produc…            5140.
#>  2 automo… electric    2020 shanghai… cps      tmsr_target_wei…            5140.
#>  3 automo… electric    2020 shanghai… cps      smsp_target_wei…            5140.
#>  4 automo… electric    2020 shanghai… sds      weighted_produc…            5140.
#>  5 automo… electric    2020 shanghai… sds      tmsr_target_wei…            5140.
#>  6 automo… electric    2020 shanghai… sds      smsp_target_wei…            5140.
#>  7 automo… electric    2020 shanghai… sps      weighted_produc…            5140.
#>  8 automo… electric    2020 shanghai… sps      tmsr_target_wei…            5140.
#>  9 automo… electric    2020 shanghai… sps      smsp_target_wei…            5140.
#> 10 automo… electric    2020 sichuan … cps      weighted_produc…            5985.
#> # … with 28,322 more rows
```

  - Use `summarize_portfolio_production()` then
    `target_market_share_portfolio()` to calculate scenario-targets for
    the whole portfolio:

<!-- end list -->

``` r
loanbook_joined_to_ald_scenario %>% 
  summarize_portfolio_production() %>% 
  target_market_share_portfolio()
#> # A tibble: 2,052 x 6
#>    sector    technology  year scenario production_name          production_value
#>    <chr>     <chr>      <int> <chr>    <chr>                               <dbl>
#>  1 automoti… electric    2020 cps      weighted_production               148935.
#>  2 automoti… electric    2020 cps      tmsr_target_weighted_pr…          148935.
#>  3 automoti… electric    2020 cps      smsp_target_weighted_pr…          148935.
#>  4 automoti… electric    2020 sds      weighted_production               148935.
#>  5 automoti… electric    2020 sds      tmsr_target_weighted_pr…          148935.
#>  6 automoti… electric    2020 sds      smsp_target_weighted_pr…          148935.
#>  7 automoti… electric    2020 sps      weighted_production               148935.
#>  8 automoti… electric    2020 sps      tmsr_target_weighted_pr…          148935.
#>  9 automoti… electric    2020 sps      smsp_target_weighted_pr…          148935.
#> 10 automoti… electric    2021 cps      weighted_production               150875.
#> # … with 2,042 more rows
```
