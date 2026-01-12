# Indicator Choices:

## Weighted Production

For most intents and purposes, we recommend calculating all targets
using the loan weighted production as an indicator. In particular, we
define the loan weighted production of a given company, $j$ as:
$${\overline{p}}_{i,j}(t) = p_{i,j}(t)*\frac{l_{j}}{\sum\limits_{j}l_{j}}$$
where $p_{i,j}$ is the production of company $i$ in technology $j$ and
$l_{j}$ is the loan given to company $j$.

To calculate portfolio targets, we aggregate this value by summing over
every company in the portfolio:
$${\overline{p}}_{i}(t) = \sum\limits_{j}\left\lbrack p_{i,j}(t)*\frac{l_{j}}{\sum\limits_{j}l_{j}} \right\rbrack$$

Effectively, this is a loan-weighted average of the production
attributed to each company in your portfolio. A significant result of
this indicator choice is that small companies (with little production)
will be favorably weighted, given that the loan to that company is
sufficiently large. This can be useful to reflect large investments into
green start-ups.

To calculate the weighted production:

``` r
library(r2dii.data)
library(r2dii.match)
#> 
#> Attaching package: 'r2dii.match'
#> The following object is masked from 'package:r2dii.data':
#> 
#>     data_dictionary
library(r2dii.analysis)
#> 
#> Attaching package: 'r2dii.analysis'
#> The following object is masked from 'package:r2dii.match':
#> 
#>     data_dictionary
#> The following object is masked from 'package:r2dii.data':
#> 
#>     data_dictionary

master <- loanbook_demo %>%
  match_name(abcd_demo) %>%
  prioritize() %>%
  join_abcd_scenario(
    abcd = abcd_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo,
    add_green_technologies = FALSE
  )
#> Warning: `join_abcd_scenario()` was deprecated in r2dii.analysis 0.5.0.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.

summarize_weighted_production(master)
#> Warning: `summarize_weighted_production()` was deprecated in r2dii.analysis 0.5.0.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> # A tibble: 168 × 5
#>    sector_abcd technology  year weighted_production weighted_technology_share
#>    <chr>       <chr>      <int>               <dbl>                     <dbl>
#>  1 automotive  electric    2020             436948.                     0.481
#>  2 automotive  electric    2021             442439.                     0.480
#>  3 automotive  electric    2022             447929.                     0.480
#>  4 automotive  electric    2023             453420.                     0.479
#>  5 automotive  electric    2024             458910.                     0.479
#>  6 automotive  electric    2025             464401.                     0.479
#>  7 automotive  electric    2026                 NA                     NA    
#>  8 automotive  electric    2027                 NA                     NA    
#>  9 automotive  electric    2028                 NA                     NA    
#> 10 automotive  electric    2029                 NA                     NA    
#> # ℹ 158 more rows
```

## Weighted Percent Change in Production

On the other-hand, if you’re more keen to understand if the large
corporations in your portfolio are planning to make any significant
changes, the percent change in production may be a more useful
indicator.

For each company, we define the percent change, $\chi_{i}(t)$, as
compared to the start year, $t_{0}$:

$$\chi_{i}(t) = \frac{p_{i}(t) - p_{i}\left( t_{0} \right)}{p_{i}\left( t_{0} \right)}*100$$
where $p_{i}(t)$ is the indicator (production or capacity) of technology
$i$, and $t0$ is the start year of the analysis.

We aggregate the percent-change in production for each company to the
portfolio-level, by using the same loan-weighted average as above. In
particular, for each loan $l_{j}$ to company $j$, we have:
$$\overline{\chi_{i}} = \sum\limits_{j}\left\lbrack \chi_{i,j}*\frac{l_{j}}{\sum\limits_{j}l_{j}} \right\rbrack$$

It should be noted that the percent change, $\chi$, is undefined for 0
initial production. Intuitively, this makes sense, since you would
require an “infinite percent” build-out to grow to anything from 0. For
this reason, any company having 0 initial production is filtered out
prior to calculating the percent change indicator.

To calculate the weighted percent change:

``` r
# using the master dataset defined in the previous chunk:
summarize_weighted_percent_change(master)
#> Warning: `summarize_weighted_percent_change()` was deprecated in r2dii.analysis 0.5.0.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> # A tibble: 168 × 4
#>    sector_abcd technology  year weighted_percent_change
#>    <chr>       <chr>      <int>                   <dbl>
#>  1 automotive  electric    2020               0        
#>  2 automotive  electric    2021               0.0000626
#>  3 automotive  electric    2022               0.000125 
#>  4 automotive  electric    2023               0.000188 
#>  5 automotive  electric    2024               0.000250 
#>  6 automotive  electric    2025               0.000313 
#>  7 automotive  electric    2026              NA        
#>  8 automotive  electric    2027              NA        
#>  9 automotive  electric    2028              NA        
#> 10 automotive  electric    2029              NA        
#> # ℹ 158 more rows
```
