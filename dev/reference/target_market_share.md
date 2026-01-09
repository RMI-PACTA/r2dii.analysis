# Add targets for production, using the market share approach

This function calculates the portfolio-level production targets, as
calculated using the market share approach applied to each relevant
climate production forecast.

## Usage

``` r
target_market_share(
  data,
  abcd,
  scenario,
  region_isos = r2dii.data::region_isos,
  use_credit_limit = FALSE,
  by_company = FALSE,
  weight_production = TRUE,
  increasing_or_decreasing = r2dii.data::increasing_or_decreasing
)
```

## Arguments

- data:

  A "data.frame" like the output of
  [`r2dii.match::prioritize`](https://rmi-pacta.github.io/r2dii.match/reference/prioritize.html).

- abcd:

  An asset level data frame like
  [r2dii.data::abcd_demo](https://rmi-pacta.github.io/r2dii.data/reference/abcd_demo.html).

- scenario:

  A scenario data frame like
  [r2dii.data::scenario_demo_2020](https://rmi-pacta.github.io/r2dii.data/reference/scenario_demo_2020.html).

- region_isos:

  A data frame like
  [r2dii.data::region_isos](https://rmi-pacta.github.io/r2dii.data/reference/region_isos.html)
  (default).

- use_credit_limit:

  Logical vector of length 1. `FALSE` defaults to using the column
  `loan_size_outstanding`. Set to `TRUE` to use the column
  `loan_size_credit_limit` instead.

- by_company:

  Logical vector of length 1. `FALSE` defaults to outputting
  `production_value` at the portfolio-level. Set to `TRUE` to output
  `production_value` at the company-level.

- weight_production:

  Logical vector of length 1. `TRUE` defaults to outputting production,
  weighted by relative loan-size. Set to `FALSE` to output the
  unweighted production values.

- increasing_or_decreasing:

  A data frame like
  [r2dii.data::increasing_or_decreasing](https://rmi-pacta.github.io/r2dii.data/reference/increasing_or_decreasing.html).

## Value

A tibble including the summarized columns `metric`, `production`,
`technology_share`, `percentage_of_initial_production_by_scope` and
`scope`. If `by_company = TRUE`, the output will also have the column
`name_abcd`.

## Handling grouped data

This function ignores existing groups and outputs ungrouped data.

## See also

Other functions to calculate scenario targets:
[`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)

## Examples

``` r
library(r2dii.data)
library(r2dii.match)

loanbook <- head(loanbook_demo, 100)
abcd <- head(abcd_demo, 100)

matched <- loanbook %>%
  match_name(abcd) %>%
  prioritize()

# Calculate targets at portfolio level
matched %>%
  target_market_share(
    abcd = abcd,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo
    )
#> # A tibble: 441 × 10
#>    sector technology  year region scenario_source metric     production
#>    <chr>  <chr>      <int> <chr>  <chr>           <chr>           <dbl>
#>  1 power  hydrocap    2020 europe demo_2020       projected         0  
#>  2 power  hydrocap    2020 europe demo_2020       target_cps        0  
#>  3 power  hydrocap    2020 europe demo_2020       target_sds        0  
#>  4 power  hydrocap    2020 europe demo_2020       target_sps        0  
#>  5 power  hydrocap    2020 global demo_2020       projected     16990. 
#>  6 power  hydrocap    2020 global demo_2020       target_cps    16990. 
#>  7 power  hydrocap    2020 global demo_2020       target_sds    16990. 
#>  8 power  hydrocap    2020 global demo_2020       target_sps    16990. 
#>  9 power  hydrocap    2021 europe demo_2020       projected         0  
#> 10 power  hydrocap    2021 europe demo_2020       target_cps       17.4
#> # ℹ 431 more rows
#> # ℹ 3 more variables: technology_share <dbl>, scope <chr>,
#> #   percentage_of_initial_production_by_scope <dbl>

# Calculate targets at company level
matched %>%
  target_market_share(
  abcd = abcd,
  scenario = scenario_demo_2020,
  region_isos = region_isos_demo,
  by_company = TRUE
  )
#> Warning: You've supplied `by_company = TRUE` and `weight_production = TRUE`.
#> This will result in company-level results, weighted by the portfolio
#> loan size, which is rarely useful. Did you mean to set one of these
#> arguments to `FALSE`?
#> # A tibble: 2,097 × 11
#>    sector technology  year region scenario_source name_abcd    metric production
#>    <chr>  <chr>      <int> <chr>  <chr>           <chr>        <chr>       <dbl>
#>  1 power  hydrocap    2020 europe demo_2020       Boyer, Boye… proje…          0
#>  2 power  hydrocap    2020 europe demo_2020       Boyer, Boye… targe…          0
#>  3 power  hydrocap    2020 europe demo_2020       Boyer, Boye… targe…          0
#>  4 power  hydrocap    2020 europe demo_2020       Boyer, Boye… targe…          0
#>  5 power  hydrocap    2020 europe demo_2020       Jahn         proje…          0
#>  6 power  hydrocap    2020 europe demo_2020       Jahn         targe…          0
#>  7 power  hydrocap    2020 europe demo_2020       Jahn         targe…          0
#>  8 power  hydrocap    2020 europe demo_2020       Jahn         targe…          0
#>  9 power  hydrocap    2020 global demo_2020       Boyer, Boye… proje…          0
#> 10 power  hydrocap    2020 global demo_2020       Boyer, Boye… targe…          0
#> # ℹ 2,087 more rows
#> # ℹ 3 more variables: technology_share <dbl>, scope <chr>,
#> #   percentage_of_initial_production_by_scope <dbl>

matched %>%
  target_market_share(
    abcd = abcd,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo,
    # Calculate unweighted targets
    weight_production = FALSE
    )
#> # A tibble: 441 × 10
#>    sector technology  year region scenario_source metric     production
#>    <chr>  <chr>      <int> <chr>  <chr>           <chr>           <dbl>
#>  1 power  hydrocap    2020 europe demo_2020       projected          0 
#>  2 power  hydrocap    2020 europe demo_2020       target_cps         0 
#>  3 power  hydrocap    2020 europe demo_2020       target_sds         0 
#>  4 power  hydrocap    2020 europe demo_2020       target_sps         0 
#>  5 power  hydrocap    2020 global demo_2020       projected     121032.
#>  6 power  hydrocap    2020 global demo_2020       target_cps    121032.
#>  7 power  hydrocap    2020 global demo_2020       target_sds    121032.
#>  8 power  hydrocap    2020 global demo_2020       target_sps    121032.
#>  9 power  hydrocap    2021 europe demo_2020       projected          0 
#> 10 power  hydrocap    2021 europe demo_2020       target_cps       136.
#> # ℹ 431 more rows
#> # ℹ 3 more variables: technology_share <dbl>, scope <chr>,
#> #   percentage_of_initial_production_by_scope <dbl>
```
