# Summaries based on the weight of each loan per sector per year

**\[deprecated\]**

These functions (summarize_weighted_production() and
summarize_weighted_percent_change()) were deprecated because they are
not required as a user-facing function for PACTA for Banks. They are
still used internally though. All relevant outputs of the PACTA for
Banks analysis can be obtained using the target_market_share() and
target_sda() functions.

Based on on the weight of each loan per sector per year,
`summarize_weighted_production()` and
`summarize_weighted_percent_change()` summarize the production and
percent-change, respectively.

## Usage

``` r
summarize_weighted_production(data, ..., use_credit_limit = FALSE)

summarize_weighted_percent_change(data, ..., use_credit_limit = FALSE)
```

## Arguments

- data:

  A data frame like the output of
  [`join_abcd_scenario()`](https://rmi-pacta.github.io/r2dii.analysis/reference/join_abcd_scenario.md).

- ...:

  Variables to group by.

- use_credit_limit:

  Logical vector of length 1. `FALSE` defaults to using the column
  `loan_size_outstanding`. Set to `TRUE` to instead use the column
  `loan_size_credit_limit`.

## Value

A tibble with the same groups as the input (if any) and columns:
`sector`, `technology`, and `year`; and `weighted_production` or
`weighted_production` for `summarize_weighted_production()` and
`summarize_weighted_percent_change()`, respectively.

## Warning

The percent-change analysis excludes companies with 0 production.
percent-change is undefined for companies that have no initial
production; including such companies would cause percent-change
percentage to be infinite, which is wrong.

## See also

[`join_abcd_scenario()`](https://rmi-pacta.github.io/r2dii.analysis/reference/join_abcd_scenario.md).

Other utility functions:
[`join_abcd_scenario()`](https://rmi-pacta.github.io/r2dii.analysis/reference/join_abcd_scenario.md)

## Examples

``` r
library(r2dii.data)
library(r2dii.match)

loanbook <- head(loanbook_demo, 150)
abcd <- head(abcd_demo, 100)
master <- loanbook %>%
  match_name(abcd) %>%
  prioritize() %>%
  join_abcd_scenario(
    abcd = abcd,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo
    ) %>%
  dplyr::filter(production != 0)

summarize_weighted_production(master)
#> Warning: `summarize_weighted_production()` was deprecated in r2dii.analysis 0.5.0.
#> # A tibble: 12 × 5
#>    sector_abcd technology     year weighted_production weighted_technology_share
#>    <chr>       <chr>         <int>               <dbl>                     <dbl>
#>  1 power       hydrocap       2020              50971.                     0.421
#>  2 power       hydrocap       2021              50230.                     0.421
#>  3 power       hydrocap       2022              49490.                     0.421
#>  4 power       hydrocap       2023              48749.                     0.421
#>  5 power       hydrocap       2024              48009.                     0.421
#>  6 power       hydrocap       2025              47268.                     0.421
#>  7 power       renewablescap  2020              61070.                     3.35 
#>  8 power       renewablescap  2021              61103.                     3.35 
#>  9 power       renewablescap  2022              61137.                     3.35 
#> 10 power       renewablescap  2023              61170.                     3.35 
#> 11 power       renewablescap  2024              61204.                     3.35 
#> 12 power       renewablescap  2025              42533.                     2.85 

summarize_weighted_production(master, use_credit_limit = TRUE)
#> # A tibble: 12 × 5
#>    sector_abcd technology     year weighted_production weighted_technology_share
#>    <chr>       <chr>         <int>               <dbl>                     <dbl>
#>  1 power       hydrocap       2020              46073.                     0.381
#>  2 power       hydrocap       2021              45404.                     0.381
#>  3 power       hydrocap       2022              44734.                     0.381
#>  4 power       hydrocap       2023              44065.                     0.381
#>  5 power       hydrocap       2024              43396.                     0.381
#>  6 power       hydrocap       2025              42726.                     0.381
#>  7 power       renewablescap  2020              60695.                     3.37 
#>  8 power       renewablescap  2021              60416.                     3.37 
#>  9 power       renewablescap  2022              60138.                     3.37 
#> 10 power       renewablescap  2023              59860.                     3.37 
#> 11 power       renewablescap  2024              59582.                     3.37 
#> 12 power       renewablescap  2025              44619.                     2.98 

summarize_weighted_percent_change(master)
#> Warning: `summarize_weighted_percent_change()` was deprecated in r2dii.analysis 0.5.0.
#> # A tibble: 12 × 4
#>    sector_abcd technology     year weighted_percent_change
#>    <chr>       <chr>         <int>                   <dbl>
#>  1 power       hydrocap       2020                0       
#>  2 power       hydrocap       2021               -0.0873  
#>  3 power       hydrocap       2022               -0.175   
#>  4 power       hydrocap       2023               -0.262   
#>  5 power       hydrocap       2024               -0.349   
#>  6 power       hydrocap       2025               -0.436   
#>  7 power       renewablescap  2020                0       
#>  8 power       renewablescap  2021                0.000439
#>  9 power       renewablescap  2022                0.000877
#> 10 power       renewablescap  2023                0.00132 
#> 11 power       renewablescap  2024                0.00175 
#> 12 power       renewablescap  2025               -0.0858  

summarize_weighted_percent_change(master, use_credit_limit = TRUE)
#> # A tibble: 12 × 4
#>    sector_abcd technology     year weighted_percent_change
#>    <chr>       <chr>         <int>                   <dbl>
#>  1 power       hydrocap       2020                 0      
#>  2 power       hydrocap       2021                -0.0789 
#>  3 power       hydrocap       2022                -0.158  
#>  4 power       hydrocap       2023                -0.237  
#>  5 power       hydrocap       2024                -0.316  
#>  6 power       hydrocap       2025                -0.395  
#>  7 power       renewablescap  2020                 0      
#>  8 power       renewablescap  2021                -0.00364
#>  9 power       renewablescap  2022                -0.00729
#> 10 power       renewablescap  2023                -0.0109 
#> 11 power       renewablescap  2024                -0.0146 
#> 12 power       renewablescap  2025                -0.0898 
```
