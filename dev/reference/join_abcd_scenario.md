# Join a data-loanbook object to the abcd and scenario

**\[deprecated\]**

This function was deprecated because it is not required as a user-facing
function for PACTA for Banks. It is still used internally though. All
relevant outputs of the PACTA for Banks analysis can be obtained using
the target_market_share() and target_sda() functions.

`join_abcd_scenario()` is a simple wrapper of several calls to
`dplyr::join_*()`, forming the master dataset to be used in later steps
of the analysis.

## Usage

``` r
join_abcd_scenario(
  data,
  abcd,
  scenario,
  region_isos = r2dii.data::region_isos,
  add_green_technologies = FALSE
)
```

## Arguments

- data:

  A data frame like the output of
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

- add_green_technologies:

  Logical vector of length 1. `FALSE` defaults to outputting only
  technologies that are present in both `data` and `abcd`. Set to
  `FALSE` to add rows of all possible green technologies (with 0
  production).

## Value

Returns a fully joined data frame, linking portfolio, abcd and scenario.

## See also

Other utility functions:
[`summarize_weighted_production()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/summarize_weighted_production.md)

## Examples

``` r
library(r2dii.data)
#> 
#> Attaching package: ‘r2dii.data’
#> The following object is masked from ‘package:r2dii.analysis’:
#> 
#>     data_dictionary
library(r2dii.match)
#> 
#> Attaching package: ‘r2dii.match’
#> The following object is masked from ‘package:r2dii.data’:
#> 
#>     data_dictionary
#> The following object is masked from ‘package:r2dii.analysis’:
#> 
#>     data_dictionary

valid_matches <- match_name(loanbook_demo, abcd_demo) %>%
  # WARNING: Remember to validate matches (see `?prioritize`)
  prioritize()

valid_matches %>%
  join_abcd_scenario(
    abcd = abcd_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo
  )
#> Warning: `join_abcd_scenario()` was deprecated in r2dii.analysis 0.5.0.
#> # A tibble: 14,592 × 37
#>    id_loan id_direct_loantaker name_direct_loantaker id_ultimate_parent
#>    <chr>   <chr>               <chr>                 <chr>             
#>  1 L6      C304                Kassulke-Kassulke     UP83              
#>  2 L6      C304                Kassulke-Kassulke     UP83              
#>  3 L6      C304                Kassulke-Kassulke     UP83              
#>  4 L6      C304                Kassulke-Kassulke     UP83              
#>  5 L6      C304                Kassulke-Kassulke     UP83              
#>  6 L6      C304                Kassulke-Kassulke     UP83              
#>  7 L6      C304                Kassulke-Kassulke     UP83              
#>  8 L6      C304                Kassulke-Kassulke     UP83              
#>  9 L6      C304                Kassulke-Kassulke     UP83              
#> 10 L6      C304                Kassulke-Kassulke     UP83              
#> # ℹ 14,582 more rows
#> # ℹ 33 more variables: name_ultimate_parent <chr>, loan_size_outstanding <dbl>,
#> #   loan_size_outstanding_currency <chr>, loan_size_credit_limit <dbl>,
#> #   loan_size_credit_limit_currency <chr>, sector_classification_system <chr>,
#> #   sector_classification_direct_loantaker <chr>, lei_direct_loantaker <chr>,
#> #   isin_direct_loantaker <chr>, id_2dii <chr>, level <chr>, sector <chr>,
#> #   sector_abcd <chr>, name <chr>, name_abcd <chr>, score <dbl>, …
```
