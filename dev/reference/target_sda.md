# Add targets for CO₂ emissions per unit production at the portfolio level, using the SDA approach

This function calculates targets of CO₂ emissions per unit production at
the portfolio-level, otherwise referred to as "emissions factors". It
uses the [sectoral-decarbonization approach
(SDA)](https://rmi-pacta.github.io/r2dii.analysis/articles/target-sda.html)
to calculate these targets.

## Usage

``` r
target_sda(
  data,
  abcd,
  co2_intensity_scenario,
  use_credit_limit = FALSE,
  by_company = FALSE,
  region_isos = r2dii.data::region_isos
)
```

## Arguments

- data:

  A dataframe like the output of
  [`r2dii.match::prioritize()`](https://rmi-pacta.github.io/r2dii.match/reference/prioritize.html).

- abcd:

  An asset-level data frame like
  [r2dii.data::abcd_demo](https://rmi-pacta.github.io/r2dii.data/reference/abcd_demo.html).

- co2_intensity_scenario:

  A scenario data frame like
  [r2dii.data::co2_intensity_scenario_demo](https://rmi-pacta.github.io/r2dii.data/reference/co2_intensity_scenario_demo.html).

- use_credit_limit:

  Logical vector of length 1. `FALSE` defaults to using the column
  `loan_size_outstanding`. Set to `TRUE` to instead use the column
  `loan_size_credit_limit`.

- by_company:

  Logical vector of length 1. `FALSE` defaults to outputting
  `weighted_production_value` at the portfolio-level. Set to `TRUE` to
  output `weighted_production_value` at the company-level.

- region_isos:

  A data frame like
  [r2dii.data::region_isos](https://rmi-pacta.github.io/r2dii.data/reference/region_isos.html)
  (default).

## Value

A tibble including the summarized columns `emission_factor_metric` and
`emission_factor_value`. If `by_company = TRUE`, the output will also
have the column `name_abcd`.

## Handling grouped data

This function ignores existing groups and outputs ungrouped data.

## See also

Other functions to calculate scenario targets:
[`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)

## Examples

``` r
library(r2dii.match)
library(r2dii.data)

loanbook <- head(loanbook_demo, 150)
abcd <- head(abcd_demo, 100)

matched <- loanbook %>%
  match_name(abcd) %>%
  prioritize()

# Calculate targets at portfolio level
matched %>%
  target_sda(
   abcd = abcd,
   co2_intensity_scenario = co2_intensity_scenario_demo,
   region_isos = region_isos_demo
   )
#> Warning: Removing rows in abcd where `emission_factor` is NA
#> Warning: Found no match between loanbook and abcd.
#> # A tibble: 0 × 4
#> # ℹ 4 variables: sector <chr>, year <int>, emission_factor_metric <chr>,
#> #   emission_factor_value <dbl>

# Calculate targets at company level
matched %>%
  target_sda(
   abcd = abcd,
   co2_intensity_scenario = co2_intensity_scenario_demo,
   region_isos = region_isos_demo,
   by_company = TRUE
   )
#> Warning: Removing rows in abcd where `emission_factor` is NA
#> Warning: Found no match between loanbook and abcd.
#> # A tibble: 0 × 4
#> # ℹ 4 variables: sector <chr>, year <int>, emission_factor_metric <chr>,
#> #   emission_factor_value <dbl>
```
