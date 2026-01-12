# Data Dictionary

A table of column names and descriptions of data frames used or exported
by the functions in this package.

## Usage

``` r
data_dictionary
```

## Format

### `data_dictionary`

- dataset:

  Name of the dataset

- column:

  Name of the column

- typeof:

  Type of the column

- definition:

  Definition of the column

## Examples

``` r
data_dictionary
#> # A tibble: 47 × 4
#>    dataset                   column                          typeof   definition
#>    <chr>                     <chr>                           <chr>    <chr>     
#>  1 join_abcd_scenario_output emission_factor                 double   Company l…
#>  2 join_abcd_scenario_output id_2dii                         charact… an id use…
#>  3 join_abcd_scenario_output id_loan                         charact… Unique lo…
#>  4 join_abcd_scenario_output is_ultimate_owner               logical  Flag if c…
#>  5 join_abcd_scenario_output level                           charact… the level…
#>  6 join_abcd_scenario_output loan_size_credit_limit          double   Total cre…
#>  7 join_abcd_scenario_output loan_size_credit_limit_currency charact… Currency …
#>  8 join_abcd_scenario_output loan_size_outstanding           double   Amount dr…
#>  9 join_abcd_scenario_output loan_size_outstanding_currency  charact… Currency …
#> 10 join_abcd_scenario_output name_abcd                       charact… the name …
#> # ℹ 37 more rows
```
