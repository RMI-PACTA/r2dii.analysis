# Market Share Approach

The goal of this article is to define the *market-share-approach* for
allocating scenario efforts and to show how to calculate the
market-share target for a given scenario.

## Scenario market-shares

Say that you want to study how a portfolio would perform in a specific
climate scenario. How can you allocate scenario efforts to the
production profile of your portfolio? You can do that in two ways – by
technology, or by sector.

### 1. Market-share by technology

We define the market-share by technology as:

$$p_{i}^{tmsr}(t) = p_{i}\left( t_{0} \right) + p_{i}\left( t_{0} \right)*\frac{s_{i}(t) - s_{i}\left( t_{0} \right)}{s_{i}\left( t_{0} \right)}$$

We can see that this reduces to:

\$\$p\_{i}^{tmsr}(t) = p\_{i}(t\_{0}) \left(1 + \frac{s_i(t) -
s\_{i}(t_0)}{s_i(t_0)} \right) \\ p\_{i}^{tmsr}(t) = p\_{i}(t\_{0})
\left(1 + \frac{s_i(t)}{s_i(t_0)} -1 \right) \\ p\_{i}^{tmsr}(t) =
p\_{i}(t\_{0}) \* \frac{s_i(t)}{s_i(t_0)}\$\$

where:

- $s_{i}(t)$ is the scenario production for technology $i$ at time $t$,
- $p_{i}\left( t_{0} \right)$ is the production allocated to the
  portfolio for some technology, $i$ at time $t_{0}$, and
- $p_{i}^{tmsr}(t)$ is the portfolio-specific target production for that
  technology.

We define the “Technology Market Share Ratio” as:

$$\frac{s_{i}(t)}{s_{i}\left( t_{0} \right)}$$ This method is used to
set targets for “decreasing” (ie. brown) technologies.

### 2. Market-share by sector

To calculate the market-share by sector, we use the initial production
of both the portfolio and scenario at the sector-level instead.
$$p_{i}^{smsp}(t) = p_{i}\left( t_{0} \right) + P\left( t_{0} \right)*\left( \frac{s_{i}(t) - s_{i}\left( t_{0} \right)}{S\left( t_{0} \right)} \right)$$
where:

- $P_{i}\left( t_{0} \right)$ is the portfolio’s total production in the
  sector at $t_{0}$, and
- $S\left( t_{0} \right)$ is the scenario total production at $t_{0}$.

We define the “Sector Market Share Percentage” as:

$$\frac{s_{i}(t) - s_{i}\left( t_{0} \right)}{S\left( t_{0} \right)}$$
This method is used to calculate targets for “increasing” (ie. green)
technologies.

## How to calculate market-share targets for a given scenario

To calculate market-share targets, you need to use the package
r2dii.analysis and a number of datasets. One of those datasets is a
“matched” dataset (loanbook + asset-level data) that you can get with
the package [r2dii.match](https://rmi-pacta.github.io/r2dii.match/). The
datasets I use here come from the package
[r2dii.data](https://rmi-pacta.github.io/r2dii.data/); they are fake but
show how you should structure your own data.

- Use packages.

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
```

- Match the loanbook to asset level data.

``` r
loanbook <- r2dii.data::loanbook_demo
abcd <- r2dii.data::abcd_demo

matched <- match_name(loanbook, abcd) %>%
  # WARNING: Remember to validate the output of match_name() before prioritize()
  prioritize()

matched
#> # A tibble: 177 × 22
#>    id_loan id_direct_loantaker name_direct_loantaker          id_ultimate_parent
#>    <chr>   <chr>               <chr>                          <chr>             
#>  1 L6      C304                Kassulke-Kassulke              UP83              
#>  2 L13     C297                Ladeck                         UP69              
#>  3 L20     C287                Weinhold                       UP35              
#>  4 L21     C286                Gallo Group                    UP63              
#>  5 L22     C285                Austermuhle GmbH               UP187             
#>  6 L24     C282                Ferraro-Ferraro Group          UP209             
#>  7 L25     C281                Lockman, Lockman and Lockman   UP296             
#>  8 L26     C280                Ankunding, Ankunding and Anku… UP67              
#>  9 L27     C278                Donati-Donati Group            UP45              
#> 10 L28     C276                Ferraro, Ferraro e Ferraro SPA UP195             
#> # ℹ 167 more rows
#> # ℹ 18 more variables: name_ultimate_parent <chr>, loan_size_outstanding <dbl>,
#> #   loan_size_outstanding_currency <chr>, loan_size_credit_limit <dbl>,
#> #   loan_size_credit_limit_currency <chr>, sector_classification_system <chr>,
#> #   sector_classification_direct_loantaker <chr>, lei_direct_loantaker <chr>,
#> #   isin_direct_loantaker <chr>, id_2dii <chr>, level <chr>, sector <chr>,
#> #   sector_abcd <chr>, name <chr>, name_abcd <chr>, score <dbl>, …
```

- Calculate market-share targets for production at the portfolio level.

``` r
# portfolio level targets
scenario <- r2dii.data::scenario_demo_2020
regions <- r2dii.data::region_isos_demo


matched %>% target_market_share(abcd, scenario, regions)
#> # A tibble: 1,076 × 10
#>    sector     technology  year region scenario_source metric     production
#>    <chr>      <chr>      <int> <chr>  <chr>           <chr>           <dbl>
#>  1 automotive electric    2020 global demo_2020       projected     145649.
#>  2 automotive electric    2020 global demo_2020       target_cps    145649.
#>  3 automotive electric    2020 global demo_2020       target_sds    145649.
#>  4 automotive electric    2020 global demo_2020       target_sps    145649.
#>  5 automotive electric    2021 global demo_2020       projected     147480.
#>  6 automotive electric    2021 global demo_2020       target_cps    146915.
#>  7 automotive electric    2021 global demo_2020       target_sds    153332.
#>  8 automotive electric    2021 global demo_2020       target_sps    147258.
#>  9 automotive electric    2022 global demo_2020       projected     149310.
#> 10 automotive electric    2022 global demo_2020       target_cps    148155.
#> # ℹ 1,066 more rows
#> # ℹ 3 more variables: technology_share <dbl>, scope <chr>,
#> #   percentage_of_initial_production_by_scope <dbl>
```

- Calculate market-share targets for production at the company level.

``` r
matched %>% target_market_share(abcd, scenario, regions, by_company = TRUE)
#> Warning: You've supplied `by_company = TRUE` and `weight_production = TRUE`.
#> This will result in company-level results, weighted by the portfolio
#> loan size, which is rarely useful. Did you mean to set one of these
#> arguments to `FALSE`?
#> # A tibble: 14,505 × 11
#>    sector    technology  year region scenario_source name_abcd metric production
#>    <chr>     <chr>      <int> <chr>  <chr>           <chr>     <chr>       <dbl>
#>  1 automoti… electric    2020 global demo_2020       Bernardi… proje…     17951.
#>  2 automoti… electric    2020 global demo_2020       Bernardi… targe…     17951.
#>  3 automoti… electric    2020 global demo_2020       Bernardi… targe…     17951.
#>  4 automoti… electric    2020 global demo_2020       Bernardi… targe…     17951.
#>  5 automoti… electric    2020 global demo_2020       Christia… proje…     11471.
#>  6 automoti… electric    2020 global demo_2020       Christia… targe…     11471.
#>  7 automoti… electric    2020 global demo_2020       Christia… targe…     11471.
#>  8 automoti… electric    2020 global demo_2020       Christia… targe…     11471.
#>  9 automoti… electric    2020 global demo_2020       Donati, … proje…      5611.
#> 10 automoti… electric    2020 global demo_2020       Donati, … targe…      5611.
#> # ℹ 14,495 more rows
#> # ℹ 3 more variables: technology_share <dbl>, scope <chr>,
#> #   percentage_of_initial_production_by_scope <dbl>
```
