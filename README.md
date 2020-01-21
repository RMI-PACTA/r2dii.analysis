
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/3jITMq8.png" align="right" height=40 /> TODO Add description

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.analysis)](https://CRAN.R-project.org/package=r2dii.analysis)
<!-- badges: end -->

The goal of r2dii.analysis is to TODO …

## Installation

Install the development version of r2dii.analysis with something like
this:

``` r
# install.packages("devtools")

# To install from a private repo, see ?usethis::browse_github_token()
devtools::install_github("2DegreesInvesting/r2dii.analysis", auth_token = "abc")
```

## Example

We start by using the r2dii.analysis package. We also use dplyr for
general purpose manipulation.

``` r
library(r2dii.analysis)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

We also use a number of sample datasets:

  - `r2dii.analysis::sample_audit` contains data of TODO …
  - `r2dii.analysis::sample_results` contains data of TODO …
  - `r2dii.analysis::tech_sector_weighting` contains data of TODO …
  - `r2dii.analysis::scenario_relationships` (not used here) contains
    data of TODO …

<!-- end list -->

``` r
glimpse(r2dii.analysis::sample_audit)
#> Observations: 314,039
#> Variables: 5
#> $ Investor.Name  <chr> "Investor 1", "Investor 1", "Investor 1", "Investor 1"…
#> $ Portfolio.Name <chr> "Portfolio 1", "Portfolio 1", "Portfolio 1", "Portfoli…
#> $ ValueUSD       <dbl> 2110841.7, 1531695.0, 1165848.6, 1144782.1, 1050711.0,…
#> $ mapped_sector  <chr> "Other", "Other", "Other", "Other", "Other", "Other", …
#> $ Asset.Type     <chr> "Bonds", "Bonds", "Bonds", "Bonds", "Bonds", "Bonds", …

glimpse(r2dii.analysis::sample_results)
#> Observations: 714
#> Variables: 13
#> $ Investor.Name         <chr> "Investor 1", "Investor 1", "Investor 1", "Inve…
#> $ Portfolio.Name        <chr> "Portfolio 1", "Portfolio 1", "Portfolio 1", "P…
#> $ Scenario              <chr> "B2DS", "B2DS", "B2DS", "B2DS", "B2DS", "B2DS",…
#> $ Allocation            <chr> "PortfolioWeight", "PortfolioWeight", "Portfoli…
#> $ ScenarioGeography     <chr> "Global", "Global", "Global", "Global", "Global…
#> $ Sector                <chr> "Automotive", "Automotive", "Automotive", "Coal…
#> $ Technology            <chr> "Electric", "Hybrid", "ICE", "Coal", "Gas", "Oi…
#> $ Asset.Type            <chr> "Bonds", "Bonds", "Bonds", "Bonds", "Bonds", "B…
#> $ Year                  <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2020, 2020,…
#> $ Scen.Alloc.WtTechProd <dbl> 3327.769, 7810.344, 320085.700, 486471.229, 208…
#> $ Plan.Alloc.WtTechProd <dbl> 3327.769, 7810.344, 320085.700, 486471.229, 208…
#> $ Trajectory.Alignment  <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000,…
#> $ Trajectory.Deviation  <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000,…

glimpse(r2dii.analysis::tech_sector_weighting)
#> Observations: 33
#> Variables: 4
#> $ Sector            <chr> "Power", "Power", "Power", "Power", "Power", "Power…
#> $ Technology        <chr> "CoalCap", "GasCap", "HydroCap", "NuclearCap", "Oil…
#> $ sector_weight     <dbl> 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.09, 0.09, 0.0…
#> $ technology_weight <dbl> 0.25, 0.06, 0.09, 0.11, 0.02, 0.46, 0.44, 0.17, 0.3…

glimpse(r2dii.analysis::scenario_relationships)
#> Observations: 12
#> Variables: 4
#> $ Scenario <chr> "CPS", "NPSRTS", "SDS", "NPSRTS", "SDS", "B2DS", "CPS", "NPS…
#> $ Sector   <chr> "Power", "Power", "Power", "Automotive", "Automotive", "Auto…
#> $ relation <chr> "upper", "reference", "lower", "upper", "reference", "lower"…
#> $ temp     <dbl> 3.50, 2.75, 2.00, 2.75, 2.00, 1.75, 3.50, 2.75, 2.00, 3.50, …
```

  - `single_indicator()` helps you TODO …

<!-- end list -->

``` r
out_single_indicator <- single_indicator(
  input_results = r2dii.analysis::sample_results,
  upper_temp_threshold = 10,
  lower_temp_threshold = 1.5,
  start_year = 2019,
  time_horizon = 5,
  allocation = "PortfolioWeight",
  group_vars = c("Investor.Name", "Portfolio.Name", "Asset.Type")
)

out_single_indicator
#> # A tibble: 24 x 22
#>    Investor.Name Portfolio.Name Asset.Type Sector Allocation Technology
#>    <chr>         <chr>          <chr>      <chr>  <chr>      <chr>     
#>  1 Investor 1    Portfolio 1    Bonds      Autom… Portfolio… Electric  
#>  2 Investor 1    Portfolio 1    Bonds      Autom… Portfolio… Hybrid    
#>  3 Investor 1    Portfolio 1    Bonds      Autom… Portfolio… ICE       
#>  4 Investor 1    Portfolio 1    Bonds      Coal   Portfolio… Coal      
#>  5 Investor 1    Portfolio 1    Bonds      Oil&G… Portfolio… Gas       
#>  6 Investor 1    Portfolio 1    Bonds      Oil&G… Portfolio… Oil       
#>  7 Investor 1    Portfolio 1    Bonds      Power  Portfolio… CoalCap   
#>  8 Investor 1    Portfolio 1    Bonds      Power  Portfolio… GasCap    
#>  9 Investor 1    Portfolio 1    Bonds      Power  Portfolio… HydroCap  
#> 10 Investor 1    Portfolio 1    Bonds      Power  Portfolio… NuclearCap
#> # … with 14 more rows, and 16 more variables: temp_lower <dbl>,
#> #   temp_reference <dbl>, temp_upper <dbl>, scen_tech_prod_lower <dbl>,
#> #   scen_tech_prod_reference <dbl>, scen_tech_prod_upper <dbl>,
#> #   plan_tech_prod <dbl>, temp_lower_range <dbl>, temp_upper_range <dbl>,
#> #   scen_tech_prod_upper_bound <dbl>, scen_tech_prod_lower_bound <dbl>,
#> #   scen_plan_prod_diff <dbl>, factor <dbl>, temperature <dbl>,
#> #   ScenarioGeography <chr>, Scenario <chr>
```

  - `influencemap_weighting_methodology()` helps you TODO …

<!-- end list -->

``` r
out_influencemap_weighting_methodology <- influencemap_weighting_methodology(
  input_results = out_single_indicator,
  input_audit = r2dii.analysis::sample_audit,
  metric_name = "temperature",
  group_vars = c("Investor.Name", "Portfolio.Name"),
  sector_weightings = r2dii.analysis::tech_sector_weighting
) %>% 
  # ASK @vintented: Should this go into influencemap_weighting_methodology()?
  distinct(Investor.Name, Portfolio.Name, Allocation, temperature)

out_influencemap_weighting_methodology
#> # A tibble: 1 x 6
#> # Groups:   Investor.Name, Portfolio.Name, Allocation, ScenarioGeography,
#> #   Scenario [1]
#>   Investor.Name Portfolio.Name Allocation  temperature ScenarioGeograp… Scenario
#>   <chr>         <chr>          <chr>             <dbl> <chr>            <chr>   
#> 1 Investor 1    Portfolio 1    PortfolioW…        2.98 Global           Aggrega…
```

  - `mapped_sector_exposure()` helps you TODO …

<!-- end list -->

``` r
out_mapped_sector_exposure <- mapped_sector_exposure(
  input_audit = r2dii.analysis::sample_audit
)

out_mapped_sector_exposure
#> # A tibble: 1 x 3
#>   Investor.Name Portfolio.Name exposure_climate_sectors
#>   <chr>         <chr>                             <dbl>
#> 1 Investor 1    Portfolio 1                       0.135
```

We now join `out_influencemap_weighting_methodology` and
`out_mapped_sector_exposure` because TODO …

``` r
joint <- inner_join(
  out_influencemap_weighting_methodology, out_mapped_sector_exposure, 
  by = c("Investor.Name", "Portfolio.Name")
)
```

  - `find_range()` helps you TODO …

<!-- end list -->

``` r
find_range(joint, range = c(1.75, 2, 2.75, 3.5))
#> # A tibble: 1 x 8
#> # Groups:   Investor.Name, Portfolio.Name, Allocation, ScenarioGeography,
#> #   Scenario [1]
#>   Investor.Name Portfolio.Name Allocation temperature ScenarioGeograp… Scenario
#>   <chr>         <chr>          <chr>            <dbl> <chr>            <chr>   
#> 1 Investor 1    Portfolio 1    Portfolio…        2.98 Global           Aggrega…
#> # … with 2 more variables: exposure_climate_sectors <dbl>,
#> #   temperature_range <chr>
```
