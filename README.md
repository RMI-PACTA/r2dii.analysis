
<!-- README.md is generated from README.Rmd. Please edit that file -->
# r2dii.analysis <a href='https://github.com/2DegreesInvesting/r2dii.analysis'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![CRAN status](https://www.r-pkg.org/badges/version/r2dii.analysis)](https://CRAN.R-project.org/package=r2dii.analysis) [![Codecov test coverage](https://codecov.io/gh/2degreesinvesting/r2dii.analysis/branch/master/graph/badge.svg)](https://codecov.io/gh/2degreesinvesting/r2dii.analysis?branch=master) [![R build status](https://github.com/2DegreesInvesting/r2dii.analysis/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.analysis/actions) <!-- badges: end -->

These tools help you to assess if a financial portfolio aligns with climate goals. They summarize key metrics attributed to the portfolio (e.g. production, emission factors), and calculate targets based on climate scenarios. They implement in R the last step of the free software 'PACTA' (Paris Agreement Capital Transition Assessment; <https://2degrees-investing.org/>). Financial institutions use 'PACTA' to study how their capital allocation impacts the climate.

## Installation

Before you install r2dii.analysis you may want to:

-   [Try an rstudio.cloud project with this package already installed](https://rstudio.cloud/project/1424833).
-   [Learn how to minimize installation errors](https://gist.github.com/maurolepore/a0187be9d40aee95a43f20a85f4caed6#installation).

When you are ready, install the released version of r2dii.analysis from CRAN with:

``` r
install.packages("r2dii.analysis")
```

Or install the development version of r2dii.analysis from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("2DegreesInvesting/r2dii.analysis")
```

[How to raise an issue?](https://2degreesinvesting.github.io/posts/2020-06-26-instructions-to-raise-an-issue/)

## Example

-   Use `library()` to attach the packages you need. r2dii.analysis does not depend on the packages r2dii.data and r2dii.match; but we suggest you install them -- with `install.packages(c("r2dii.data", "r2dii.match"))` -- so you can reproduce our examples.

``` r
library(r2dii.data)
library(r2dii.match)
library(r2dii.analysis)
```

-   Use `r2dii.match::match_name()` to identify matches between your loanbook and the asset level data.

``` r
matched <- match_name(loanbook_demo, ald_demo) %>%
  prioritize()
```

### Add Scenario Targets

-   Use `target_sda()` to calculate SDA targets of CO2 emissions.

``` r
matched %>%
  target_sda(
    ald = ald_demo,
    co2_intensity_scenario = co2_intensity_scenario_demo
  )
#> Warning: Removing ald rows where `emission_factor` is NA
#> <print(tibble::tibble())>
```

-   Use `target_market_share` to calculate market-share scenario targets at the portfolio level:

``` r
matched %>%
  target_market_share(
    ald = ald_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo
  )
#> <print(tibble::tibble())>
```

-   Or at the company level:

``` r
matched %>%
  target_market_share(
    ald = ald_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo,
    by_company = TRUE
  )
#> Warning: You've supplied `by_company = TRUE` and `weight_production = TRUE`.
#> This will result in company-level results, weighted by the portfolio
#> loan size, which is rarely useful. Did you mean to set one of these
#> arguments to `FALSE`?
#> <print(tibble::tibble())>
```

### Utility Functions

The `target_*()` functions provide shortcuts for common operations. They wrap some utility functions that you may also use directly:

-   Use `join_ald_scenario()` to join a matched dataset to the relevant scenario data, and to pick assets in the relevant regions.

``` r
loanbook_joined_to_ald_scenario <- matched %>%
  join_ald_scenario(
    ald = ald_demo,
    scenario = scenario_demo_2020,
    region_isos = region_isos_demo
  )
```

-   Use `summarize_weighted_production()` with different grouping arguments to calculate scenario-targets:

``` r
# portfolio level
loanbook_joined_to_ald_scenario %>%
  summarize_weighted_production(scenario, tmsr, smsp, region)
#> <print(tibble::tibble())>

# company level
loanbook_joined_to_ald_scenario %>%
  summarize_weighted_production(scenario, tmsr, smsp, region, name_ald)
#> <print(tibble::tibble())>
```

[Get started](https://2degreesinvesting.github.io/r2dii.analysis/articles/r2dii-analysis.html).

## Funding

This project has received funding from the [European Union LIFE program](https://ec.europa.eu/easme/en/life) and the [International Climate Initiative (IKI)](https://www.international-climate-initiative.com/en/details/project/measuring-paris-agreement-alignment-and-financial-risk-in-financial-markets-18_I_351-2982). The Federal Ministry for the Environment, Nature Conservation and Nuclear Safety (BMU) supports this initiative on the basis of a decision adopted by the German Bundestag. The views expressed are the sole responsibility of the authors and do not necessarily reflect the views of the funders. The funders are not responsible for any use that may be made of the information it contains.
