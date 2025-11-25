# Sectoral Decarbonization Approach (SDA)

## SDA Methodology

The Sectoral Decarbonization Approach (SDA) is a method for setting
corporate CO₂ emissions intensity reduction targets in line with climate
science. This method was developed by the Science-Based Targets
Initiative ([SBTI](https://sciencebasedtargets.org/)), an international
initiative on science-based target setting for companies initiated by
[CDP](https://www.cdp.net/en), the [United Nations Global
Compact](https://www.unglobalcompact.org/), the World Resources
Institute ([WRI](https://www.wri.org/)), and the Worldwide Fund for
Nature ([WWF](https://www.worldwildlife.org/)).

In the context of PACTA, this methodology is used to calculate emission
factor targets for homogenous sectors (i.e. sectors with no
technology-level scenario pathways).

First, the distance, $d$, between the company’s CO₂ emissions intensity
per unit production (or emissions factor), $I^{Co}(t)$ at some base
year, $t_{0}$, and a scenario target intensity in 2050, $I^{Sc}(2050)$
is calculated. The target intensity in 2050 can be taken from any
relevant climate scenario:

$$d = I^{Co}\left( t_{0} \right) - I^{Sc}(2050)$$

The company’s market share parameter, $m(t)$, is defined as the
company’s expected future activity, $P^{Co}(t)$ divided by the sector’s
future activity, $P^{Sc}(t)$ to reflect the expected forward-looking
market share of the company. This is given as a ratio to the company’s
base year market share, derived from its activity,
$P^{Co}\left( t_{0} \right)$ divided by the sector’s activity in the
same year, $P^{Sc}\left( t_{0} \right)$. In both cases the former is
calculated per company, and the latter is determined from the climate
scenario:

$$m(t) = \frac{P^{Co}\left( t_{0} \right)/P^{Sc}\left( t_{0} \right)}{P^{Co}(t)/P^{Sc}(t)}$$

It should be noted that this parameter does not capture the change in
the market share of the company but rather the inverse. This is useful
as it equates to a decreasing parameter when the company’s market share
is increasing. This equates to larger reduction efforts when the
companies market share is increasing over time.

The sector decarbonization factor, $p(t)$ is defined as:

$$p(t) = \frac{I^{Sc}(t) - I^{Sc}(2050)}{I^{Sc}\left( t_{0} \right) - I^{Sc}(2050)}$$

where $I^{in}(t)$ and $I^{Sc}(t)$ are the average market and scenario
emission intensities respectively, at time $t$.

This variable captures the remaining effort needed from the market to
meet the target in 2050, per year. Under the SDA assumptions the CO₂
intensity for all companies in a sector converge in 2050. Note that
$p\left( t_{0} \right) = 1$ and $p(2050) = 0$, indicating that 100% of
the expected decarbonization efforts are still to be met at the base
year and 0% should be left at 2050.

The company-level emission intensity target is then defined as:
$$I^{Target}(t) = \left( d*p(t)*m(t) \right) + I^{Sc}(2050)$$

## PACTA Assumptions

The SDA applied in PACTA differs slightly from the way it is applied by
the SBTI. In particular, we must align the top-down approach laid out by
climate scenarios with the bottom-up asset-based company data used in
the PACTA analysis.

**Assumption: Market share stays constant ($m(t)$ = 1)**

Due to the lack of quantitative data on the expected market share
changes throughout the entire time horizon up to 2050. $m(t)$ is set to
1 for all years. Under the SBTI method for calculating $m(t)$, there
will be a higher intensity reduction target in cases where the absolute
pathway of the sector exceeds the scenario target. This makes sense.
However, applying this at company level is counter-intuitive:

> Companies that decrease their market share would be allowed to have a
> higher CO₂-Intensity than the average market actor. While, companies
> that are increasing their market share are forced to do more in terms
> of CO₂-Intensity than ones whose market share remains constant. It
> follows that if a company reaches the targeted CO₂-Intensity it would
> not be allowed to increase its share in the market. This is a
> desirable outcome.

Under this assumption, our target calculation reduces to:

$$I^{Target}(t) = \left( d*p(t) \right) + I^{Sc}(2050)$$

**Approximation: Adjust base year scenario emission intensity**

In both the SBTI and the PACTA methodology the target emissions for the
sector are taken from climate scenarios. These implement a global
economy top-down approach which applies an absolute emissions value in
the year 2050 and then converts this to yearly emission intensities.
However, there may be discrepancies between the Scenario projected
emission intensities, and the bottom-up ABCD emission intensities. To
reflect this discrepancy, we adjust the scenario projections by the
following factor,

$$\frac{I^{ABCD}\left( t_{0} \right)}{I^{Sc}\left( t_{0} \right)}$$
yielding the adjusted scenario pathway:

$$I\prime^{Sc}(t) = \left( \frac{I^{ABCD}\left( t_{0} \right)}{I^{Sc}\left( t_{0} \right)} \right)*I^{Sc}(t)$$
This yields the final PACTA SDA target equation:

$$I^{Target}(t) = \left( d*p(t) \right) + I\prime^{Sc}(t)$$ Note: $d$
and $p(t)$ also must be re-calculated using this adjusted scenario
intensity, $I\prime^{Sc}$.

## Calculating SDA Targets

To calculate SDA targets you need to use the package r2dii.analysis and
a number of datasets, including a “matched” dataset (loanbook +
asset-level data) that you can get with the package
[r2dii.match](https://rmi-pacta.github.io/r2dii.match/). The datasets I
use here come from the package
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

- Calculate SDA targets for CO₂ emissions intensities:

``` r
co2_intensity <- r2dii.data::co2_intensity_scenario_demo

matched %>% target_sda(abcd, co2_intensity)
#> Warning: Removing rows in abcd where `emission_factor` is NA
#> Warning: Found no matching regions for input scenario
#> # A tibble: 0 × 4
#> # ℹ 4 variables: sector <chr>, year <int>, emission_factor_metric <chr>,
#> #   emission_factor_value <dbl>
```
