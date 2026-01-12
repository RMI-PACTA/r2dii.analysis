# Changelog

## r2dii.analysis (development version)

## r2dii.analysis 0.5.3

CRAN release: 2026-01-12

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now correctly adds missing low carbon technologies when only some
  companies are missing these technologies
  ([\#557](https://github.com/RMI-PACTA/r2dii.analysis/issues/557)).
- updated link to current SDA article in docs
  ([\#550](https://github.com/RMI-PACTA/r2dii.analysis/issues/550)).

## r2dii.analysis 0.5.2

CRAN release: 2025-06-17

## r2dii.analysis 0.5.1

CRAN release: 2025-02-18

- add definitions to `data_dictionary`
  ([\#532](https://github.com/RMI-PACTA/r2dii.analysis/issues/532))

## r2dii.analysis 0.5.0

CRAN release: 2025-02-13

### New features

- `data_dictionary` dataset added to define the columns in each dataset
  used or exported by the functions in this package
  ([\#521](https://github.com/RMI-PACTA/r2dii.analysis/issues/521)).

### Lifecycle changes

- `join_ald_scenario`, `summarize_weighted_production` and
  `summarize_weighted_percent_change` are now soft-deprecated
  ([\#526](https://github.com/RMI-PACTA/r2dii.analysis/issues/526)).
- r2dii.analysis is now
  [stable](https://lifecycle.r-lib.org/articles/stages.html)
  ([\#488](https://github.com/RMI-PACTA/r2dii.analysis/issues/488)).

### Other

- @jacobvjk is now the maintainer
  ([\#519](https://github.com/RMI-PACTA/r2dii.analysis/issues/519)).

## r2dii.analysis 0.4.0

CRAN release: 2024-03-26

- `target_market_share` now outputs `target_*` value for all `year`s in
  `scenario`
  ([\#481](https://github.com/RMI-PACTA/r2dii.analysis/issues/481)).
- Complete deprecation of `ald` in favour of `abcd`
  ([\#466](https://github.com/RMI-PACTA/r2dii.analysis/issues/466)).
- `target_market_share` now correctly handles input scenarios with a
  hyphen in their name
  ([\#425](https://github.com/RMI-PACTA/r2dii.analysis/issues/425)).
- `target_market_share` now handles `abcd` with rows where `production`
  is `NA` by filling with `0`
  ([\#423](https://github.com/RMI-PACTA/r2dii.analysis/issues/423)).

## r2dii.analysis 0.3.0

CRAN release: 2023-10-23

- `target_sda` now uses final year of scenario as convergence target
  when `by_company = TRUE`
  ([\#445](https://github.com/RMI-PACTA/r2dii.analysis/issues/445)).
- `target_market_share` gains argument `increasing_or_decreasing`
  ([\#426](https://github.com/RMI-PACTA/r2dii.analysis/issues/426)).

## r2dii.analysis 0.2.1

CRAN release: 2022-11-03

- `r2dii.analysis` has transferred to a new organization:
  <https://github.com/RMI-PACTA/>.

## r2dii.analysis 0.2.0

CRAN release: 2022-05-05

- New argument `abcd` of
  [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  and `target_sda` supersedes the argument `ald`
  ([\#404](https://github.com/RMI-PACTA/r2dii.analysis/issues/404)).

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  now only outputs data for `sector` values that are in all three input
  datasets (`data`, `ald` and `co2_intensity_scenario`)
  ([\#390](https://github.com/RMI-PACTA/r2dii.analysis/issues/390)).

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  now outputs unweighted `emission_factor` if `by_company` is `TRUE`
  ([\#376](https://github.com/RMI-PACTA/r2dii.analysis/issues/376)).

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  gains `region_isos` argument
  ([\#323](https://github.com/RMI-PACTA/r2dii.analysis/issues/323)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now only outputs values for years that are in both `ald` and
  `scenario` inputs
  ([\#394](https://github.com/RMI-PACTA/r2dii.analysis/issues/394)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now outputs two new columns,
  `percentage_of_initial_production_by_scope` and `scope` (ADO
  [\#4143](https://github.com/RMI-PACTA/r2dii.analysis/issues/4143)).

## r2dii.analysis 0.1.12

CRAN release: 2021-08-18

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now outputs 0 `technology_share`, for companies with 0 sectoral
  production
  ([\#306](https://github.com/RMI-PACTA/r2dii.analysis/issues/306)
  [@Antoine-Lalechere](https://github.com/Antoine-Lalechere)).

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  now filters `scenario` start year to be consistent with `ald` start
  year ([\#346](https://github.com/RMI-PACTA/r2dii.analysis/issues/346)
  [@waltjl](https://github.com/waltjl)).

## r2dii.analysis 0.1.10

CRAN release: 2021-07-09

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now sets all negative `smsp` targets to zero
  ([\#336](https://github.com/RMI-PACTA/r2dii.analysis/issues/336)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now only outputs `sector`s that are present in all input datasets
  ([\#329](https://github.com/RMI-PACTA/r2dii.analysis/issues/329)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now always adds targets for green technologies (defined by
  `r2dii.data::green_or_brown`), even when not present in input `data`
  ([\#318](https://github.com/RMI-PACTA/r2dii.analysis/issues/318)
  [@Antoine-Lalechere](https://github.com/Antoine-Lalechere)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now correctly groups by `region` when calculating `technology_share`
  ([\#315](https://github.com/RMI-PACTA/r2dii.analysis/issues/315)
  [@Antoine-Lalechere](https://github.com/Antoine-Lalechere)).

## r2dii.analysis 0.1.9

CRAN release: 2021-06-30

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  now only outputs `sector` values that are present in the input
  `co2_intensity_scenario` data
  ([\#308](https://github.com/RMI-PACTA/r2dii.analysis/issues/308)).

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  now outputs targets for the range of years in the input
  `co2_intenstiy_scenario`
  ([\#307](https://github.com/RMI-PACTA/r2dii.analysis/issues/307)).

## r2dii.analysis 0.1.8

CRAN release: 2021-05-22

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now correctly outputs target `technology share`, in line with
  methodology ([@georgeharris2deg](https://github.com/georgeharris2deg)
  [\#277](https://github.com/RMI-PACTA/r2dii.analysis/issues/277)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now correctly projects technology share as ‘production / total
  production’ when computing by company, unweighted by relative loan
  size ([@KapitanKombajn](https://github.com/KapitanKombajn)
  [\#288](https://github.com/RMI-PACTA/r2dii.analysis/issues/288)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  no longer outputs columns `sector_weighted_production` or
  `technology_weighted_production`. Those columns are internal so they
  shouldn’t face users
  ([\#291](https://github.com/RMI-PACTA/r2dii.analysis/issues/291)).

## r2dii.analysis 0.1.6

CRAN release: 2021-03-10

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now correctly outputs `technology_share` with multiple loans at
  different `level` to the same company
  ([@ab-bbva](https://github.com/ab-bbva)
  [\#265](https://github.com/RMI-PACTA/r2dii.analysis/issues/265)).

## r2dii.analysis 0.1.5

CRAN release: 2021-01-22

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now errors if input `data` has an unexpected column
  ([@georgeharris2deg](https://github.com/georgeharris2deg)
  [\#267](https://github.com/RMI-PACTA/r2dii.analysis/issues/267)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now correctly outputs `technology_share` with multiple loans to the
  same company ([@georgeharris2deg](https://github.com/georgeharris2deg)
  [\#262](https://github.com/RMI-PACTA/r2dii.analysis/issues/262),
  [@ab-bbva](https://github.com/ab-bbva)
  [\#265](https://github.com/RMI-PACTA/r2dii.analysis/issues/265)).

## r2dii.analysis 0.1.4

CRAN release: 2021-01-05

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now correctly outputs unweighted production by company, equal to
  ald-production for one company with multiple loans of different size
  ([\#255](https://github.com/RMI-PACTA/r2dii.analysis/issues/255)
  [@georgeharris2deg](https://github.com/georgeharris2deg)).

## r2dii.analysis 0.1.3

CRAN release: 2020-12-15

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now correctly outputs unweighted production when multiple levels exist
  for the same company
  ([\#249](https://github.com/RMI-PACTA/r2dii.analysis/issues/249)).

## r2dii.analysis 0.1.2

CRAN release: 2020-12-05

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now outputs `weighted_technology_share` that correctly sums to 1 when
  grouped by `sector`, `metric` and `scenario`
  ([\#218](https://github.com/RMI-PACTA/r2dii.analysis/issues/218)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now correctly outputs unweighted production when multiple loans exist
  for the same company
  ([\#239](https://github.com/RMI-PACTA/r2dii.analysis/issues/239)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now outputs empty named tibble if no matching region definitions can
  be found
  ([\#236](https://github.com/RMI-PACTA/r2dii.analysis/issues/236)).

- `target_market_share` now outputs all technologies present in `ald`,
  even if they are not present in `data`
  ([\#235](https://github.com/RMI-PACTA/r2dii.analysis/issues/235)).

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  now interpolates input scenario file by year and correctly calculates
  target, regardless of the time-horizon of `ald`
  ([\#234](https://github.com/RMI-PACTA/r2dii.analysis/issues/234)).

- Hyperlinks on the “Get Started” tab of the website now points to
  correct links
  ([\#222](https://github.com/RMI-PACTA/r2dii.analysis/issues/222)
  [@apmanning](https://github.com/apmanning)).

- Depend on dplyr \>= 0.8.5, explicitly. We commit to this version
  because the newer dplyr 1 is still relatively new, and represents a
  major change which some users initially resist.

- Relax dependency on rlang, as it is mostly driven dynamically by the
  by our recursive dependencies. For example, dplyr 0.8.5 depends on a
  specific version of rlang that is more recent than the version we
  explicitly depended on – which suggests that being explicit about
  rlang is unhelpful and misleading.

- New internal data `loanbook_stable` and `region_isos_stable` make
  regression tests more stable
  ([\#227](https://github.com/RMI-PACTA/r2dii.analysis/issues/227)).

## r2dii.analysis 0.1.1

CRAN release: 2020-09-12

- Change license to MIT.

- The website’s home page now thanks founders.

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now works as expected when some value of the column `scenario` is
  missing for some value of the column `region`. It no longer results in
  output columns `production` and `technology_share` of type “list”
  ([\#203](https://github.com/RMI-PACTA/r2dii.analysis/issues/203)).

- The website now shows the News tab.

## r2dii.analysis 0.1.0

CRAN release: 2020-09-03

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  now correctly handles differing `country_of_domicile` inputs
  ([\#171](https://github.com/RMI-PACTA/r2dii.analysis/issues/171)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now outputs `technology_share`
  ([\#184](https://github.com/RMI-PACTA/r2dii.analysis/issues/184)).

- `join_ald_scenario()` now returns visibly with dev-magrittr
  ([\#188](https://github.com/RMI-PACTA/r2dii.analysis/issues/188)
  [@lionel-](https://github.com/lionel-)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  gains `weight_production` parameter
  ([\#181](https://github.com/RMI-PACTA/r2dii.analysis/issues/181)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now correctly use `sector_ald` column from input `data` argument
  ([\#178](https://github.com/RMI-PACTA/r2dii.analysis/issues/178)).

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  now automatically filters out `ald` rows where the `emissions_factor`
  values are `NA`
  ([\#173](https://github.com/RMI-PACTA/r2dii.analysis/issues/173)).

- `join_ald_scenario()` now converts to lower case the values of the
  columns `sector_ald` and `technology`
  ([\#172](https://github.com/RMI-PACTA/r2dii.analysis/issues/172)).

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  now aggregates input `ald` by `technology` and `plant_location` prior
  to calculating targets
  ([@QianFeng2020](https://github.com/QianFeng2020)
  [\#160](https://github.com/RMI-PACTA/r2dii.analysis/issues/160)).

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  now errors if input data has any duplicated `id_loan`
  ([@QianFeng2020](https://github.com/QianFeng2020)
  [\#164](https://github.com/RMI-PACTA/r2dii.analysis/issues/164)).

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  gains `by_company` parameter
  ([\#155](https://github.com/RMI-PACTA/r2dii.analysis/issues/155)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  now outputs the actual aggregated corporate economy. Previously, the
  output would, erroneously, be normalized to the starting portfolio
  value
  ([\#158](https://github.com/RMI-PACTA/r2dii.analysis/issues/158)).

- [`target_sda()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_sda.md)
  now correctly calculates SDA targets
  ([\#153](https://github.com/RMI-PACTA/r2dii.analysis/issues/153)):
  Targets are now calculated using scenario data that is adjusted to
  corporate economy data. The adjusted scenario data is also output by
  the function along with the usual metrics. Methodology error fixed,
  and reflected in the code. Previously, the target was, incorrectly,
  calculated by multiplying the adjusted scenario. Now the scenario data
  is added instead.

- New
  [`summarize_weighted_percent_change()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/summarize_weighted_production.md)
  allows user to calculate a new indicator
  ([\#141](https://github.com/RMI-PACTA/r2dii.analysis/issues/141)).

- [`target_market_share()`](https://rmi-pacta.github.io/r2dii.analysis/dev/reference/target_market_share.md)
  no longer errors if the combination of `sector` and
  `scenario_target_value` does not uniquely identify an observation
  ([@georgeharris2deg](https://github.com/georgeharris2deg)
  [\#142](https://github.com/RMI-PACTA/r2dii.analysis/issues/142)).

## r2dii.analysis 0.0.1

CRAN release: 2020-06-28

- First release on CRAN
