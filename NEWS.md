# r2dii.analysis 0.1.5

* `target_market_share()` now errors if input `data` has an unexpected column
  (@georgeharris2deg #267).

* `target_market_share()` now correctly outputs `technology_share` with
  multiple loans to the same company (@georgeharris2deg #262, @ab-bbva #265).

# r2dii.analysis 0.1.4

* `target_market_share()` now correctly outputs unweighted production by
  company, equal to ald-production for one company with multiple loans of
  different size (#255 @georgeharris2deg).

# r2dii.analysis 0.1.3

* `target_market_share()` now correctly outputs unweighted production when 
  multiple levels exist for the same company (#249).

# r2dii.analysis 0.1.2

* `target_market_share()` now outputs `weighted_technology_share` that 
  correctly sums to 1 when grouped by `sector`, `metric` and `scenario` (#218).

* `target_market_share()` now correctly outputs unweighted production when 
  multiple loans exist for the same company (#239).

* `target_market_share()` now outputs empty named tibble if no matching region 
  definitions can be found (#236).
  
* `target_market_share` now outputs all technologies present in `ald`, even if 
  they are not present in `data` (#235).

* `target_sda()` now interpolates input scenario file by year and correctly 
  calculates target, regardless of the time-horizon of `ald` (#234).

* Hyperlinks on the "Get Started" tab of the website now points to correct links 
  (#222 @apmanning).

* Depend on dplyr >= 0.8.5, explicitly. We commit to this version because the
  newer dplyr 1 is still relatively new, and represents a major change which
  some users initially resist.

* Relax dependency on rlang, as it is mostly driven dynamically by the 
  by our recursive dependencies. For example, dplyr 0.8.5 depends on a specific
  version of rlang that is more recent than the version we explicitly 
  depended on -- which suggests that being explicit about rlang is unhelpful and
  misleading.

* New internal data `loanbook_stable` and `region_isos_stable` make regression
  tests more stable (#227).

# r2dii.analysis 0.1.1

* Change license to MIT.

* The website's home page now thanks founders.

* `target_market_share()` now works as expected when some value of the column
`scenario` is missing for some value of the column `region`. It no longer
results in output columns `production` and `technology_share` of type "list"
(#203).

* The website now shows the News tab.

# r2dii.analysis 0.1.0

* `target_sda()` now correctly handles differing `country_of_domicile` inputs 
  (#171).

* `target_market_share()` now outputs `technology_share` (#184).

* `join_ald_scenario()` now returns visibly with dev-magrittr (#188 @lionel-).

* `target_market_share()` gains `weight_production` parameter (#181).

* `target_market_share()` now correctly use `sector_ald` column from input 
  `data` argument (#178).

* `target_sda()` now automatically filters out `ald` rows where the 
  `emissions_factor` values are `NA` (#173).

* `join_ald_scenario()` now converts to lower case the values of the columns
  `sector_ald` and `technology` (#172).

* `target_sda()` now aggregates input `ald` by `technology` and `plant_location`
  prior to calculating targets (@QianFeng2020 #160).

* `target_sda()` now errors if input data has any duplicated `id_loan` 
  (@QianFeng2020 #164).

* `target_sda()` gains `by_company` parameter (#155).

* `target_market_share()` now outputs the actual aggregated corporate economy. 
  Previously, the output would, erroneously, be normalized to the starting 
  portfolio value (#158).

* `target_sda()` now correctly calculates SDA targets (#153):
  Targets are now calculated using scenario data that is adjusted to corporate 
  economy data. 
  The adjusted scenario data is also output by the function along with the usual 
  metrics. 
  Methodology error fixed, and reflected in the code. Previously, the target 
  was, incorrectly, calculated by multiplying the adjusted scenario. Now the 
  scenario data is added instead. 

* New `summarize_weighted_percent_change()` allows user to calculate a new 
  indicator (#141).

* `target_market_share()` no longer errors if the combination of `sector` and 
  `scenario_target_value` does not uniquely identify an observation 
  (@georgeharris2deg #142).

# r2dii.analysis 0.0.1

* First release on CRAN
