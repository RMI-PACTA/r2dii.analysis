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
