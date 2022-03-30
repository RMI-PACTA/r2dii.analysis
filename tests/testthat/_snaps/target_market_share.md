# w/ fake data, outputs known value

    Code
      out
    Output
      # A tibble: 3 x 10
        sector     technology  year region scenario_source metric           production
        <chr>      <chr>      <dbl> <chr>  <chr>           <chr>                 <dbl>
      1 automotive ice         2025 global demo_2020       projected               1  
      2 automotive ice         2025 global demo_2020       target_sds              0.5
      3 automotive ice         2025 global demo_2020       corporate_econo~        1  
      # ... with 3 more variables: technology_share <dbl>, scope <chr>,
      #   percentage_of_initial_production_by_scope <dbl>

