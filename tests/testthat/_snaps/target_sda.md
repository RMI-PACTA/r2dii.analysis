# with fake data outputs known value

    Code
      out
    Output
      # A tibble: 68 x 6
         sector  year region scenario_source emission_factor_metric emission_factor_v~
         <chr>  <dbl> <chr>  <chr>           <chr>                               <dbl>
       1 cement  2020 global demo_2020       projected                           1    
       2 cement  2021 global demo_2020       projected                           2    
       3 cement  2022 global demo_2020       projected                           3    
       4 cement  2020 global demo_2020       corporate_economy                   1    
       5 cement  2021 global demo_2020       corporate_economy                   2    
       6 cement  2022 global demo_2020       corporate_economy                   3    
       7 cement  2020 global demo_2020       target_b2ds                         1    
       8 cement  2021 global demo_2020       target_b2ds                         0.978
       9 cement  2022 global demo_2020       target_b2ds                         0.956
      10 cement  2023 global demo_2020       target_b2ds                         0.933
      # ... with 58 more rows

---

    Code
      out_company
    Output
      # A tibble: 68 x 7
         sector  year region scenario_source name_abcd    emission_factor_metric
         <chr>  <dbl> <chr>  <chr>           <chr>        <chr>                 
       1 cement  2020 global demo_2020       shaanxi auto projected             
       2 cement  2021 global demo_2020       shaanxi auto projected             
       3 cement  2022 global demo_2020       shaanxi auto projected             
       4 cement  2020 global demo_2020       market       corporate_economy     
       5 cement  2021 global demo_2020       market       corporate_economy     
       6 cement  2022 global demo_2020       market       corporate_economy     
       7 cement  2020 global demo_2020       shaanxi auto target_b2ds           
       8 cement  2021 global demo_2020       shaanxi auto target_b2ds           
       9 cement  2022 global demo_2020       shaanxi auto target_b2ds           
      10 cement  2023 global demo_2020       <NA>         target_b2ds           
      # ... with 58 more rows, and 1 more variable: emission_factor_value <dbl>

