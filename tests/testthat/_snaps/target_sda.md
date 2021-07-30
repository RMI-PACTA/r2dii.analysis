# with fake data outputs known value

    Code
      out
    Output
      # A tibble: 68 x 4
         sector  year emission_factor_metric emission_factor_value
         <chr>  <dbl> <chr>                                  <dbl>
       1 cement  2020 projected                              1    
       2 cement  2021 projected                              2    
       3 cement  2022 projected                              3    
       4 cement  2020 corporate_economy                      1    
       5 cement  2021 corporate_economy                      2    
       6 cement  2022 corporate_economy                      3    
       7 cement  2020 target_b2ds                            1    
       8 cement  2021 target_b2ds                            0.978
       9 cement  2022 target_b2ds                            0.956
      10 cement  2023 target_b2ds                            0.933
      # ... with 58 more rows

---

    Code
      out_company
    Output
      # A tibble: 68 x 5
         sector  year name_ald     emission_factor_metric emission_factor_value
         <chr>  <dbl> <chr>        <chr>                                  <dbl>
       1 cement  2020 shaanxi auto projected                              1    
       2 cement  2021 shaanxi auto projected                              2    
       3 cement  2022 shaanxi auto projected                              3    
       4 cement  2020 market       corporate_economy                      1    
       5 cement  2021 market       corporate_economy                      2    
       6 cement  2022 market       corporate_economy                      3    
       7 cement  2020 shaanxi auto target_b2ds                            1    
       8 cement  2021 shaanxi auto target_b2ds                            0.999
       9 cement  2022 shaanxi auto target_b2ds                            0.997
      10 cement  2023 <NA>         target_b2ds                           NA    
      # ... with 58 more rows

