# for percent-change, with demo data returns known value

    Code
      credit_limit0
    Output
      # A tibble: 138 x 4
         sector_abcd technology  year weighted_percent_change
         <chr>       <chr>      <int>                   <dbl>
       1 automotive  electric    2020                0       
       2 automotive  electric    2021                0.000102
       3 automotive  electric    2022                0.000205
       4 automotive  electric    2023                0.000307
       5 automotive  electric    2024                0.000409
       6 automotive  electric    2025                0.000512
       7 automotive  electric    2026                0.000614
       8 automotive  hybrid      2020                0.00576 
       9 automotive  hybrid      2021                0.00547 
      10 automotive  hybrid      2022                0.00518 
      # ... with 128 more rows

---

    Code
      credit_limit1
    Output
      # A tibble: 138 x 4
         sector_abcd technology  year weighted_percent_change
         <chr>       <chr>      <int>                   <dbl>
       1 automotive  electric    2020               0        
       2 automotive  electric    2021               0.0000743
       3 automotive  electric    2022               0.000149 
       4 automotive  electric    2023               0.000223 
       5 automotive  electric    2024               0.000297 
       6 automotive  electric    2025               0.000371 
       7 automotive  electric    2026               0.000446 
       8 automotive  hybrid      2020               0.00540  
       9 automotive  hybrid      2021               0.00537  
      10 automotive  hybrid      2022               0.00534  
      # ... with 128 more rows

