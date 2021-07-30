# for production, with demo data returns known value

    Code
      credit_limit0
    Output
      # A tibble: 138 x 5
         sector_ald technology  year weighted_production weighted_technology_share
         <chr>      <chr>      <int>               <dbl>                     <dbl>
       1 automotive electric    2020             437827.                    0.548 
       2 automotive electric    2021             444635.                    0.548 
       3 automotive electric    2022             451443.                    0.548 
       4 automotive electric    2023             458252.                    0.547 
       5 automotive electric    2024             465060.                    0.547 
       6 automotive electric    2025             471868.                    0.547 
       7 automotive electric    2026             478676.                    0.547 
       8 automotive hybrid      2020             425303.                    0.0973
       9 automotive hybrid      2021             416061.                    0.0971
      10 automotive hybrid      2022             406818.                    0.0970
      # ... with 128 more rows

---

    Code
      credit_limit1
    Output
      # A tibble: 138 x 5
         sector_ald technology  year weighted_production weighted_technology_share
         <chr>      <chr>      <int>               <dbl>                     <dbl>
       1 automotive electric    2020             409532.                    0.540 
       2 automotive electric    2021             414472.                    0.540 
       3 automotive electric    2022             419411.                    0.539 
       4 automotive electric    2023             424350.                    0.539 
       5 automotive electric    2024             429290.                    0.538 
       6 automotive electric    2025             434229.                    0.538 
       7 automotive electric    2026             439168.                    0.537 
       8 automotive hybrid      2020             388781.                    0.0997
       9 automotive hybrid      2021             387804.                    0.100 
      10 automotive hybrid      2022             386827.                    0.101 
      # ... with 128 more rows

# for percent-change, with demo data returns known value

    Code
      credit_limit0
    Output
      # A tibble: 138 x 4
         sector_ald technology  year weighted_percent_change
         <chr>      <chr>      <int>                   <dbl>
       1 automotive electric    2020                0       
       2 automotive electric    2021                0.000102
       3 automotive electric    2022                0.000205
       4 automotive electric    2023                0.000307
       5 automotive electric    2024                0.000409
       6 automotive electric    2025                0.000512
       7 automotive electric    2026                0.000614
       8 automotive hybrid      2020                0.00576 
       9 automotive hybrid      2021                0.00547 
      10 automotive hybrid      2022                0.00518 
      # ... with 128 more rows

---

    Code
      credit_limit1
    Output
      # A tibble: 138 x 4
         sector_ald technology  year weighted_percent_change
         <chr>      <chr>      <int>                   <dbl>
       1 automotive electric    2020               0        
       2 automotive electric    2021               0.0000743
       3 automotive electric    2022               0.000149 
       4 automotive electric    2023               0.000223 
       5 automotive electric    2024               0.000297 
       6 automotive electric    2025               0.000371 
       7 automotive electric    2026               0.000446 
       8 automotive hybrid      2020               0.00540  
       9 automotive hybrid      2021               0.00537  
      10 automotive hybrid      2022               0.00534  
      # ... with 128 more rows

