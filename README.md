
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/3jITMq8.png" align="right" height=40 /> TODO Add description

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.analysis)](https://CRAN.R-project.org/package=r2dii.analysis)
<!-- badges: end -->

The goal of r2dii.analysis is to TODO …

## Installation

Install the development version of r2dii.analysis with something like
this:

``` r
# install.packages("devtools")

# To install from a private repo, see ?usethis::browse_github_token()
devtools::install_github("2DegreesInvesting/r2dii.analysis", auth_token = "abc")
```

## Example

``` r
library(r2dii.analysis)
library(tidyverse)
#> -- Attaching packages -------------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.2.1     v purrr   0.3.3
#> v tibble  2.1.3     v dplyr   0.8.3
#> v tidyr   1.0.2     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.4.0
#> -- Conflicts ----------------------------------------------- tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(reprex)

sample_audit <- tibble::tribble(
  ~Investor.Name, ~Portfolio.Name, ~mapped_sector, ~Asset.Type,   ~ValueUSD,
    "Investor 1",   "Portfolio 1",   "Automotive",    "Equity",  9232082760,
    "Investor 1",   "Portfolio 1",        "Power",    "Equity",     9000000
)

sample_results <- tibble::tribble(
  ~Investor.Name, ~Portfolio.Name, ~Scenario,       ~Allocation, ~ScenarioGeography,      ~Sector,     ~Technology, ~Asset.Type, ~Year, ~Scen.Alloc.WtTechProd, ~Plan.Alloc.WtTechProd, ~Trajectory.Alignment, ~Trajectory.Deviation,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",           "Global", "Automotive",      "Electric",    "Equity",  2019,             706.930655,             706.930655,                     0,                     0,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",           "Global", "Automotive",        "Hybrid",    "Equity",  2019,            1470.743875,            1470.743875,                     0,                     0,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",           "Global", "Automotive",           "ICE",    "Equity",  2019,            63876.87817,            63876.87817,                     0,                     0,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",           "Global", "Automotive",      "Electric",    "Equity",  2020,            3221.822522,            1355.866636,            -0.5791616,            -0.5791616,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",           "Global", "Automotive",        "Hybrid",    "Equity",  2020,            4438.500794,            1712.821428,          -0.614099105,          -0.614099105,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",           "Global", "Automotive",           "ICE",    "Equity",  2020,            51984.88005,            63715.20756,          -0.225648833,           0.225648833,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",           "Global", "Automotive",      "Electric",    "Equity",  2021,            3767.862356,            2176.228222,          -0.422423641,          -0.422423641,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",           "Global", "Automotive",        "Hybrid",    "Equity",  2021,            6160.306542,            1802.532293,          -0.707395682,          -0.707395682,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",           "Global", "Automotive",           "ICE",    "Equity",  2021,            51011.34073,            63830.71251,          -0.251304349,           0.251304349,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2019,            116.6525386,            116.6525386,                     0,                     0,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2019,            269.6698767,            269.6698767,                     0,                     0,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2019,            232.8800937,            232.8800937,                     0,                     0,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2019,            116.0075844,            116.0075844,                     0,                     0,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2019,            28.13481867,            28.13481867,                     0,                     0,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2019,              194.86255,              194.86255,                     0,                     0,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2020,            105.4440023,            117.3498622,          -0.112911684,           0.112911684,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2020,            271.9095323,            284.0902211,          -0.044796844,           0.044796844,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2020,            234.8331245,            233.6997547,          -0.004826277,          -0.004826277,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2020,            115.3491503,            117.1959365,           0.016010402,           0.016010402,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2020,            26.32470632,            28.13483184,           0.068761471,           0.068761471,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2020,            213.0801716,            233.3274378,           0.095021822,           0.095021822,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2021,            94.46408664,            117.4049853,          -0.242853125,           0.242853125,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2021,            274.2804054,            291.7305933,          -0.063621708,           0.063621708,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2021,            236.4099468,             239.465056,            0.01292293,            0.01292293,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2021,            114.8347861,            117.2010447,           0.020605765,           0.020605765,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2021,            24.50423628,            28.13483184,           0.148161955,           0.148161955,
    "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2021,            229.0041531,            244.9723856,           0.069729008,           0.069729008,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2019,            116.6525386,            116.6525386,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2019,            269.6698767,            269.6698767,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2019,            232.8800937,            232.8800937,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2019,            116.0075844,            116.0075844,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2019,            28.13481867,            28.13481867,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2019,              194.86255,              194.86255,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2020,            114.8839584,            117.3498622,            -0.0214643,             0.0214643,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2020,            275.0620125,            284.0902211,          -0.032822448,           0.032822448,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2020,            234.0625011,            233.6997547,          -0.001549784,          -0.001549784,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2020,            113.7213785,            117.1959365,           0.030553253,           0.030553253,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2020,            26.58936285,            28.13483184,           0.058123581,           0.058123581,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2020,            208.5099164,            233.3274378,           0.119023219,           0.119023219,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2021,            113.1153782,            117.4049853,          -0.037922405,           0.037922405,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2021,            280.4541483,            291.7305933,          -0.040207802,           0.040207802,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2021,            235.2449086,             239.465056,           0.017939378,           0.017939378,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2021,            111.4351725,            117.2010447,           0.051741941,           0.051741941,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2021,            25.04390704,            28.13483184,           0.123420231,           0.123420231,
    "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2021,            222.1572827,            244.9723856,           0.102697974,           0.102697974,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",           "Global", "Automotive",      "Electric",    "Equity",  2019,             706.930655,             706.930655,                     0,                     0,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",           "Global", "Automotive",        "Hybrid",    "Equity",  2019,            1470.743875,            1470.743875,                     0,                     0,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",           "Global", "Automotive",           "ICE",    "Equity",  2019,            63876.87817,            63876.87817,                     0,                     0,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",           "Global", "Automotive",      "Electric",    "Equity",  2020,            760.1817186,            1355.866636,            0.78360858,            0.78360858,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",           "Global", "Automotive",        "Hybrid",    "Equity",  2020,            2804.653877,            1712.821428,          -0.389293116,          -0.389293116,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",           "Global", "Automotive",           "ICE",    "Equity",  2020,            64232.72117,            63715.20756,           0.008056853,          -0.008056853,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",           "Global", "Automotive",      "Electric",    "Equity",  2021,            797.0242196,            2176.228222,           1.730441771,           1.730441771,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",           "Global", "Automotive",        "Hybrid",    "Equity",  2021,            4431.021262,            1802.532293,          -0.593201615,          -0.593201615,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",           "Global", "Automotive",           "ICE",    "Equity",  2021,            64719.76135,            63830.71251,           0.013736899,          -0.013736899,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2019,            116.6525386,            116.6525386,                     0,                     0,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2019,            269.6698767,            269.6698767,                     0,                     0,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2019,            232.8800937,            232.8800937,                     0,                     0,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2019,            116.0075844,            116.0075844,                     0,                     0,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2019,            28.13481867,            28.13481867,                     0,                     0,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2019,              194.86255,              194.86255,                     0,                     0,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2020,            112.8284081,            117.3498622,          -0.040073721,           0.040073721,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2020,             273.999542,            284.0902211,          -0.036827357,           0.036827357,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2020,            233.9944091,            233.6997547,          -0.001259237,          -0.001259237,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2020,            113.7077179,            117.1959365,           0.030677062,           0.030677062,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2020,            26.56128136,            28.13483184,           0.059242265,           0.059242265,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2020,            212.2053808,            233.3274378,           0.099535916,           0.099535916,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2021,            109.0042776,            117.4049853,          -0.077067689,           0.077067689,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2021,            278.3292073,            291.7305933,          -0.048149406,           0.048149406,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2021,            235.1087245,             239.465056,           0.018529008,           0.018529008,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2021,            111.4078514,            117.2010447,           0.051999866,           0.051999866,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2021,            24.98774404,            28.13483184,           0.125945255,           0.125945255,
    "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2021,            229.5482117,            244.9723856,           0.067193614,           0.067193614,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",           "Global", "Automotive",      "Electric",    "Equity",  2019,             706.930655,             706.930655,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",           "Global", "Automotive",        "Hybrid",    "Equity",  2019,            1470.743875,            1470.743875,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",           "Global", "Automotive",           "ICE",    "Equity",  2019,            63876.87817,            63876.87817,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",           "Global", "Automotive",      "Electric",    "Equity",  2020,             2363.29258,            1355.866636,           -0.42628067,           -0.42628067,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",           "Global", "Automotive",        "Hybrid",    "Equity",  2020,            3841.135149,            1712.821428,          -0.554084571,          -0.554084571,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",           "Global", "Automotive",           "ICE",    "Equity",  2020,            54865.18084,            63715.20756,          -0.161304977,           0.161304977,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",           "Global", "Automotive",      "Electric",    "Equity",  2021,            2563.934942,            2176.228222,          -0.151215506,          -0.151215506,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",           "Global", "Automotive",        "Hybrid",    "Equity",  2021,            5634.454539,            1802.532293,          -0.680087526,          -0.680087526,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",           "Global", "Automotive",           "ICE",    "Equity",  2021,            54549.26632,            63830.71251,          -0.170147956,           0.170147956,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2019,            116.6525386,            116.6525386,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2019,            269.6698767,            269.6698767,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2019,            232.8800937,            232.8800937,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2019,            116.0075844,            116.0075844,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2019,            28.13481867,            28.13481867,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2019,              194.86255,              194.86255,                     0,                     0,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2020,             110.428017,            117.3498622,          -0.062681966,           0.062681966,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2020,            271.1505622,            284.0902211,          -0.047721306,           0.047721306,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2020,            234.3848457,            233.6997547,          -0.002922932,          -0.002922932,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2020,            114.2785561,            117.1959365,           0.025528678,           0.025528678,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2020,            26.36058381,            28.13483184,           0.067306856,           0.067306856,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2020,            218.5521323,            233.3274378,           0.067605406,           0.067605406,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",       "CoalCap",    "Equity",  2021,            104.2034954,            117.4049853,          -0.126689511,           0.126689511,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "GasCap",    "Equity",  2021,            272.6312477,            291.7305933,            -0.0700556,             0.0700556,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",      "HydroCap",    "Equity",  2021,            235.8895977,             239.465056,           0.015157338,           0.015157338,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",    "NuclearCap",    "Equity",  2021,            112.5495278,            117.2010447,           0.041328623,           0.041328623,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power",        "OilCap",    "Equity",  2021,            24.58634895,            28.13483184,           0.144327362,           0.144327362,
    "Investor 1",   "Portfolio 1",     "SDS", "PortfolioWeight",  "GlobalAggregate",      "Power", "RenewablesCap",    "Equity",  2021,            242.2417145,            244.9723856,           0.011272506,           0.011272506
  )

temperature_indicator_results <- calculate_temperature_indicator(
  input_results = sample_results,
  upper_temp_threshold = 6,
  lower_temp_threshold = 1.5,
  start_year = 2019,
  time_horizon = 3,
  allocation = "PortfolioWeight",
  production_type = "absolute",
  group_vars = c(
    "Investor.Name",
    "Portfolio.Name",
    "Asset.Type"
  )
)

influencemap_results <- apply_influencemap_portfolio_weighting(
  input_results = temperature_indicator_results,
  input_audit = sample_audit,
  metric_name = "temperature",
  group_vars = c(
    "Investor.Name",
    "Portfolio.Name",
    "Asset.Type"
  )
)

range_results <- find_range(
  input_results = influencemap_results,
  metric_name = "temperature_metric_group_vars",
  range = c(
    1.75, 2, 2.75, 3.5
    )
)

range_results %>%
  ungroup() %>%
  distinct(
    Investor.Name, Portfolio.Name, temperature_range
  )
#> # A tibble: 1 x 3
#>   Investor.Name Portfolio.Name temperature_range
#>   <chr>         <chr>          <chr>            
#> 1 Investor 1    Portfolio 1    2.01-2.75
```
