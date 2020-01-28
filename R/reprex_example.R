library(tidyverse)
library(reprex)


input_results = sample_results_t
upper_temp_threshold = 6
lower_temp_threshold = 1.5
start_year = 2019
time_horizon = 2
allocation = "PortfolioWeight"
production_type = "absolute"
group_vars = c(
  "Investor.Name",
  "Portfolio.Name",
  "Asset.Type"
)

sample_audit <- tibble::tribble(
                  ~Investor.Name, ~Portfolio.Name, ~mapped_sector, ~Asset.Type,   ~ValueUSD,
                    "Investor 1",   "Portfolio 1",   "Automotive",     "Bonds",  6367081739,
                    "Investor 1",   "Portfolio 1",   "Automotive",    "Equity",  9232082760,
                    "Investor 1",   "Portfolio 1",     "Aviation",     "Bonds",   778498174,
                    "Investor 1",   "Portfolio 1",     "Aviation",    "Equity",  1332915952,
                    "Investor 1",   "Portfolio 1", "Cement&Steel",     "Bonds",  5901164912,
                    "Investor 1",   "Portfolio 1", "Cement&Steel",    "Equity",  9374011798,
                    "Investor 1",   "Portfolio 1",         "Coal",     "Bonds",  2067796157,
                    "Investor 1",   "Portfolio 1",         "Coal",    "Equity",  5843238075,
                    "Investor 1",   "Portfolio 1",      "Oil&Gas",     "Bonds",  9155273820,
                    "Investor 1",   "Portfolio 1",      "Oil&Gas",    "Equity", 15591090390,
                    "Investor 1",   "Portfolio 1",        "Other",     "Bonds", 2.55032e+11,
                    "Investor 1",   "Portfolio 1",        "Other",    "Equity", 3.10931e+11,
                    "Investor 1",   "Portfolio 1",       "Others",     "Bonds",    46128806,
                    "Investor 1",   "Portfolio 1",       "Others",    "Equity",    37032865,
                    "Investor 1",   "Portfolio 1",        "Power",     "Bonds",  9386880315,
                    "Investor 1",   "Portfolio 1",        "Power",    "Equity", 12699834038,
                    "Investor 1",   "Portfolio 1",     "Shipping",     "Bonds",   263884127,
                    "Investor 1",   "Portfolio 1",     "Shipping",    "Equity",   135339639
                  )

sample_results_t <- tibble::tribble(
                    ~Investor.Name, ~Portfolio.Name, ~Scenario,       ~Allocation,        ~ScenarioGeography,      ~Sector,     ~Technology, ~Asset.Type, ~Year, ~Scen.Alloc.WtTechProd, ~Plan.Alloc.WtTechProd, ~Trajectory.Alignment, ~Trajectory.Deviation,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",          "Global", "Automotive",      "Electric",     "Bonds",  2019,             3327.76889,             3327.76889,                     0,                     0,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",          "Global", "Automotive",        "Hybrid",     "Bonds",  2019,             7810.34415,             7810.34415,                     0,                     0,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",          "Global", "Automotive",           "ICE",     "Bonds",  2019,            320085.6997,            320085.6997,                     0,                     0,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",          "Global", "Automotive",      "Electric",     "Bonds",  2020,            15938.43748,             5991.01396,          -0.624115352,          -0.624115352,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",          "Global", "Automotive",        "Hybrid",     "Bonds",  2020,            22691.85832,             9746.77591,          -0.570472556,          -0.570472556,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight",          "Global", "Automotive",           "ICE",     "Bonds",  2020,             260495.146,            319355.6634,          -0.225956293,           0.225956293,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power",       "CoalCap",     "Bonds",  2019,              168.31284,              168.31284,                     0,                     0,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power",        "GasCap",     "Bonds",  2019,              171.61067,              171.61067,                     0,                     0,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power",      "HydroCap",     "Bonds",  2019,              206.40172,              206.40172,                     0,                     0,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power",    "NuclearCap",     "Bonds",  2019,              298.52739,              298.52739,                     0,                     0,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power",        "OilCap",     "Bonds",  2019,               26.01953,               26.01953,                     0,                     0,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power", "RenewablesCap",     "Bonds",  2019,               95.58361,               95.58361,                     0,                     0,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power",       "CoalCap",     "Bonds",  2020,              159.30301,              172.90833,          -0.085405286,           0.085405286,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power",        "GasCap",     "Bonds",  2020,              174.13826,              187.89415,          -0.078994069,           0.078994069,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power",      "HydroCap",     "Bonds",  2020,              208.35003,              208.84726,           0.002386503,           0.002386503,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power",    "NuclearCap",     "Bonds",  2020,              296.86186,              304.65331,           0.026246031,           0.026246031,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power",        "OilCap",     "Bonds",  2020,               25.12819,               26.01953,           0.035471872,           0.035471872,
                      "Investor 1",   "Portfolio 1",    "B2DS", "PortfolioWeight", "GlobalAggregate",      "Power", "RenewablesCap",     "Bonds",  2020,              115.33216,              107.97356,          -0.063803537,          -0.063803537,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power",       "CoalCap",     "Bonds",  2019,              168.31284,              168.31284,                     0,                     0,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power",        "GasCap",     "Bonds",  2019,              171.61067,              171.61067,                     0,                     0,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power",      "HydroCap",     "Bonds",  2019,              206.40172,              206.40172,                     0,                     0,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power",    "NuclearCap",     "Bonds",  2019,              298.52739,              298.52739,                     0,                     0,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power",        "OilCap",     "Bonds",  2019,               26.01953,               26.01953,                     0,                     0,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power", "RenewablesCap",     "Bonds",  2019,               95.58361,               95.58361,                     0,                     0,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power",       "CoalCap",     "Bonds",  2020,              168.04879,              172.90833,          -0.028917437,           0.028917437,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power",        "GasCap",     "Bonds",  2020,              177.09782,              187.89415,          -0.060962494,           0.060962494,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power",      "HydroCap",     "Bonds",  2020,              207.33512,              208.84726,           0.007293197,           0.007293197,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power",    "NuclearCap",     "Bonds",  2020,              292.64852,              304.65331,           0.041021194,           0.041021194,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power",        "OilCap",     "Bonds",  2020,               25.08618,               26.01953,           0.037205828,           0.037205828,
                      "Investor 1",   "Portfolio 1",     "CPS", "PortfolioWeight", "GlobalAggregate",      "Power", "RenewablesCap",     "Bonds",  2020,               110.4335,              107.97356,          -0.022275304,          -0.022275304,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",          "Global", "Automotive",      "Electric",     "Bonds",  2019,             3327.76889,             3327.76889,                     0,                     0,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",          "Global", "Automotive",        "Hybrid",     "Bonds",  2019,             7810.34415,             7810.34415,                     0,                     0,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",          "Global", "Automotive",           "ICE",     "Bonds",  2019,            320085.6997,            320085.6997,                     0,                     0,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",          "Global", "Automotive",      "Electric",     "Bonds",  2020,             3594.79091,             5991.01396,           0.666582037,           0.666582037,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",          "Global", "Automotive",        "Hybrid",     "Bonds",  2020,             14499.0997,             9746.77591,           -0.32776682,           -0.32776682,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight",          "Global", "Automotive",           "ICE",     "Bonds",  2020,            321870.0381,            319355.6634,            0.00781177,           -0.00781177,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power",       "CoalCap",     "Bonds",  2019,              168.31284,              168.31284,                     0,                     0,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power",        "GasCap",     "Bonds",  2019,              171.61067,              171.61067,                     0,                     0,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power",      "HydroCap",     "Bonds",  2019,              206.40172,              206.40172,                     0,                     0,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power",    "NuclearCap",     "Bonds",  2019,              298.52739,              298.52739,                     0,                     0,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power",        "OilCap",     "Bonds",  2019,               26.01953,               26.01953,                     0,                     0,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power", "RenewablesCap",     "Bonds",  2019,               95.58361,               95.58361,                     0,                     0,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power",       "CoalCap",     "Bonds",  2020,              165.95516,              172.90833,          -0.041897851,           0.041897851,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power",        "GasCap",     "Bonds",  2020,              176.02716,              187.89415,          -0.067415702,           0.067415702,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power",      "HydroCap",     "Bonds",  2020,              207.32225,              208.84726,           0.007355718,           0.007355718,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power",    "NuclearCap",     "Bonds",  2020,              292.66924,              304.65331,           0.040947475,           0.040947475,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power",        "OilCap",     "Bonds",  2020,               25.08236,               26.01953,           0.037363713,           0.037363713,
                      "Investor 1",   "Portfolio 1",  "NPSRTS", "PortfolioWeight", "GlobalAggregate",      "Power", "RenewablesCap",     "Bonds",  2020,              114.43981,              107.97356,           -0.05650349,           -0.05650349
                    )

temperature_indicator_results <- calculate_temperature_indicator(
  input_results = sample_results_t,
  upper_temp_threshold = 6,
  lower_temp_threshold = 1.5,
  start_year = 2019,
  time_horizon = 2,
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
  range = c(1.75, 2, 2.75, 3.5)
  )

range_results %>%
  distinct(
    Investor.Name, Portfolio.Name, temperature_range
    )

