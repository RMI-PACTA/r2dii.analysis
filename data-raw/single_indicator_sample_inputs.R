path <- here::here("data-raw", "single_indicator_sample_inputs.xlsx")

sheets <- readxl::excel_sheets(path)

sheets_lst <- sheets %>%
  purrr::map(~ readxl::read_excel(path, sheet = .x)) %>%
  purrr::set_names(sheets)

sheets_csv <- here::here("data-raw", glue::glue("{sheets}.csv"))
purrr::walk2(sheets_lst, sheets_csv, readr::write_csv)

tech_sector_weighting <- sheets_lst$tech_sector_weighting
scenario_relationships <- sheets_lst$scenario_relationships
sample_results <- sheets_lst$sample_results
sample_audit <- sheets_lst$sample_audit

use_data(tech_sector_weighting, overwrite = TRUE)
use_data(scenario_relationships, overwrite = TRUE)
use_data(sample_results, overwrite = TRUE)
use_data(sample_audit, overwrite = TRUE)
