# The `*_stable` datasets help create regression tests independent from changes
# in r2dii.data which might cause meaningless errors when checking for problems
# in reverse dependencies of r2dii.data. For example, in `loanbook_demo`, values
# of the columns `sector` and `borderline` may change if the classification
# system is updated (see #227).

library(dplyr)
library(fs)
library(withr)

# Access older version of r2dii.data without changing current version
temp_lib <- tempdir()
fs::dir_create(temp_lib)

withr::with_libpaths(temp_lib, {
  remotes::install_version("r2dii.data", version = "0.1.4")
  region_isos_demo <- r2dii.data::region_isos_demo
  nace_classification <- r2dii.data::nace_classification
  loanbook_demo <- r2dii.data::loanbook_demo
})

region_isos_stable <- region_isos_demo

nace_classification <- nace_classification %>%
  select(.data$code, .data$sector, .data$borderline)
loanbook_stable <- left_join(
  loanbook_demo, nace_classification,
  by = c(sector_classification_direct_loantaker = "code")
)

usethis::use_data(
  region_isos_stable,
  loanbook_stable,
  internal = TRUE,
  overwrite = TRUE
)

fs::dir_delete(temp_lib)
