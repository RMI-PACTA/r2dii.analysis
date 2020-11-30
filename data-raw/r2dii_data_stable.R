# The purpose of this stable version of loanbook_demo is to mask the,
# potentially unstable data coming from r2dii.data, for the purpose of testing.
# In particular, the `sector` and `borderline` values are subject to change if
# the classification system is updated, and this gives many false positives in
# our regression tests. See the convo here:
# https://github.com/2DegreesInvesting/r2dii.analysis/issues/227

r2dii_data_installed <- requireNamespace("r2dii.data", quietly = TRUE)

if (r2dii_data_installed) {
  version <- packageVersion("r2dii.data")
}

install_version("r2dii.data", version = "0.1.4")

library(r2dii.data)
library(dplyr)

loanbook_stable <- loanbook_demo %>%
  left_join(
    select(nace_classification, code, sector, borderline),
    by = c(sector_classification_direct_loantaker = "code")
  )


# do the same for region_isos ------------------------------------------------

region_isos_stable <- region_isos_demo

usethis::use_data(
  loanbook_stable,
  region_isos_stable,
  internal = TRUE,
  overwrite = TRUE
)

if (r2dii_data_installed) {
  install_version("r2dii.data", version = version)
} else {
  remove.packages("r2dii.data")
}
