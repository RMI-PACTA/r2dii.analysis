# The `*_stable` datasets help create regression tests independent from changes
# in r2dii.data which might cause meaningless errors when checking for problems
# in reverse dependencies of r2dii.data. For example, in `loanbook_demo`, values
# of the columns `sector` and `borderline` may change if the classification
# system is updated (see #227).

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
