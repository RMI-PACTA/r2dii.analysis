# The `*_stable` datasets help create regression tests independent from changes
# in r2dii.data which might cause meaningless errors when checking for problems
# in reverse dependencies of r2dii.data. For example, in `loanbook_demo`, values
# of the columns `sector` and `borderline` may change if the classification
# system is updated (see #227).

library(dplyr, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)
library(fs)
library(withr)
library(r2dii.match)

use_r2dii_data <- function(version) {
  library("r2dii.data", lib.loc = test_path("test_lib"))
  on.exit(unloadNamespace("r2dii.data"), add = TRUE)
  stopifnot(packageVersion("r2dii.data") == version)

  region <- r2dii.data::region_isos_demo

  loanbook_demo <- r2dii.data::loanbook_demo %>%
    mutate(across(.data$sector_classification_direct_loantaker, as.character))

  nace_classification <- r2dii.data::nace_classification %>%
    select(.data$code, .data$sector, .data$borderline)

  loanbook <- nace_classification %>%
    rename(sector_classification_direct_loantaker = .data$code) %>%
    mutate(across(sector_classification_direct_loantaker, as.character)) %>%
    right_join(loanbook_demo)

  local_options(r2dii.match.allow_reserved_columns = TRUE)
  matched <- match_name(loanbook, r2dii.data::ald_demo) %>%
    prioritize()

  list(
    region = region,
    loanbook = loanbook,
    matched = matched
  )
}

stable <- use_r2dii_data(version = "0.1.4")
region_isos_stable <- stable$region
loanbook_stable <- stable$loanbook
matched_stable <- stable$matched

usethis::use_data(
  region_isos_stable,
  loanbook_stable,
  matched_stable,
  internal = TRUE,
  overwrite = TRUE
)

installed_version_remains_the_same <- packageVersion("r2dii.data") > "0.1.4"
stopifnot(installed_version_remains_the_same)
