# The `*_stable` datasets help create regression tests independent from changes
# in r2dii.data which might cause meaningless errors when checking for problems
# in reverse dependencies of r2dii.data. For example, in `loanbook_demo`, values
# of the columns `sector` and `borderline` may change if the classification
# system is updated (see #227).

library(dplyr, warn.conflicts = FALSE)
library(withr)
library(r2dii.match)

local({
  devtools::load_all(here::here("data-raw/r2dii.data-0.1.4-pruned/"))
  defer(unloadNamespace("r2dii.data"))
  stopifnot(packageVersion("r2dii.data") == "0.1.4")

  region_isos_stable <- region_isos_demo

  loanbook_demo <- loanbook_demo %>%
    mutate(across(.data$sector_classification_direct_loantaker, as.character))

  nace_classification <- nace_classification %>%
    select(.data$code, .data$sector, .data$borderline)

  loanbook_stable <- nace_classification %>%
    rename(sector_classification_direct_loantaker = .data$code) %>%
    mutate(across(sector_classification_direct_loantaker, as.character)) %>%
    right_join(loanbook_demo)

  local_options(r2dii.match.allow_reserved_columns = TRUE)
  matched_stable <- match_name(loanbook_stable, ald_demo) %>%
    prioritize()

  usethis::use_data(
    region_isos_stable,
    loanbook_stable,
    matched_stable,
    internal = TRUE,
    overwrite = TRUE
  )
})

stopifnot(packageVersion("r2dii.data") > "0.1.4")
