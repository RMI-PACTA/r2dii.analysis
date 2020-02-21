matched <- prioritize(match_name(loanbook = loanbook_demo, ald = ald_demo))

valid_matches <- matched %>%
  check_crucial_names(crucial_names())

path <- here::here("inst", "extdata", "valid_matches.csv")
readr::write_csv(valid_matches, path)

crucial_names <- function() {
  c(
    "name_ald",
    "sector_ald"
  )
}
