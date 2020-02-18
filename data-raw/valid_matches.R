matched <- prioritize(match_name(loanbook = loanbook_demo, ald = ald_demo))

# FIXME: We should validate `valid_matches`
valid_matches <- matched

path <- here::here("inst", "extdata", "valid_matches.csv")
readr::write_csv(valid_matches, path)

