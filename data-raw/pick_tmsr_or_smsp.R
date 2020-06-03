library(readr)
library(usethis)

# Source: Conversation between @jdhoffa and @2diiKlaus on May 26
# NOTE to 2dii internals: if you wish to set different specifications
# for high-carbon/ low-carbon technologies, do so in this internal
# dataset
path <- file.path("data-raw", "pick_tmsr_or_smsp.csv")
pick_tmsr_or_smsp <- readr::read_csv(path)

usethis::use_data(pick_tmsr_or_smsp,
  overwrite = TRUE,
  internal = TRUE
)
