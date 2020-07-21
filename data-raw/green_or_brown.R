library(readr)
library(usethis)

# Source: Conversation between @jdhoffa and @2diiKlaus on May 26
# NOTE to 2dii internals: if you wish to set different specifications
# for high-carbon/ low-carbon technologies, do so in this internal
# dataset
path_tmsr_or_smsp <- file.path("data-raw", "tmsr_or_smsp.csv")
tmsr_or_smsp <- readr::read_csv(path_tmsr_or_smsp)

path_green_or_brown <- file.path("data-raw", "green_or_brown.csv")
green_or_brown <- readr::read_csv(path_green_or_brown)

usethis::use_data(green_or_brown,
                  tmsr_or_smsp,
                  overwrite = TRUE,
                  internal = TRUE
)
