library(readr)
library(usethis)

# Source: Conversation between @jdhoffa and @2diiKlaus on May 26
# NOTE to 2dii internals: if you wish to set different specifications
# for high-carbon/ low-carbon technologies, do so in this internal
# dataset

path <- file.path("data-raw", "green_or_brown.csv")
green_or_brown <- readr::read_csv(path)

usethis::use_data(green_or_brown,
                  overwrite = TRUE,
                  internal = TRUE
)
