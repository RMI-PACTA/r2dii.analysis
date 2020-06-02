library(readr)
library(usethis)

path <- file.path("data-raw", "pick_tmsr_or_smsp.csv")
pick_tmsr_or_smsp <- readr::read_csv(path)

usethis::use_data(pick_tmsr_or_smsp,
                  overwrite = TRUE,
                  internal = TRUE)
