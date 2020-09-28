# Packages
library(r2dii.data)
library(r2dii.match)
library(r2dii.analysis)

# Data
loanbook <- loanbook_demo
ald <- ald_demo

# Match
matched <- match_name(loanbook, ald) %>%
  prioritize()
