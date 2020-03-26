library(r2dii.data)

scenario_with_fair_shares <- r2dii.scenario::scenario_demo %>%
  r2dii.scenario::add_market_share_columns(start_year = 2020)

master <- r2dii.data::loanbook_demo %>%
  r2dii.match::match_name(r2dii.data::ald_demo) %>%
  r2dii.match::prioritize() %>%
  join_ald_scenario(r2dii.data::ald_demo, scenario_with_fair_shares)

data <- summarize_portfolio_production(master, tmsr, smsp)

test_that("outputs a tibble", {
  out <- add_portfolio_target(data)
  expect_is(out, "tbl_df")
})

test_that("with known input outputs known output", {
  out <- add_portfolio_target(data)

  expect_known_output(
    out,
    "ref-add_portfolio_target-output",
    print = TRUE
  )
})

test_that("with known input outputs known output", {
  expect_error(add_portfolio_target(1), "data.frame.*not.*TRUE")
})
