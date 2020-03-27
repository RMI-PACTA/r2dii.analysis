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


test_that("with the ouptput of summarize_porfolio_production() just works", {
  # From test-summarize_portfolio_production.R
  data <- fake_master(
    name = paste("company", c("a", "b", "a", "b", "a", "b", "a", "b")),
    technology = c("ta", "ta", "tb", "tb", "ta", "ta", "tb", "tb"),
    id_loan = c("i1", "i2", "i1", "i2", "i1", "i2", "i1", "i2"),
    loan_size_outstanding = c(40, 10, 40, 10, 40, 10, 40, 10),
    production = c(10, 30, 20, 40, 10, 30, 20, 40),
    scenario = c("a", "a", "a", "a", "b", "b", "b", "b"),
    value = c(1, 2, 3, 4, 1, 2, 4, 8),
    units = "MW",
    region = "global"
  )

  out <- summarize_portfolio_production(data)
  expect_equal(out$weighted_production, c(14, 14, 24, 24))
})
