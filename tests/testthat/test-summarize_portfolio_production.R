test_that("with fake data outputs known value", {
  out <- summarize_portfolio_production(
    fake_master()
  )

  expect_known_value(out, "ref-summarize_portfolio_production", update = FALSE)
})
