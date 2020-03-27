test_that("with fake data outputs known value", {
  out <- summarize_company_production(
    fake_master()
  )

  expect_known_value(out, "ref-summarize_company_production", update = FALSE)
})
