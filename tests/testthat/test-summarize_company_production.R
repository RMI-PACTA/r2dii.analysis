test_that("with known input outputs as expected", {

  data <- fake_master(
    name =                  c("company a", "company b", "company a", "company b"),
    technology =            c("ta", "ta", "tb", "tb"),
    id_loan    =            c("i1", "i2", "i1", "i2"),
    loan_size_outstanding = c(40,   10,   40,   10),
    production            = c(10,   30,   20,   40),
    region = "global"
  )

  out <- summarize_company_production(data)
  expect_equal(out$weighted_production, c(8, 6, 16, 8))

})
