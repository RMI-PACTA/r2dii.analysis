library(dplyr)

test_that("with fake data outputs known value", {
  out <- summarize_company_production(
    fake_master()
  )

  expect_known_value(out, "ref-summarize_company_production", update = FALSE)
})

test_that("with known input outputs as expected", {
  # styler: off
  data <- fake_master(
    technology =            c("ta", "ta", "tb", "tb"),
    id_loan    =            c("i1", "i2", "i1", "i2"),
    loan_size_outstanding = c( 40,   10,   40,   10),
    production            = c( 10,   30,   20,   40),
  )
  out1 <- summarize_weighted_production(data)
  out1$weighted_production
  expect_equal(out1$weighted_production, c(14, 24))

  # Is sensitive to `use_credit_limit`
  # Reversing loan_size and production outputs reverse result
  data2 <- fake_master(
    technology =             c("ta", "ta", "tb", "tb"),
    id_loan    =             c("i1", "i2", "i1", "i2"),
    loan_size_credit_limit = c( 10,   40,   10,   40),
    production            =  c( 40,   20,   30,   10),
  )
  out2 <- summarize_weighted_production(data2, use_credit_limit = TRUE)
  expect_equal(out2$weighted_production, c(24, 14))
  # styler: on
})

test_that("works with demo data from r2dii.data", {
  master <- r2dii.data::loanbook_demo %>%
    r2dii.match::match_name(r2dii.data::ald_demo) %>%
    r2dii.match::prioritize() %>%
    join_ald_scenario(
      r2dii.data::ald_demo,
      r2dii.data::scenario_demo_2020,
      r2dii.data::region_isos_demo
    )

  expect_error_free(out <- summarize_company_production(master))
  expect_true(nrow(out) > 0L)
})

test_that("outputs uniquely identifiable scenario targets (#87)", {
  master <- fake_master(region = c("a", "b"),
                     tmsr = c(1,2))
  out <- summarize_company_production(master) %>%
    dplyr::group_by_at(dplyr::vars(-c(tmsr, smsp))) %>%
    dplyr::summarise(distinct_tmsr = n_distinct(tmsr),
              distinct_smsp = n_distinct(smsp))

  distinct_tmsr_equal_to_1 <- out$distinct_tmsr == 1
  distinct_smsp_equal_to_1 <- out$distinct_smsp == 1

  expect_true(all(distinct_tmsr_equal_to_1))
  expect_true(all(distinct_smsp_equal_to_1))

})
