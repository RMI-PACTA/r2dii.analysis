add_revenue_data <- function(portfolio, revenue_data, port_type){

  if (port_type == "Equity"){sec_revenue <- sec_revenue %>% rename(company_id = bloomberg_id) %>% select(-ticker)}
  if (port_type == "Bonds"){sec_revenue <- sec_revenue %>% rename(company_id = ticker) %>% select(-bloomberg_id)}


  port_rev <- left_join(portfolio, sec_revenue, by = "company_id", all.x = T)

  # when to merge in?

  # Steps
  # 1. merge - check for no drops, missing data (where bbg is available) (set to Other)
  # 2. multiply by the weightof the portfolio - check that the total weight still adds up to 100%
  # 3. ensure that duplicate checks aren't a problem
  # 4. check that this works even if the revenue data isn't merged in.

  # To get the weighting in
  # 1. split the portfolios by asset type
  # 2. columns per weight methodology

}

calculate_revenue_values <- function(portfolio){


  if ("port_weight" %in% colnames(portfolio)){
    portfolio <- portfolio %>%
      mutate(port_weight = port_weight * tot_rev)
  }
  if ("ownership_weight" %in% colnames(portfolio)){
    portfolio <- portfolio %>%
      mutate(ownership_weight = ownership_weight * tot_rev)
  }
  if ("value_usd" %in% colnames(portfolio)){
    portfolio <- portfolio %>%
      mutate(value_usd = value_usd * tot_rev)
  }

  portfolio

}

select_portfolio_cols <- function(portfolio){

  if(!"ownership_weight" %in% colnames(portfolio)){
    portfolio <- portfolio %>% mutate(ownership_weight = NA)
  }

  if(!"has_revenue_data" %in% colnames(portfolio)){
    portfolio <- portfolio %>% mutate(has_revenue = FALSE)
  }

  if(!"sector" %in% colnames(portfolio)){
    portfolio <- portfolio %>% mutate(sector = NA)
  }


  portfolio <- portfolio %>%
    select(investor_name,
           portfolio_name,
           company_id,
           number_of_holdings,
           number_of_shares,
           value_usd,
           port_weight,
           ownership_weight,
           sector,
           has_revenue_data
           )

  portfolio

}

