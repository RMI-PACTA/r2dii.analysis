# Revenue methodology

weight_calculations <- function(portfolio, port_type){
  ### Create portfolios to process


  portfolio <- create_valid_portfolio(portfolio_total, port_type)
  portfolio <- set_portfolio_id(portfolio, port_type)

  portfolio_size <- calculate_port_size(portfolio)
  portfolio <- calculate_port_weight(portfolio,portfolio_size)
  portfolio <- consolidate_to_company_level(portfolio)


  if (port_type == "Equity"){


    portfolio <- merge_in_total_shares(portfolio, company_financial_data)
    portfolio <- calculate_ownership_weight(portfolio)
  }

  portfolio

}

create_valid_portfolio <- function(portfolio, port_type){

  portfolio <- portfolio %>%
    filter(asset_type == port_type,
           valid_input == TRUE)
  #TEST, if the portfolio has zero lines - this is fine

}

calculate_port_size <- function(portfolio){

  portfolio_size = sum(portfolio$value_usd, na.rm = T)

}


set_portfolio_id <- function(portfolio, port_type){

  if(port_type == "Equity"){
    portfolio$company_id <- portfolio$bloomberg_id

  }

  if (port_type == "Bonds"){
    portfolio$company_id <- portfolio$company_corp_ticker

  }

  portfolio

}

calculate_port_weight <- function(portfolio, portfolio_size){

  portfolio <- portfolio %>% mutate(port_weight = value_usd/portfolio_size)

  if(sum(portfolio$port_weight) != 1){print("Portfolio weight not == 1")}

  portfolio

}

consolidate_to_company_level <- function(portfolio){

  portfolio <- portfolio %>%
    group_by(investor_name, portfolio_name, company_id) %>%
    summarise(number_of_holdings = n_distinct(holding_id),
              number_of_shares = sum(number_of_shares, na.rm = T),
              value_usd = sum(value_usd, na.rm = T),
              port_weight = sum(port_weight))



}

merge_in_total_shares <- function(portfolio, company_financial_data){

  company_financial_data_short <- company_financial_data %>%
    select(bloomberg_id, current_shares_outstanding_all_classes) %>%
    distinct()

  portfolio <- merge(portfolio, company_financial_data_short, by.x = "company_id", by.y = "bloomberg_id", all.x = T)

  # test for any blank values in the total shares out.

}

calculate_ownership_weight <- function(portfolio){

  # PORT$Ownership.Wt <- PORT$Num.Shares / PORT$TotalShares
  portfolio <- portfolio %>%
    mutate(ownership_weight = number_of_shares/current_shares_outstanding_all_classes) %>%
    select(-current_shares_outstanding_all_classes)



}


