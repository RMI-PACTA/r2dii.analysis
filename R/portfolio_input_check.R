portfolio_input_check <- function() {
  # FIXME: Instead of `project_name` reuse usethis::proj_*()
  portfolio <- read_raw_portfolio_file(project_name)

  portfolio <- clean_colnames_portfolio_input_file(portfolio)

  portfolio <- rename_portfolio_columns(portfolio)

  portfolio <- clear_portfolio_input_blanks(portfolio)

  portfolio <- add_meta_portfolio(
    r2dii.utils::inc_metaportfolio(), r2dii.utils::inc_project_metaportfolio()
  )

  portfolio <- add_holding_id(portfolio)

  portfolio <- check_missing_cols(portfolio)

  portfolio <- clean_portfolio_col_types(portfolio)

  # FIXME: Where is `currencies` comming from? (ASK @Clare2D)
  # Is this the `Currencies` dataset?
  portfolio <- convert_currencies(portfolio, currencies)

  cols_portfolio_no_bbg <- colnames(portfolio)
  cols_funds <- c("direct_holding", "fund_isin", "original_value_usd")

  # Add financial data
  # Merges in the clean data and calculates the marketvalue and number of shares
  # FIXME: Where does `fin_data` come from? (ASK @Clare2D)
  portfolio <- add_fin_data(portfolio, fin_data)

  portfolio <- calculate_value_usd_with_fin_data(portfolio)

  original_value_usd <- sum(portfolio$value_usd, na.rm = TRUE)

  # identify funds in the portfolio
  fund_portfolio <- identify_fund_portfolio(portfolio)

  # Creates the fund_portfolio to match the original portfolio
  # FIXME: Where does `fund_data` come from?
  fund_portfolio <- calculate_fund_portfolio(fund_portfolio, fund_data)

  # Merges in the bbg data to the fund portfolio
  fund_portfolio <- add_fin_data(fund_portfolio, fin_data)

  # add fund_portfolio and check that the total value is the same
  portfolio_total <- add_fund_portfolio(portfolio, fund_portfolio)

  if (round(sum(portfolio_total$value_usd, na.rm = TRUE), 1) != round(original_value_usd, 1)) {
    stop("Fund Portfolio introducing errors in total value")
  }


  ### TO DO
  # summarise fund results
  # create summary of results
  #   use summary that already should exist
  summarise_fund_info <- function(fund_data) {




  }
  #   use summary data frame that's already created
  #   check for funds with no bbg info
  identify_missing_funds <- function(portfolio_total, fund_data) {}
  ###

  # Keep going with portfolio_total

  ### FLAGS/Exclusions

  portfolio_total <- check_isin_format(portfolio_total)
  portfolio_total <- check_missing_currency(portfolio_total)
  portfolio_total <- check_valid_input_value(portfolio_total)
  portfolio_total <- check_bloomberg_data(portfolio_total)

  portfolio_total <- add_flags(portfolio_total)
  portfolio_total <- overall_validity_flag(portfolio_total)


  portfolio_overview <- portfolio_summary(portfolio_total)
  # Missing is still flags about whether an isin is linked to ald or data or not.
  # these tend to confuse, without more specific information about what sector the asset level data is in
  # option to add in technology information from datastore?


  ### Create portfolios to print.

  ### Revenue Data
  # This needs to be an optional
}

### Portfolio cleaning functions
read_raw_portfolio_file <- function(project_name) {
  portfolio <- NA
  # FIXME: Instead of `peoject_location` reuse r2dii.utils
  input_path <- paste0(project_location, "20_Raw_Inputs/")

  csv_to_read <- list.files(path = input_path, pattern = paste0(project_name, "_Input.csv"))
  txt_to_read <- list.files(path = input_path, pattern = paste0(project_name, "_Input.txt"))


  if (length(csv_to_read) == 1) {
    portfolio <- readr::read_csv(paste0(input_path, csv_to_read))
  }
  if (length(txt_to_read) == 1) {
    # FIXME: Where does `guess_encoding()` come from? (ASK @Clare2D)
    enc <- guess_encoding(paste0(input_path, txt_to_read))$encoding[1]
    portfolio <- utils::read.table(paste0(input_path, txt_to_read), sep = ",", header = T, fileEncoding = enc)
  }

  # Reads in Files saved with a ; not a ,
  if (ncol(portfolio) == 1 & length(csv_to_read) == 1) {
    portfolio <- utils::read.csv(paste0(input_path, csv_to_read), strip.white = T, stringsAsFactors = F, sep = ";")
  }
  if (ncol(portfolio) == 1 & length(txt_to_read) == 1) {
    portfolio <- utils::read.table(paste0(input_path, txt_to_read), sep = "\t", header = T, fileEncoding = enc)
  }
  if (ncol(portfolio) == 1 & length(txt_to_read) == 1) {
    portfolio <- utils::read.table(paste0(input_path, txt_to_read), sep = ";", header = T, fileEncoding = enc)
  }



  if (!data_check(portfolio)) {
    stop("No portfolio Input File")
  }

  portfolio
}

rename_portfolio_columns <- function(portfolio) {
  portfolio <- plyr::rename(portfolio,
    replace = c(
      "PortfolioName" = "portfolio_name",
      "InvestorName" = "investor_name",
      "Portfolio.Name" = "portfolio_name",
      "Investor.Name" = "investor_name",
      "Currency" = "currency",
      "NumberofShares" = "number_of_shares",
      "MarketValue" = "market_value",
      "ISIN" = "isin"
    ),
    warn_missing = F
  )
}

clean_colnames_portfolio_input_file <- function(portfolio) {
  if (is.data.frame(portfolio)) {
    # Removes additional columns added by Excel on saving
    portfolio <- portfolio[, !grepl("X", colnames(portfolio))]
  } else {
    print("No portfolio Data readable")
  }

  colnames(portfolio) <- gsub("\u00EF..", "", colnames(portfolio))

  names(portfolio)[1] <- gsub("[^A-Za-z0-9]", "", names(portfolio)[1])


  portfolio
}

clean_portfolio_col_types <- function(portfolio) {
  portfolio$investor_name <- clean_punctuation(portfolio$investor_name)
  portfolio$portfolio_name <- clean_punctuation(portfolio$portfolio_name)
  portfolio$number_of_shares <- as.numeric(portfolio$number_of_shares)
  portfolio$market_value <- as.numeric(portfolio$market_value)
  portfolio$currency <- as.character(portfolio$currency)

  portfolio$currency <- if_else(portfolio$currency == "Euro", "EUR", portfolio$currency)

  portfolio
}

clear_portfolio_input_blanks <- function(portfolio) {

  # clear blank lines
  if (any(portfolio$investor_name == "" | is.na(portfolio$investor_name))) {
    ### TEST - this replaces a warning log
    print("Warning: Missing Investor Names, corresponding lines removed")
    print("Missing Investor Names, corresponding lines removed")
    portfolio <- portfolio[portfolio$investor_name != "" & !is.na(portfolio$investor_name), ]
  }

  if (any(portfolio$portfolio_name == "" | is.na(portfolio$portfolio_name))) {
    ### TEST
    print("Warning: Missing portfolio Names, corresponding lines removed")
    print("Missing portfolio Names, corresponding lines removed")
    portfolio <- portfolio[portfolio$portfolio_name != "" & !is.na(portfolio$portfolio_name), ]
  }

  portfolio
}

add_meta_portfolio <- function(inc_metaportfolio, inc_project_metaportfolio) {
  portfolio_meta <- portfolio

  if (inc_metaportfolio) {
    portfolio_meta$portfolio_name <- portfolio_meta$investor_name
    portfolio <- rbind(portfolio, portfolio_meta)
  }

  if (inc_project_metaportfolio) {
    portfolio_meta$portfolio_name <- project_meta_portfolio_name()
    portfolio_meta$investor_name <- project_meta_investor_name()
    portfolio <- rbind(portfolio, portfolio_meta)
  }

  portfolio
}

add_holding_id <- function(portfolio) {
  if (length(setdiff("holding_id", names(portfolio))) != 0) {
    portfolio$holding_id <- row.names(portfolio)
  }

  portfolio
}

check_missing_cols <- function(portfolio) {
  required_input_cols <- c("holding_id", "market_value", "currency", "isin", "portfolio_name", "investor_name", "number_of_shares")

  missing_columns <- setdiff(required_input_cols, colnames(portfolio))

  if (length(missing_columns) > 0) {
    # FIXME: Where is this function comming from? (ASK @Clare2D)
    addMessageToLogFile("Error", paste0("The input file is missing the following data columns: ", missing_columns))
    error_count <- error_count + 1
  }

  portfolio <- as_tibble(portfolio)

  portfolio
}

###
set_currency_timestamp <- function(currencies) {
  currencies <- currencies %>%
    select(.data$Currency_abbr, paste0("ExchangeRate_", financial_timestamp())) %>%
    filter(!is.na(.data$Currency_abbr), .data$Currency_abbr != "") %>%
    distinct()

  names(currencies) <- c("currency", "exchange_rate")

  currencies$exchange_rate <- as.numeric(currencies$exchange_rate)

  currencies
}



### Fin data cleaning functions

override_sector_classification <- function(fin_data, overrides) {
  overrides <- overrides %>%
    dplyr::mutate_at(
      dplyr::vars(
        .data$company_name,
        .data$company_corp_ticker,
        .data$fin_sector_override
      ),
      list(as.character)
    )

  overrides$sector_override <- TRUE


  # Merge in by company corp ticker
  overrides_cbt <- overrides %>%
    filter(
      .data$company_corp_ticker != "" | is.na(.data$company_corp_ticker)
    ) %>%
    select(
      .data$company_corp_ticker,
      .data$fin_sector_override,
      .data$sector_override
    ) %>%
    distinct()

  fin_data <- left_join(fin_data, overrides_cbt, by = "company_corp_ticker")

  # Merge in by bloomberg_id
  overrides_bbg <- overrides %>%
    filter(
      is.na(.data$company_corp_ticker) | .data$company_corp_ticker == ""
    ) %>%
    select(
      .data$bloomberg_id,
      .data$fin_sector_override,
      .data$sector_override
    ) %>%
    distinct()

  fin_data <- left_join(fin_data, overrides_bbg, by = "bloomberg_id")


  # Clean resulting financial data
  fin_data <- fin_data %>%
    mutate(
      sector_override = .data$sector_override.x,
      sector_override = if_else(
        .data$sector_override.y != "" & !is.na(.data$sector_override.y),
        .data$sector_override.y,
        .data$sector_override
      ),
      fin_sector_override = .data$fin_sector_override.x,
      fin_sector_override = if_else(
        !is.na(.data$fin_sector_override.y) & .data$fin_sector_override.y != "",
        .data$fin_sector_override.y,
        .data$fin_sector_override
      ),
      sector_override = if_else(is.na(.data$sector_override), FALSE, TRUE)
    ) %>%
    select(
      -.data$sector_override.x,
      -.data$sector_override.y,
      -.data$fin_sector_override.x,
      -.data$fin_sector_override.y
    )

  fin_data <- fin_data %>%
    mutate(
      mapped_sector = if_else(
        .data$sector_override, .data$fin_sector_override, .data$mapped_sector
      )
    ) %>%
    select(-.data$fin_sector_override)

  fin_data
}

check_asset_types <- function(fin_data) {
  fin_data <- fin_data %>%
    mutate(
      asset_type = if_else(
        .data$asset_type == "Other", "Others", .data$asset_type
      )
    )

  fin_data$asset_type <- first_char_up(fin_data$asset_type)

  ### TEST
  if (
    !any(unique(fin_data$asset_type) %in% r2dii.utils::allowable_asset_list())
  ) {
    stop("Check Financial Data Asset Types")
  }

  fin_data
}

check_mapped_assets_flag <- function(fin_data) {


  # convert old naming of "mapped to assets" column to be mapped_to_assets

  if ("EQ.mapped_to_assets" %in% colnames(fin_data) |
    "CB.mapped_to_assets" %in% colnames(fin_data) |
    "has_prod_after_2018" %in% colnames(fin_data)) {
    if ("EQ.mapped_to_assets" %in% colnames(fin_data)
    | "CB.mapped_to_assets" %in% colnames(fin_data)) {
      fin_data <- fin_data %>%
        mutate(
          mapped_to_assets = case_when(
            Asset.Type == "Equity" ~ EQ.mapped_to_assets,
            Asset.Type == "Bonds" ~ CB.mapped_to_assets,
            TRUE ~ 0
          )
        ) %>%
        select(-.data$CB.mapped_to_assets, -.data$EQ.mapped_to_assets)
    } else if ("has_prod_after_2018" %in% colnames(fin_data)) {
      fin_data <- fin_data %>%
        mutate(
          mapped_to_assets = .data$has_prod_after_2018
        ) %>%
        select(-.data$has_prod_after_2018)
    }
  }

  unique(fin_data$mapped_to_assets)

  # Ensure that flag is a logical

  fin_data <- fin_data %>%
    mutate(mapped_to_assets = case_when(
      mapped_to_assets %in% c("t", 1) ~ TRUE,
      mapped_to_assets %in% c("f", 0) ~ FALSE
    ))

  ### TEST
  any(!fin_data$mapped_to_assets %in% c(TRUE, FALSE))
  ###


  fin_data
}

check_fin_mapped_sectors <- function(fin_data) {
  fin_data <- fin_data %>%
    mutate(mapped_sector = case_when(
      mapped_sector == "Others" ~ "Other",
      mapped_sector == "OIl&Gas" ~ "Oil&Gas",
      mapped_sector == "Steel" ~ "Cement&Steel",
      mapped_sector == "Cement" ~ "Cement&Steel",
      TRUE ~ mapped_sector
    ))

  # actual_sectors <- unique(fin_data$mapped_sector)

  if (any(!unique(fin_data$mapped_sector) %in% c(sector_list(), other_sector_list(), "Other"))) {
    stop("Additional Sectors in fin_data")
  }

  fin_data
}

identify_sb <- function(fin_data) {
  sb_groups <- c("Sovereign Debt", "Sovereign Agency Debt", "Government inflation linked Bonds", "Sovereign", "Sovereign Agency", "Sovereigns")

  fin_data <- fin_data %>%
    mutate(is_sb = case_when(
      security_type %in% sb_groups ~ TRUE,
      bics_subgroup %in% sb_groups ~ TRUE,
      TRUE ~ FALSE
    ))

  fin_data
}

classify_all_funds <- function(fin_data) {
  nrow(fin_data[fin_data$asset_type == "Funds", ])

  fin_data <- fin_data %>%
    mutate(asset_type = case_when(
      grepl("Fund", security_type) ~ "Funds",
      grepl("ETF", security_type) ~ "Funds",
      grepl("Fund", bclass4) ~ "Funds",
      grepl("ETF", bclass4) ~ "Funds",
      grepl("Fund", icb_subsector) ~ "Funds",
      grepl("ETF", icb_subsector) ~ "Funds",
      TRUE ~ asset_type
    ))


  ### TEST?

  fin_data
}

clean_fin_data <- function(fin_data_raw, overrides) {
  fin_data <- fin_data_raw

  # Adds in the manual sector classification overrides
  fin_data <- override_sector_classification(fin_data_raw, overrides)

  # Checks that only eq, cb, funds and others are in the fin_data
  fin_data <- check_asset_types(fin_data)

  # Checks for other mapped sectors not within the sector lists
  fin_data <- check_fin_mapped_sectors(fin_data)

  # Cleans and normalises the mapped_to_assets flag
  fin_data <- check_mapped_assets_flag(fin_data)

  # Checks whether the bond is sovereign or not
  fin_data <- identify_sb(fin_data)

  # Checks to ensure all finds are classified as such
  fin_data <- classify_all_funds(fin_data)

  # POSSIBLE colnames(fin_data)
  # [1] "bloomberg_id"                           "company_name"                           "country_of_domicile"                    "ticker"
  # [5] "company_corp_ticker"                    "listed_owner_bloomberg_id"              "figi"                                   "isin"
  # [9] "cusip"                                  "sedol1"                                 "asset_type"                             "icb_subsector"
  # [13] "bics_subgroup"                          "bclass4"                                "mapped_sector"                          "cfi_code"
  # [17] "security_type"                          "maturity_date"                          "amount_issued"                          "amount_out"
  # [21] "coupon_value"                           "par_price"                              "par_amount"                             "current_face_amount"
  # [25] "accrued_interest"                       "security_amount_out_native"             "unit_share_price"                       "position_market"
  # [29] "market_value"                           "current_shares_outstanding"             "current_shares_outstanding_all_classes" "free_float_percent"
  # [33] "free_float_shares"                      "exchange_rate_usd"                      "calendar_year"                          "calendar_quarter"
  # [37] "mapped_to_assets"

  # Select relevant columns
  fin_data <- fin_data %>%
    mutate(ald_date = paste0(.data$calendar_year, .data$calendar_quarter)) %>%
    select(
      .data$bloomberg_id,
      .data$company_name,
      .data$country_of_domicile,
      .data$ticker,
      .data$company_corp_ticker,
      .data$isin, # figi, cusip, sedol1,
      .data$unit_share_price,
      .data$exchange_rate_usd,
      .data$asset_type,
      .data$security_type,
      .data$mapped_sector,
      .data$icb_subsector,
      .data$bics_subgroup, # bclass4,
      .data$mapped_to_assets,
      .data$sector_override,
      .data$is_sb,
      .data$ald_date
    )

  fin_data
}

###


### Fund data cleaning functions
clean_fund_data <- function(fund_data) {

  # checks to see whether sufficient data is present
  # fund_data_col_names <- c( "Fund.ISIN","Holding.ISIN","ISIN.Weight","Fund.Type")

  fund_data <- plyr::rename(fund_data,
    replace = c(
      "Fund.ISIN" = "fund_isin",
      "Holding.ISIN" = "holding_isin",
      "ISIN.Weight" = "isin_weight",
      "Fund.Type" = "fund_type"
    ),
    warn_missing = F
  )

  fund_data <- fund_data %>%
    filter(!is.na(.data$holding_isin) & .data$holding_isin != "")


  fund_data
}

normalise_fund_data <- function(fund_data) {
  if (data_check(fund_data)) {
    fund_data <- fund_data %>%
      group_by(.data$fund_isin) %>%
      mutate(total_weight = sum(.data$isin_weight, na.rm = TRUE))

    fund_data_large <- fund_data %>%
      group_by(.data$fund_isin) %>%
      filter(.data$total_weight > 1) %>%
      mutate(isin_weight = .data$isin_weight / .data$total_weight) %>%
      select(-.data$total_weight)

    fund_data_small <- fund_data %>%
      group_by(.data$fund_isin) %>%
      filter(.data$total_weight <= 1) %>%
      select(-.data$total_weight)

    fund_data_missing <- fund_data_small %>%
      dplyr::summarise(
        isin_weight = 1 - sum(.data$isin_weight, na.rm = TRUE)
      ) %>%
      mutate(holding_isin = "MissingValue")


    fund_data <- dplyr::bind_rows(
      fund_data_large, fund_data_small, fund_data_missing
    )

    fund_data
  } else {
    stop("No fund data")
  }

  fund_data
}


### Portfolio Check Functions
convert_currencies <- function(portfolio, currencies) {
  portfolio <- left_join(portfolio, currencies, by = "currency")

  portfolio$value_usd <- portfolio$market_value * portfolio$exchange_rate

  portfolio
}

add_fin_data <- function(portfolio, fin_data) {
  portfolio_fin <- left_join(portfolio, fin_data, by = "isin")

  portfolio_fin
}

calculate_value_usd_with_fin_data <- function(portfolio) {

  # check correct inputs
  necessary_columns <- c("currency", "unit_share_price")

  ### TEST
  if (!any(necessary_columns %in% colnames(portfolio))) {
    stop("portfolio not structured correctly")
  }


  # add missing currency for number of shares
  portfolio <- portfolio %>%
    mutate(
      currency = if_else(!is.na(.data$number_of_shares), "USD", .data$currency)
    )

  # calculates the value_usd where number of shares are given
  portfolio <- portfolio %>%
    mutate(value_usd = if_else(
      .data$asset_type %in% c("Equity", "Funds") & is.na(.data$value_usd),
      .data$number_of_shares * .data$unit_share_price,
      .data$value_usd
    ))

  portfolio
}

identify_fund_portfolio <- function(portfolio) {
  portfolio %>% filter(.data$asset_type == "Funds")
}

calculate_fund_portfolio <- function(fund_portfolio, fund_data) {
  if (data_check(fund_portfolio)) {
    fund_portfolio <- left_join(fund_portfolio, fund_data, by = c("isin" = "fund_isin"), all.x = T)
    fund_portfolio$direct_holding <- FALSE

    fund_portfolio$original_value_usd <- fund_portfolio$value_usd
    fund_portfolio$value_usd <- fund_portfolio$isin_weight * fund_portfolio$value_usd
    fund_portfolio$fund_isin <- fund_portfolio$isin
    fund_portfolio$isin <- fund_portfolio$holding_isin

    # If there is no fund breakdown available, return the "original isin data"
    # to the original locations
    fund_portfolio <- fund_portfolio %>%
      mutate(
        value_usd = if_else(
          !.data$fund_isin %in% fund_data$fund_isin,
          .data$original_value_usd,
          .data$value_usd
        ),
        isin = if_else(
          !.data$fund_isin %in% fund_data$fund_isin,
          .data$fund_isin,
          .data$isin
        ),
        direct_holding = if_else(
          !.data$fund_isin %in% fund_data$fund_isin,
          TRUE,
          .data$direct_holding
        ),
      )
  } else {
    fund_portfolio <- fund_portfolio %>%
      dplyr::bind_cols(
        data.frame(
          direct_holding = integer(0),
          fund_isin = character(0),
          original_value_usd = numeric(0)
        )
      )
  }



  fund_portfolio <- fund_portfolio %>%
    select(.data$cols_portfolio_no_bbg, .data$cols_funds)

  fund_portfolio
}

add_fund_portfolio <- function(portfolio, fund_portfolio) {

  # Remove the fund lines from the portfolio
  portfolio_no_funds <- portfolio %>%
    filter(!.data$isin %in% fund_portfolio$fund_isin)

  # Check that there are the correct number of isins in both portfolios
  if (nrow(portfolio_no_funds) + length(unique(fund_portfolio$holding_id)) != nrow(portfolio)) {
    stop("Something unexpected with fund portfolio merge")
  }

  # Add additional fund relevant lines to original portfolio
  portfolio_no_funds <- portfolio_no_funds %>%
    mutate(
      direct_holding = TRUE,
      fund_isin = NA,
      original_value_usd = .data$value_usd
    )

  # select same columns for both portfolios
  portfolio_no_funds <- portfolio_no_funds %>%
    select(colnames(portfolio), .data$cols_funds)
  fund_portfolio <- fund_portfolio %>%
    select(colnames(portfolio), .data$cols_funds)

  if (!identical(colnames(portfolio_no_funds), colnames(fund_portfolio))) {
    stop("Colnames not equal, funds vs no funds")
  }

  # Merge in the results

  portfolio_total <- rbind(portfolio_no_funds, fund_portfolio)

  portfolio_total <- as_tibble(portfolio_total)

  portfolio_total
}

check_funds_wo_bbg <- function(fund_data, fin_data) {

  # isin in the fund_data but no bbg data available
  fin_data_funds <- fin_data %>%
    filter(.data$asset_type == "Funds") %>%
    select(.data$isin) %>%
    distinct()

  fund_isins <- fund_data %>%
    select(.data$fund_isin) %>%
    distinct()

  fund_isins_missing_bbg <- fund_isins %>%
    filter(!.data$fund_isin %in% fin_data_funds$isin)

  known_missing_isins <- utils::read.csv("data/Fund_ISINs_No_BBG_Data.csv")
  known_missing_isins <- known_missing_isins %>%
    dplyr::bind_rows(fund_isins_missing_bbg) %>%
    distinct()

  utils::write.csv(
    fund_isins_missing_bbg,
    "data/Fund_ISINs_No_BBG_Data.csv",
    row.names = FALSE
  )

  if (data_check(fund_isins_missing_bbg)) {
    warning(
      "There are funds without bbg data. These are excluded from the analysis."
    )
  }
}


###

# Add Columns for missing or incorrect information
check_isin_format <- function(portfolio_total) {
  portfolio_total <- portfolio_total %>%
    mutate(has_valid_isin = case_when(
      nchar(isin) != 12 ~ FALSE,
      isin == "" ~ FALSE,
      is.na(isin) ~ FALSE,
      grepl("[^[:alnum:]]", isin) ~ FALSE,
      TRUE ~ TRUE
    ))

  portfolio_total
}

check_missing_currency <- function(portfolio_total) {

  # Currency blank or not in our currency data frame
  portfolio_total <- portfolio_total %>%
    mutate(has_currency = case_when(
      is.na(currency) ~ FALSE,
      currency == "" ~ FALSE,
      !currency %in% currencies$currency ~ FALSE,
      TRUE ~ TRUE
    ))

  portfolio_total
}

check_valid_input_value <- function(portfolio_total) {

  # Currency negative or missing market value/number of shares
  portfolio_total <- portfolio_total %>%
    mutate(has_valid_input = case_when(
      is.na(market_value) & is.na(number_of_shares) ~ FALSE,
      market_value < 0 ~ FALSE,
      number_of_shares < 0 ~ FALSE,
      # !currency %in% currencies$currency ~ FALSE,
      TRUE ~ TRUE
    ))

  portfolio_total
}

check_bloomberg_data <- function(portfolio_total) {
  portfolio_total <- portfolio_total %>%
    mutate(has_bbg_data = case_when(
      asset_type == "Equity" & (is.na(bloomberg_id) | bloomberg_id == "" | is.na(mapped_sector) | mapped_sector == "") ~ FALSE,
      asset_type == "Bonds" & (is.na(company_corp_ticker) | company_corp_ticker == "" | is.na(mapped_sector) | mapped_sector == "") ~ FALSE,
      asset_type == "" ~ FALSE,
      is.na(asset_type) ~ FALSE,
      TRUE ~ TRUE
    ))

  portfolio_total
}

add_flags <- function(portfolio_total) {
  portfolio_total <- portfolio_total %>%
    mutate(flag = case_when(
      !has_currency ~ "Missing currency information",
      !has_bbg_data ~ "Holding not in Bloomberg database",
      !has_valid_input ~ "Negative or missing input value",
      !has_valid_isin ~ "Invalid or missing ISIN",
      TRUE ~ "Included in analysis"
    ))

  portfolio_total
}

overall_validity_flag <- function(portfolio_total) {
  portfolio_total <- portfolio_total %>%
    mutate(valid_input = case_when(
      !has_currency ~ FALSE,
      !has_bbg_data ~ FALSE,
      !has_valid_input ~ FALSE,
      !has_valid_isin ~ FALSE,
      TRUE ~ TRUE
    ))

  portfolio_total
}

portfolio_summary <- function(portfolio_total) {
  overview_data <- portfolio_total %>%
    group_by(
      .data$investor_name,
      .data$portfolio_name,
      .data$asset_type,
      .data$mapped_sector,
      .data$valid_input,
      .data$mapped_to_assets
    ) %>%
    mutate(mapped_value_usd = sum(.data$value_usd, na.rm = TRUE)) %>%
    group_by(
      .data$investor_name,
      .data$portfolio_name,
      .data$asset_type,
      .data$mapped_sector,
      .data$valid_input
    ) %>%
    mutate(valid_value_usd = sum(.data$value_usd, na.rm = TRUE)) %>%
    group_by(
      .data$investor_name,
      .data$portfolio_name,
      .data$asset_type,
      .data$valid_input
    ) %>%
    mutate(asset_value_usd = sum(.data$value_usd, na.rm = TRUE)) %>%
    group_by(.data$investor_name, .data$portfolio_name, .data$valid_input) %>%
    mutate(portfolio_value_usd = sum(.data$value_usd, na.rm = TRUE)) %>%
    select(
      .data$investor_name,
      .data$portfolio_name,
      .data$asset_type,
      .data$mapped_sector,
      .data$valid_input,
      .data$mapped_to_assets,
      .data$mapped_value_usd,
      .data$valid_value_usd,
      .data$asset_value_usd,
      .data$portfolio_value_usd
    ) %>%
    distinct()

  overview_data
}

### print portfolios
