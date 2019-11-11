#' @importFrom tibble as_tibble
#' @importFrom dplyr case_when distinct filter group_by if_else left_join
#' @importFrom dplyr mutate select
NULL

# FIXME: It is best practice to avoid this global variables in other ways:
# * Import required functions from others packages
# * Use the `.data$` pronoun to refer to variables of the data mask.
globalVariables(
  c(
    "CB.mapped_to_assets",
    "Currency_abbr",
    "DROPBOX.PATH",
    "EQ.mapped_to_assets",
    "addMessageToLogFile",
    "ald_date",
    "asset_type",
    "asset_value_usd",
    "bics_subgroup",
    "bloomberg_id",
    "calendar_quarter",
    "calendar_year",
    "cols_funds",
    "cols_portfolio_no_bbg",
    "company_corp_ticker",
    "company_name",
    "country_of_domicile",
    "currencies",
    "currency",
    "direct_holding",
    "exchange_rate_usd",
    "fin_data",
    "fin_sector_override",
    "fin_sector_override.x",
    "fin_sector_override.y",
    "fund_data",
    "fund_isin",
    "guess_encoding",
    "has_prod_after_2018",
    "holding_isin",
    "icb_subsector",
    "investor_name",
    "is_sb",
    "isin",
    "isin_weight",
    "mapped_sector",
    "mapped_to_assets",
    "mapped_value_usd",
    "number_of_shares",
    "original_value_usd",
    "path_dropbox_2dii",
    "portcheck_v2_path",
    "portfolio_name",
    "portfolio_value_usd",
    "project_location",
    "project_name",
    "sector_override",
    "sector_override.x",
    "sector_override.y",
    "security_type",
    "ticker",
    "total_weight",
    "unit_share_price",
    "valid_input",
    "valid_value_usd",
    "value_usd"
  )
)
