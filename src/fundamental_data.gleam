import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/string

/// Fundamental Data Module
/// 
/// This module provides functionality to retrieve and analyze
/// fundamental company data such as financial ratios, earnings,
/// revenue, and other key metrics for fundamental analysis.
/// Fundamental data report type
pub type FundamentalReport {
  FundamentalReport(
    /// Request ID
    request_id: Int,
    /// Contract ID
    contract_id: Int,
    /// Symbol
    symbol: String,
    /// Exchange
    exchange: String,
    /// Currency
    currency: String,
  )
}

/// Financial ratio
pub type FinancialRatio {
  FinancialRatio(
    /// Ratio name
    name: String,
    /// Ratio value
    value: Float,
    /// Description
    description: String,
  )
}

/// Earnings data
pub type EarningsData {
  EarningsData(
    /// EPS (Earnings Per Share)
    eps: Float,
    /// PE ratio (Price to Earnings)
    pe_ratio: Float,
    /// PEG ratio (Price/Earnings to Growth)
    peg_ratio: Float,
    /// Dividend yield
    dividend_yield: Float,
    /// Payout ratio
    payout_ratio: Float,
  )
}

/// Revenue data
pub type RevenueData {
  RevenueData(
    /// Total revenue
    total_revenue: Float,
    /// Revenue growth (year over year)
    revenue_growth: Float,
    /// Revenue per share
    revenue_per_share: Float,
    /// Revenue margin
    revenue_margin: Float,
  )
}

/// Balance sheet data
pub type BalanceSheetData {
  BalanceSheetData(
    /// Total assets
    total_assets: Float,
    /// Total liabilities
    total_liabilities: Float,
    /// Shareholder equity
    shareholder_equity: Float,
    /// Debt to equity ratio
    debt_to_equity: Float,
    /// Current ratio
    current_ratio: Float,
  )
}

/// Request fundamental data
pub fn request_fundamental_data(request_id: Int, contract_id: Int) -> BitArray {
  // Message format for REQ_FUNDAMENTAL_DATA (MsgCode 18)
  bit_array.concat([
    <<18:size(8)>>,
    <<request_id:size(32)>>,
    <<contract_id:size(32)>>,
  ])
}

/// Cancel fundamental data
pub fn cancel_fundamental_data(request_id: Int) -> BitArray {
  // Message format for CANCEL_FUNDAMENTAL_DATA (MsgCode 19)
  bit_array.concat([<<19:size(8)>>, <<request_id:size(32)>>])
}

/// Analysis Functions
/// Calculate PE ratio from price and EPS
pub fn calculate_pe_ratio(price: Float, eps: Float) -> Result(Float, String) {
  case eps {
    0.0 -> Error("EPS cannot be zero")
    _ -> Ok(price /. eps)
  }
}

/// Calculate dividend yield from dividend and price
pub fn calculate_dividend_yield(
  dividend: Float,
  price: Float,
) -> Result(Float, String) {
  case price {
    0.0 -> Error("Price cannot be zero")
    _ -> Ok(dividend /. price *. 100.0)
  }
}

/// Calculate debt to equity ratio
pub fn calculate_debt_to_equity(
  debt: Float,
  equity: Float,
) -> Result(Float, String) {
  case equity {
    0.0 -> Error("Equity cannot be zero")
    _ -> Ok(debt /. equity)
  }
}

/// Calculate current ratio
pub fn calculate_current_ratio(
  current_assets: Float,
  current_liabilities: Float,
) -> Result(Float, String) {
  case current_liabilities {
    0.0 -> Error("Current liabilities cannot be zero")
    _ -> Ok(current_assets /. current_liabilities)
  }
}

/// Filter ratios by name pattern
pub fn filter_ratios_by_name(
  ratios: List(FinancialRatio),
  pattern: String,
) -> List(FinancialRatio) {
  list.filter(ratios, fn(ratio) { string.contains(ratio.name, pattern) })
}

/// Get ratio by name
pub fn get_ratio_by_name(
  ratios: List(FinancialRatio),
  name: String,
) -> Result(FinancialRatio, String) {
  case list.find(ratios, fn(ratio) { ratio.name == name }) {
    Ok(ratio) -> Ok(ratio)
    Error(Nil) -> Error("Ratio not found: " <> name)
  }
}

/// Format financial ratio for display
pub fn format_financial_ratio(ratio: FinancialRatio) -> String {
  ratio.name
  <> ": "
  <> float.to_string(ratio.value)
  <> " - "
  <> ratio.description
}

/// Format earnings data for display
pub fn format_earnings_data(earnings: EarningsData) -> String {
  "
Earnings Data:
  EPS: " <> float.to_string(earnings.eps) <> "
  PE Ratio: " <> float.to_string(earnings.pe_ratio) <> "
  PEG Ratio: " <> float.to_string(earnings.peg_ratio) <> "
  Dividend Yield: " <> float.to_string(earnings.dividend_yield) <> "%
  Payout Ratio: " <> float.to_string(earnings.payout_ratio) <> "%"
}

/// Format revenue data for display
pub fn format_revenue_data(revenue: RevenueData) -> String {
  "
Revenue Data:
  Total Revenue: " <> float.to_string(revenue.total_revenue) <> "
  Revenue Growth: " <> float.to_string(revenue.revenue_growth) <> "% YoY
  Revenue Per Share: " <> float.to_string(revenue.revenue_per_share) <> "
  Revenue Margin: " <> float.to_string(revenue.revenue_margin) <> "%"
}

/// Format balance sheet data for display
pub fn format_balance_sheet_data(balance: BalanceSheetData) -> String {
  "
Balance Sheet Data:
  Total Assets: " <> float.to_string(balance.total_assets) <> "
  Total Liabilities: " <> float.to_string(balance.total_liabilities) <> "
  Shareholder Equity: " <> float.to_string(balance.shareholder_equity) <> "
  Debt to Equity: " <> float.to_string(balance.debt_to_equity) <> "
  Current Ratio: " <> float.to_string(balance.current_ratio)
}

/// Helper Functions
/// Convert integer to field (simplified)
fn int_to_field(value: Int) -> BitArray {
  // Simplified implementation
  bit_array.from_string(int.to_string(value) <> "\\0")
}

/// Parse fundamental data (placeholder - to be implemented in message_handler)
pub fn parse_fundamental_data(
  data: BitArray,
) -> Result(FundamentalReport, String) {
  // This will be implemented when parsing actual fundamental data
  Error("Not yet implemented - will be added to message_handler")
}

/// Parse earnings data (placeholder - to be implemented in message_handler)
pub fn parse_earnings_data(data: BitArray) -> Result(EarningsData, String) {
  // This will be implemented when parsing actual earnings data
  Error("Not yet implemented - will be added to message_handler")
}

/// Parse revenue data (placeholder - to be implemented in message_handler)
pub fn parse_revenue_data(data: BitArray) -> Result(RevenueData, String) {
  // This will be implemented when parsing actual revenue data
  Error("Not yet implemented - will be added to message_handler")
}

/// Parse balance sheet data (placeholder - to be implemented in message_handler)
pub fn parse_balance_sheet_data(
  data: BitArray,
) -> Result(BalanceSheetData, String) {
  // This will be implemented when parsing actual balance sheet data
  Error("Not yet implemented - will be added to message_handler")
}
