import fundamental_data
import gleam/bit_array
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Test 1: Fundamental Report Creation
pub fn fundamental_report_creation_test() {
  let report =
    fundamental_data.FundamentalReport(
      request_id: 1001,
      contract_id: 12_345,
      symbol: "AAPL",
      exchange: "SMART",
      currency: "USD",
    )

  report.request_id
  |> should.equal(1001)

  report.symbol
  |> should.equal("AAPL")

  report.currency
  |> should.equal("USD")
}

// Test 2: Financial Ratio Creation
pub fn financial_ratio_creation_test() {
  let ratio =
    fundamental_data.FinancialRatio(
      name: "PE Ratio",
      value: 25.5,
      description: "Price to Earnings ratio",
    )

  ratio.name
  |> should.equal("PE Ratio")

  ratio.value
  |> should.equal(25.5)

  ratio.description
  |> should.equal("Price to Earnings ratio")
}

// Test 3: Earnings Data Creation
pub fn earnings_data_creation_test() {
  let earnings =
    fundamental_data.EarningsData(
      eps: 5.5,
      pe_ratio: 25.5,
      peg_ratio: 1.2,
      dividend_yield: 1.5,
      payout_ratio: 30.0,
    )

  earnings.eps
  |> should.equal(5.5)

  earnings.pe_ratio
  |> should.equal(25.5)

  earnings.peg_ratio
  |> should.equal(1.2)
}

// Test 4: Revenue Data Creation
pub fn revenue_data_creation_test() {
  let revenue =
    fundamental_data.RevenueData(
      total_revenue: 100_000.0,
      revenue_growth: 15.5,
      revenue_per_share: 50.0,
      revenue_margin: 25.0,
    )

  revenue.total_revenue
  |> should.equal(100_000.0)

  revenue.revenue_growth
  |> should.equal(15.5)

  revenue.revenue_margin
  |> should.equal(25.0)
}

// Test 5: Balance Sheet Data Creation
pub fn balance_sheet_data_creation_test() {
  let balance =
    fundamental_data.BalanceSheetData(
      total_assets: 200_000.0,
      total_liabilities: 80_000.0,
      shareholder_equity: 120_000.0,
      debt_to_equity: 0.67,
      current_ratio: 2.5,
    )

  balance.total_assets
  |> should.equal(200_000.0)

  balance.total_liabilities
  |> should.equal(80_000.0)

  balance.shareholder_equity
  |> should.equal(120_000.0)
}

// Test 6: Request Fundamental Data Message
pub fn request_fundamental_data_test() {
  let message = fundamental_data.request_fundamental_data(1001, 12_345)

  let size = message |> bit_array.byte_size()
  size
  |> should.equal(9)
}

// Test 7: Cancel Fundamental Data Message
pub fn cancel_fundamental_data_test() {
  let message = fundamental_data.cancel_fundamental_data(1001)

  let size = message |> bit_array.byte_size()
  size
  |> should.equal(5)
}

// Test 8: Calculate PE Ratio
pub fn calculate_pe_ratio_test() {
  let result = fundamental_data.calculate_pe_ratio(150.0, 6.0)

  result
  |> should.equal(Ok(25.0))

  let result2 = fundamental_data.calculate_pe_ratio(150.0, 0.0)

  result2
  |> should.equal(Error("EPS cannot be zero"))
}

// Test 9: Calculate Dividend Yield
pub fn calculate_dividend_yield_test() {
  let result = fundamental_data.calculate_dividend_yield(2.0, 100.0)

  result
  |> should.equal(Ok(2.0))

  let result2 = fundamental_data.calculate_dividend_yield(2.0, 0.0)

  result2
  |> should.equal(Error("Price cannot be zero"))
}

// Test 10: Calculate Debt to Equity
pub fn calculate_debt_to_equity_test() {
  let result = fundamental_data.calculate_debt_to_equity(80_000.0, 120_000.0)

  result
  |> should.equal(Ok(0.6666666666666666))

  let result2 = fundamental_data.calculate_debt_to_equity(80_000.0, 0.0)

  result2
  |> should.equal(Error("Equity cannot be zero"))
}

// Test 11: Calculate Current Ratio
pub fn calculate_current_ratio_test() {
  let result = fundamental_data.calculate_current_ratio(150_000.0, 60_000.0)

  result
  |> should.equal(Ok(2.5))

  let result2 = fundamental_data.calculate_current_ratio(150_000.0, 0.0)

  result2
  |> should.equal(Error("Current liabilities cannot be zero"))
}

// Test 12: Filter Ratios by Name Pattern
pub fn filter_ratios_by_name_test() {
  let ratios = [
    fundamental_data.FinancialRatio(
      name: "PE Ratio",
      value: 25.5,
      description: "Price to Earnings",
    ),
    fundamental_data.FinancialRatio(
      name: "PB Ratio",
      value: 5.2,
      description: "Price to Book",
    ),
    fundamental_data.FinancialRatio(
      name: "PEG Ratio",
      value: 1.2,
      description: "PE to Growth",
    ),
  ]

  let filtered = fundamental_data.filter_ratios_by_name(ratios, "PE")

  list.length(filtered)
  |> should.equal(2)

  filtered
  |> list.first()
  |> should.equal(
    Ok(fundamental_data.FinancialRatio(
      name: "PE Ratio",
      value: 25.5,
      description: "Price to Earnings",
    )),
  )
}

// Test 13: Get Ratio by Name
pub fn get_ratio_by_name_test() {
  let ratios = [
    fundamental_data.FinancialRatio(
      name: "PE Ratio",
      value: 25.5,
      description: "Price to Earnings",
    ),
    fundamental_data.FinancialRatio(
      name: "PB Ratio",
      value: 5.2,
      description: "Price to Book",
    ),
  ]

  let result = fundamental_data.get_ratio_by_name(ratios, "PB Ratio")

  result
  |> should.equal(
    Ok(fundamental_data.FinancialRatio(
      name: "PB Ratio",
      value: 5.2,
      description: "Price to Book",
    )),
  )

  let result2 = fundamental_data.get_ratio_by_name(ratios, "ROE")

  result2
  |> should.equal(Error("Ratio not found: ROE"))
}

// Test 14: Format Financial Ratio
pub fn format_financial_ratio_test() {
  let ratio =
    fundamental_data.FinancialRatio(
      name: "PE Ratio",
      value: 25.5,
      description: "Price to Earnings ratio",
    )

  let formatted = fundamental_data.format_financial_ratio(ratio)

  formatted
  |> string.contains("PE Ratio")
  |> should.equal(True)

  formatted
  |> string.contains("25.5")
  |> should.equal(True)

  formatted
  |> string.contains("Price to Earnings ratio")
  |> should.equal(True)
}

// Test 15: Format Earnings Data
pub fn format_earnings_data_test() {
  let earnings =
    fundamental_data.EarningsData(
      eps: 5.5,
      pe_ratio: 25.5,
      peg_ratio: 1.2,
      dividend_yield: 1.5,
      payout_ratio: 30.0,
    )

  let formatted = fundamental_data.format_earnings_data(earnings)

  formatted
  |> string.contains("EPS: 5.5")
  |> should.equal(True)

  formatted
  |> string.contains("PE Ratio: 25.5")
  |> should.equal(True)

  formatted
  |> string.contains("Dividend Yield: 1.5%")
  |> should.equal(True)
}

// Test 16: Format Revenue Data
pub fn format_revenue_data_test() {
  let revenue =
    fundamental_data.RevenueData(
      total_revenue: 100_000.0,
      revenue_growth: 15.5,
      revenue_per_share: 50.0,
      revenue_margin: 25.0,
    )

  let formatted = fundamental_data.format_revenue_data(revenue)

  formatted
  |> string.contains("Total Revenue: 100000.0")
  |> should.equal(True)

  formatted
  |> string.contains("Revenue Growth: 15.5% YoY")
  |> should.equal(True)

  formatted
  |> string.contains("Revenue Margin: 25.0%")
  |> should.equal(True)
}

// Test 17: Format Balance Sheet Data
pub fn format_balance_sheet_data_test() {
  let balance =
    fundamental_data.BalanceSheetData(
      total_assets: 200_000.0,
      total_liabilities: 80_000.0,
      shareholder_equity: 120_000.0,
      debt_to_equity: 0.67,
      current_ratio: 2.5,
    )

  let formatted = fundamental_data.format_balance_sheet_data(balance)

  formatted
  |> string.contains("Total Assets: 200000.0")
  |> should.equal(True)

  formatted
  |> string.contains("Total Liabilities: 80000.0")
  |> should.equal(True)

  formatted
  |> string.contains("Current Ratio: 2.5")
  |> should.equal(True)
}

// Test 18: Edge Case - Zero EPS
pub fn edge_case_zero_eps_test() {
  let result = fundamental_data.calculate_pe_ratio(150.0, 0.0)

  result
  |> should.equal(Error("EPS cannot be zero"))
}

// Test 19: Edge Case - Zero Price
pub fn edge_case_zero_price_test() {
  let result = fundamental_data.calculate_dividend_yield(2.0, 0.0)

  result
  |> should.equal(Error("Price cannot be zero"))
}

// Test 20: Edge Case - Zero Equity
pub fn edge_case_zero_equity_test() {
  let result = fundamental_data.calculate_debt_to_equity(80_000.0, 0.0)

  result
  |> should.equal(Error("Equity cannot be zero"))
}
