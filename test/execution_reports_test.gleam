import execution_reports
import gleam/bit_array
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Test 1: Execution Status
pub fn execution_status_test() {
  let pending = execution_reports.Pending
  let submitted = execution_reports.Submitted
  let cancelled = execution_reports.Cancelled
  let filled = execution_reports.Filled
  let partially_filled = execution_reports.PartiallyFilled

  execution_reports.format_execution_status(pending)
  |> should.equal("PENDING")

  execution_reports.format_execution_status(submitted)
  |> should.equal("SUBMITTED")

  execution_reports.format_execution_status(cancelled)
  |> should.equal("CANCELLED")

  execution_reports.format_execution_status(filled)
  |> should.equal("FILLED")

  execution_reports.format_execution_status(partially_filled)
  |> should.equal("PARTIALLY FILLED")
}

// Test 2: Execution Type
pub fn execution_type_test() {
  let market = execution_reports.Market
  let limit = execution_reports.Limit
  let stop = execution_reports.Stop
  let stop_limit = execution_reports.StopLimit
  let trailing_stop = execution_reports.TrailingStop

  execution_reports.format_execution_type(market)
  |> should.equal("MARKET")

  execution_reports.format_execution_type(limit)
  |> should.equal("LIMIT")

  execution_reports.format_execution_type(stop)
  |> should.equal("STOP")

  execution_reports.format_execution_type(stop_limit)
  |> should.equal("STOP LIMIT")

  execution_reports.format_execution_type(trailing_stop)
  |> should.equal("TRAILING STOP")
}

// Test 3: Execution Creation
pub fn execution_creation_test() {
  let execution =
    execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5001,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 100.0,
      price: 150.5,
      time: "2024-01-07 10:30:00",
      account: "DU12345",
      avg_price: 150.5,
      last_liquidity: 100,
    )

  execution.order_id
  |> should.equal(1001)

  execution.symbol
  |> should.equal("AAPL")

  execution.quantity
  |> should.equal(100.0)

  execution.price
  |> should.equal(150.5)
}

// Test 4: Commission Report Creation
pub fn commission_report_creation_test() {
  let report =
    execution_reports.CommissionReport(
      exec_id: 5001,
      currency: "USD",
      commission: 1.5,
      realized_pnl: 0.0,
      yield: 0.0,
      yield_redemption_date: "",
    )

  report.exec_id
  |> should.equal(5001)

  report.currency
  |> should.equal("USD")

  report.commission
  |> should.equal(1.5)

  report.realized_pnl
  |> should.equal(0.0)
}

// Test 5: Filter Executions by Order ID
pub fn filter_by_order_id_test() {
  let executions = [
    execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5001,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 100.0,
      price: 150.5,
      time: "2024-01-07 10:30:00",
      account: "DU12345",
      avg_price: 150.5,
      last_liquidity: 100,
    ),
    execution_reports.Execution(
      order_id: 1002,
      client_id: 1,
      exec_id: 5002,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "GOOGL",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 140.0,
      time: "2024-01-07 11:00:00",
      account: "DU12345",
      avg_price: 140.0,
      last_liquidity: 50,
    ),
    execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5003,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 151.0,
      time: "2024-01-07 11:30:00",
      account: "DU12345",
      avg_price: 151.0,
      last_liquidity: 50,
    ),
  ]

  let filtered = execution_reports.get_executions_by_order(executions, 1001)

  list.length(filtered)
  |> should.equal(2)

  filtered
  |> list.first()
  |> should.equal(
    Ok(execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5001,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 100.0,
      price: 150.5,
      time: "2024-01-07 10:30:00",
      account: "DU12345",
      avg_price: 150.5,
      last_liquidity: 100,
    )),
  )
}

// Test 6: Filter Executions by Symbol
pub fn filter_by_symbol_test() {
  let executions = [
    execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5001,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 100.0,
      price: 150.5,
      time: "2024-01-07 10:30:00",
      account: "DU12345",
      avg_price: 150.5,
      last_liquidity: 100,
    ),
    execution_reports.Execution(
      order_id: 1002,
      client_id: 1,
      exec_id: 5002,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "GOOGL",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 140.0,
      time: "2024-01-07 11:00:00",
      account: "DU12345",
      avg_price: 140.0,
      last_liquidity: 50,
    ),
    execution_reports.Execution(
      order_id: 1003,
      client_id: 1,
      exec_id: 5003,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 151.0,
      time: "2024-01-07 11:30:00",
      account: "DU12345",
      avg_price: 151.0,
      last_liquidity: 50,
    ),
  ]

  let filtered = execution_reports.get_executions_by_symbol(executions, "AAPL")

  list.length(filtered)
  |> should.equal(2)

  filtered
  |> list.first()
  |> should.equal(
    Ok(execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5001,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 100.0,
      price: 150.5,
      time: "2024-01-07 10:30:00",
      account: "DU12345",
      avg_price: 150.5,
      last_liquidity: 100,
    )),
  )
}

// Test 7: Get Filled Executions
pub fn get_filled_executions_test() {
  let executions = [
    execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5001,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 100.0,
      price: 150.5,
      time: "2024-01-07 10:30:00",
      account: "DU12345",
      avg_price: 150.5,
      last_liquidity: 100,
    ),
    execution_reports.Execution(
      order_id: 1002,
      client_id: 1,
      exec_id: 5002,
      status: execution_reports.Submitted,
      exec_type: execution_reports.Market,
      symbol: "GOOGL",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 140.0,
      time: "2024-01-07 11:00:00",
      account: "DU12345",
      avg_price: 140.0,
      last_liquidity: 50,
    ),
    execution_reports.Execution(
      order_id: 1003,
      client_id: 1,
      exec_id: 5003,
      status: execution_reports.PartiallyFilled,
      exec_type: execution_reports.Market,
      symbol: "MSFT",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 300.0,
      time: "2024-01-07 11:30:00",
      account: "DU12345",
      avg_price: 300.0,
      last_liquidity: 50,
    ),
  ]

  let filled = execution_reports.get_filled_executions(executions)

  list.length(filled)
  |> should.equal(2)
}

// Test 8: Get Cancelled Executions
pub fn get_cancelled_executions_test() {
  let executions = [
    execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5001,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 100.0,
      price: 150.5,
      time: "2024-01-07 10:30:00",
      account: "DU12345",
      avg_price: 150.5,
      last_liquidity: 100,
    ),
    execution_reports.Execution(
      order_id: 1002,
      client_id: 1,
      exec_id: 5002,
      status: execution_reports.Cancelled,
      exec_type: execution_reports.Market,
      symbol: "GOOGL",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 140.0,
      time: "2024-01-07 11:00:00",
      account: "DU12345",
      avg_price: 140.0,
      last_liquidity: 50,
    ),
    execution_reports.Execution(
      order_id: 1003,
      client_id: 1,
      exec_id: 5003,
      status: execution_reports.Cancelled,
      exec_type: execution_reports.Market,
      symbol: "MSFT",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 300.0,
      time: "2024-01-07 11:30:00",
      account: "DU12345",
      avg_price: 300.0,
      last_liquidity: 50,
    ),
  ]

  let cancelled = execution_reports.get_cancelled_executions(executions)

  list.length(cancelled)
  |> should.equal(2)
}

// Test 9: Calculate Total Quantity
pub fn calculate_total_quantity_test() {
  let executions = [
    execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5001,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 100.0,
      price: 150.5,
      time: "2024-01-07 10:30:00",
      account: "DU12345",
      avg_price: 150.5,
      last_liquidity: 100,
    ),
    execution_reports.Execution(
      order_id: 1002,
      client_id: 1,
      exec_id: 5002,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "GOOGL",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 140.0,
      time: "2024-01-07 11:00:00",
      account: "DU12345",
      avg_price: 140.0,
      last_liquidity: 50,
    ),
    execution_reports.Execution(
      order_id: 1003,
      client_id: 1,
      exec_id: 5003,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 151.0,
      time: "2024-01-07 11:30:00",
      account: "DU12345",
      avg_price: 151.0,
      last_liquidity: 50,
    ),
  ]

  let total_qty = execution_reports.calculate_total_quantity(executions, "AAPL")

  total_qty
  |> should.equal(150.0)
}

// Test 10: Calculate Average Price
pub fn calculate_average_price_test() {
  let executions = [
    execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5001,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 100.0,
      price: 150.5,
      time: "2024-01-07 10:30:00",
      account: "DU12345",
      avg_price: 150.5,
      last_liquidity: 100,
    ),
    execution_reports.Execution(
      order_id: 1003,
      client_id: 1,
      exec_id: 5003,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 151.0,
      time: "2024-01-07 11:30:00",
      account: "DU12345",
      avg_price: 151.0,
      last_liquidity: 50,
    ),
  ]

  let avg_price = execution_reports.calculate_average_price(executions, "AAPL")

  avg_price
  |> should.equal(Ok(150.66666666666666))
}

// Test 11: Calculate Average Price with No Executions
pub fn calculate_average_price_no_executions_test() {
  let executions = [
    execution_reports.Execution(
      order_id: 1002,
      client_id: 1,
      exec_id: 5002,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "GOOGL",
      exchange: "SMART",
      side: "BUY",
      quantity: 50.0,
      price: 140.0,
      time: "2024-01-07 11:00:00",
      account: "DU12345",
      avg_price: 140.0,
      last_liquidity: 50,
    ),
  ]

  let avg_price = execution_reports.calculate_average_price(executions, "AAPL")

  avg_price
  |> should.equal(Error("No filled executions found for symbol"))
}

// Test 12: Calculate Total Commissions
pub fn calculate_total_commissions_test() {
  let reports = [
    execution_reports.CommissionReport(
      exec_id: 5001,
      currency: "USD",
      commission: 1.5,
      realized_pnl: 0.0,
      yield: 0.0,
      yield_redemption_date: "",
    ),
    execution_reports.CommissionReport(
      exec_id: 5002,
      currency: "USD",
      commission: 2.0,
      realized_pnl: 0.0,
      yield: 0.0,
      yield_redemption_date: "",
    ),
    execution_reports.CommissionReport(
      exec_id: 5003,
      currency: "EUR",
      commission: 1.0,
      realized_pnl: 0.0,
      yield: 0.0,
      yield_redemption_date: "",
    ),
  ]

  let total_usd = execution_reports.calculate_total_commissions(reports, "USD")

  total_usd
  |> should.equal(3.5)

  let total_eur = execution_reports.calculate_total_commissions(reports, "EUR")

  total_eur
  |> should.equal(1.0)
}

// Test 13: Calculate Total Realized P&L
pub fn calculate_total_realized_pnl_test() {
  let reports = [
    execution_reports.CommissionReport(
      exec_id: 5001,
      currency: "USD",
      commission: 1.5,
      realized_pnl: 100.5,
      yield: 0.0,
      yield_redemption_date: "",
    ),
    execution_reports.CommissionReport(
      exec_id: 5002,
      currency: "USD",
      commission: 2.0,
      realized_pnl: -50.25,
      yield: 0.0,
      yield_redemption_date: "",
    ),
  ]

  let total_pnl = execution_reports.calculate_total_realized_pnl(reports)

  total_pnl
  |> should.equal(50.25)
}

// Test 14: Format Execution
pub fn format_execution_test() {
  let execution =
    execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5001,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 100.0,
      price: 150.5,
      time: "2024-01-07 10:30:00",
      account: "DU12345",
      avg_price: 150.5,
      last_liquidity: 100,
    )

  let formatted = execution_reports.format_execution(execution)

  formatted
  |> string.contains("Execution ID: 5001")
  |> should.equal(True)

  formatted
  |> string.contains("Order ID: 1001")
  |> should.equal(True)

  formatted
  |> string.contains("Symbol: AAPL")
  |> should.equal(True)

  formatted
  |> string.contains("Status: FILLED")
  |> should.equal(True)
}

// Test 15: Format Execution CSV
pub fn format_execution_csv_test() {
  let execution =
    execution_reports.Execution(
      order_id: 1001,
      client_id: 1,
      exec_id: 5001,
      status: execution_reports.Filled,
      exec_type: execution_reports.Market,
      symbol: "AAPL",
      exchange: "SMART",
      side: "BUY",
      quantity: 100.0,
      price: 150.5,
      time: "2024-01-07 10:30:00",
      account: "DU12345",
      avg_price: 150.5,
      last_liquidity: 100,
    )

  let csv = execution_reports.format_execution_csv(execution)

  csv
  |> string.contains("5001")
  |> should.equal(True)

  csv
  |> string.contains("1001")
  |> should.equal(True)

  csv
  |> string.contains("AAPL")
  |> should.equal(True)
}

// Test 16: Format Commission Report
pub fn format_commission_report_test() {
  let report =
    execution_reports.CommissionReport(
      exec_id: 5001,
      currency: "USD",
      commission: 1.5,
      realized_pnl: 100.5,
      yield: 5.5,
      yield_redemption_date: "",
    )

  let formatted = execution_reports.format_commission_report(report)

  formatted
  |> string.contains("Execution ID: 5001")
  |> should.equal(True)

  formatted
  |> string.contains("Currency: USD")
  |> should.equal(True)

  formatted
  |> string.contains("Commission: 1.5")
  |> should.equal(True)

  formatted
  |> string.contains("Realized P&L: 100.5")
  |> should.equal(True)
}

// Test 17: Request Executions Message
pub fn request_executions_test() {
  let message = execution_reports.request_executions()

  message
  |> bit_array.byte_size()
  |> should.equal(1)
}

// Test 18: Cancel Executions Message
pub fn cancel_executions_test() {
  let message = execution_reports.cancel_executions()

  message
  |> bit_array.byte_size()
  |> should.equal(1)
}

// Test 19: Request Commission Report Message
pub fn request_commission_report_test() {
  let message = execution_reports.request_commission_report()

  message
  |> bit_array.byte_size()
  |> should.equal(1)
}

// Test 20: Cancel Commission Report Message
pub fn cancel_commission_report_test() {
  let message = execution_reports.cancel_commission_report()

  message
  |> bit_array.byte_size()
  |> should.equal(1)
}
