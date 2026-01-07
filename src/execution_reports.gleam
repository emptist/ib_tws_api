import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/string

/// Execution and Commission Reports Module
/// 
/// This module provides functionality to track order executions and commissions.
/// After placing orders, it's essential to track their execution status
/// and the commissions charged for each trade.
/// Order execution status
pub type ExecutionStatus {
  Pending
  Submitted
  Cancelled
  Filled
  PartiallyFilled
}

/// Execution type
pub type ExecutionType {
  Market
  Limit
  Stop
  StopLimit
  TrailingStop
}

/// Order execution data
pub type Execution {
  Execution(
    /// Order ID
    order_id: Int,
    /// Client ID
    client_id: Int,
    /// Execution ID
    exec_id: Int,
    /// Execution status
    status: ExecutionStatus,
    /// Execution type
    exec_type: ExecutionType,
    /// Symbol
    symbol: String,
    /// Exchange
    exchange: String,
    /// Side (BUY/SELL)
    side: String,
    /// Quantity
    quantity: Float,
    /// Price
    price: Float,
    /// Execution time
    time: String,
    /// Account
    account: String,
    /// Average price
    avg_price: Float,
    /// Last liquidity
    last_liquidity: Int,
  )
}

/// Commission report data
pub type CommissionReport {
  CommissionReport(
    /// Execution ID
    exec_id: Int,
    /// Commission currency
    currency: String,
    /// Commission amount
    commission: Float,
    /// Realized P&L
    realized_pnl: Float,
    /// Yield
    yield: Float,
    /// Yield redem date
    yield_redemption_date: String,
  )
}

/// Request execution reports
pub fn request_executions() -> BitArray {
  // Message format for REQ_EXECUTIONS (MsgCode 7)
  <<7:size(8)>>
}

/// Cancel execution reports
pub fn cancel_executions() -> BitArray {
  // Message format for CANCEL_EXECUTIONS (MsgCode 8)
  <<8:size(8)>>
}

/// Request commission reports
pub fn request_commission_report() -> BitArray {
  // Message format for REQ_COMMISSION_REPORT (MsgCode 9)
  <<9:size(8)>>
}

/// Cancel commission reports
pub fn cancel_commission_report() -> BitArray {
  // Message format for CANCEL_COMMISSION_REPORT (MsgCode 10)
  <<10:size(8)>>
}

/// Analysis Functions
/// Get all executions for a specific order
pub fn get_executions_by_order(
  executions: List(Execution),
  order_id: Int,
) -> List(Execution) {
  list.filter(executions, fn(exec) { exec.order_id == order_id })
}

/// Get all executions for a specific symbol
pub fn get_executions_by_symbol(
  executions: List(Execution),
  symbol: String,
) -> List(Execution) {
  list.filter(executions, fn(exec) { exec.symbol == symbol })
}

/// Get filled executions only
pub fn get_filled_executions(executions: List(Execution)) -> List(Execution) {
  list.filter(executions, fn(exec) {
    case exec.status {
      Filled -> True
      PartiallyFilled -> True
      _ -> False
    }
  })
}

/// Get cancelled executions only
pub fn get_cancelled_executions(executions: List(Execution)) -> List(Execution) {
  list.filter(executions, fn(exec) {
    case exec.status {
      Cancelled -> True
      _ -> False
    }
  })
}

/// Calculate total quantity executed for a symbol
pub fn calculate_total_quantity(
  executions: List(Execution),
  symbol: String,
) -> Float {
  executions
  |> get_executions_by_symbol(symbol)
  |> list.filter(fn(exec) {
    case exec.status {
      Filled -> True
      PartiallyFilled -> True
      _ -> False
    }
  })
  |> list.fold(0.0, fn(acc, exec) { acc +. exec.quantity })
}

/// Calculate average execution price for a symbol
pub fn calculate_average_price(
  executions: List(Execution),
  symbol: String,
) -> Result(Float, String) {
  let symbol_executions =
    executions
    |> get_executions_by_symbol(symbol)
    |> list.filter(fn(exec) {
      case exec.status {
        Filled -> True
        PartiallyFilled -> True
        _ -> False
      }
    })

  case list.length(symbol_executions) {
    0 -> Error("No filled executions found for symbol")
    _ -> {
      let total_value =
        list.fold(symbol_executions, 0.0, fn(acc, exec) {
          acc +. exec.quantity *. exec.price
        })

      let total_qty =
        list.fold(symbol_executions, 0.0, fn(acc, exec) { acc +. exec.quantity })

      Ok(total_value /. total_qty)
    }
  }
}

/// Calculate total commissions for a symbol
pub fn calculate_total_commissions(
  reports: List(CommissionReport),
  currency: String,
) -> Float {
  list.filter(reports, fn(report) { report.currency == currency })
  |> list.fold(0.0, fn(acc, report) { acc +. report.commission })
}

/// Calculate total realized P&L for a symbol
pub fn calculate_total_realized_pnl(reports: List(CommissionReport)) -> Float {
  list.fold(reports, 0.0, fn(acc, report) { acc +. report.realized_pnl })
}

/// Formatting Functions
/// Format execution status for display
pub fn format_execution_status(status: ExecutionStatus) -> String {
  case status {
    Pending -> "PENDING"
    Submitted -> "SUBMITTED"
    Cancelled -> "CANCELLED"
    Filled -> "FILLED"
    PartiallyFilled -> "PARTIALLY FILLED"
  }
}

/// Format execution type for display
pub fn format_execution_type(exec_type: ExecutionType) -> String {
  case exec_type {
    Market -> "MARKET"
    Limit -> "LIMIT"
    Stop -> "STOP"
    StopLimit -> "STOP LIMIT"
    TrailingStop -> "TRAILING STOP"
  }
}

/// Format execution for display
pub fn format_execution(exec: Execution) -> String {
  let status_str = format_execution_status(exec.status)
  let type_str = format_execution_type(exec.exec_type)

  "
Execution ID: " <> int.to_string(exec.exec_id) <> "
Order ID: " <> int.to_string(exec.order_id) <> "
Symbol: " <> exec.symbol <> "
Exchange: " <> exec.exchange <> "
Side: " <> exec.side <> "
Quantity: " <> float.to_string(exec.quantity) <> "
Price: " <> float.to_string(exec.price) <> "
Avg Price: " <> float.to_string(exec.avg_price) <> "
Status: " <> status_str <> "
Type: " <> type_str <> "
Time: " <> exec.time <> "
Account: " <> exec.account
}

/// Format execution as CSV
pub fn format_execution_csv(exec: Execution) -> String {
  let status_str = format_execution_status(exec.status)
  let type_str = format_execution_type(exec.exec_type)

  int.to_string(exec.exec_id)
  <> ","
  <> int.to_string(exec.order_id)
  <> ","
  <> exec.symbol
  <> ","
  <> exec.exchange
  <> ","
  <> exec.side
  <> ","
  <> float.to_string(exec.quantity)
  <> ","
  <> float.to_string(exec.price)
  <> ","
  <> float.to_string(exec.avg_price)
  <> ","
  <> status_str
  <> ","
  <> type_str
  <> ","
  <> exec.time
  <> ","
  <> exec.account
}

/// Format commission report for display
pub fn format_commission_report(report: CommissionReport) -> String {
  "
Commission Report
  Execution ID: " <> int.to_string(report.exec_id) <> "
  Currency: " <> report.currency <> "
  Commission: " <> float.to_string(report.commission) <> "
  Realized P&L: " <> float.to_string(report.realized_pnl) <> "
  Yield: " <> float.to_string(report.yield) <> "%"
}

/// Parse execution (placeholder - to be implemented in message_handler)
pub fn parse_execution(data: BitArray) -> Result(Execution, String) {
  // This will be implemented when parsing actual execution data
  Error("Not yet implemented - will be added to message_handler")
}

/// Parse commission report (placeholder - to be implemented in message_handler)
pub fn parse_commission_report(
  data: BitArray,
) -> Result(CommissionReport, String) {
  // This will be implemented when parsing actual commission data
  Error("Not yet implemented - will be added to message_handler")
}
