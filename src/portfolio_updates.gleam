import gleam/bit_array
import gleam/float
import gleam/list
import gleam/string

/// Portfolio updates for real-time position and account tracking
/// Essential for monitoring your trading activity and account status
/// Position data
pub type Position {
  Position(
    /// Account ID
    account: String,
    /// Contract ID
    contract_id: Int,
    /// Symbol
    symbol: String,
    /// Security type
    sec_type: String,
    /// Exchange
    exchange: String,
    /// Currency
    currency: String,
    /// Position size (positive for long, negative for short)
    position: Float,
    /// Average cost price
    avg_cost: Float,
    /// Market value
    market_price: Float,
    /// Market value
    market_value: Float,
    /// Realized profit/loss
    realized_pnl: Float,
    /// Unrealized profit/loss
    unrealized_pnl: Float,
  )
}

/// Account update type
pub type AccountUpdateType {
  /// Account summary update
  AccountSummaryUpdate
  /// Portfolio value update
  PortfolioValueUpdate
  /// Account download complete
  AccountDownloadEnd
}

/// Account value
pub type AccountValue {
  AccountValue(
    /// Account ID
    account: String,
    /// Key (e.g., "NetLiquidation", "TotalCashBalance")
    key: String,
    /// Value
    value: String,
    /// Currency
    currency: String,
  )
}

/// Request portfolio updates
/// Returns: Message bytes to send to IB TWS
pub fn request_portfolio() -> BitArray {
  // Message format for REQ_POSITIONS (MsgCode 61):
  // No additional fields needed

  // Add message code (61 for REQ_POSITIONS)
  <<61>>
}

/// Cancel portfolio updates
/// Returns: Message bytes to send to IB TWS
pub fn cancel_portfolio() -> BitArray {
  // Message format for CANCEL_POSITIONS (MsgCode 62):
  // No additional fields needed

  // Add message code (62 for CANCEL_POSITIONS)
  <<62>>
}

/// Request account updates
/// subscribe: True to subscribe, False to unsubscribe
/// Returns: Message bytes to send to IB TWS
pub fn request_account_updates(subscribe: Bool) -> BitArray {
  // Message format for REQ_ACCOUNT_UPDATES (MsgCode 6):
  // subscribe (int)

  let subscribe_int = case subscribe {
    True -> 1
    False -> 0
  }

  // Add message code (6 for REQ_ACCOUNT_UPDATES)
  let message_code = <<6>>
  let subscribe_field = <<subscribe_int:32>>

  bit_array.concat([message_code, subscribe_field])
}

/// Request account summary
/// req_id: Unique request ID
/// group: Account group (e.g., "All")
/// tags: List of account summary tags to request
/// Returns: Message bytes to send to IB TWS
pub fn request_account_summary(
  req_id: Int,
  group: String,
  tags: List(String),
) -> BitArray {
  // Message format for REQ_ACCOUNT_SUMMARY (MsgCode 63):
  // version (int) + req_id (int) + group + tags

  let version = 1
  let tags_str = string.join(tags, ",")

  let fields = [
    int_to_field(version),
    int_to_field(req_id),
    string_to_field(group),
    string_to_field(tags_str),
  ]

  // Add message code (63 for REQ_ACCOUNT_SUMMARY)
  let message_code = <<63>>

  // Combine fields with null separators
  let body =
    list.map(fields, fn(f) { f })
    |> list.intersperse(<<0>>)
    |> bit_array.concat

  bit_array.concat([message_code, body])
}

/// Cancel account summary
/// req_id: The request ID from the original request
/// Returns: Message bytes to send to IB TWS
pub fn cancel_account_summary(req_id: Int) -> BitArray {
  // Message format for CANCEL_ACCOUNT_SUMMARY (MsgCode 64):
  // req_id (int)

  let fields = [
    int_to_field(req_id),
  ]

  // Add message code (64 for CANCEL_ACCOUNT_SUMMARY)
  let message_code = <<64>>

  // Combine fields with null separators
  let body =
    list.map(fields, fn(f) { f })
    |> list.intersperse(<<0>>)
    |> bit_array.concat

  bit_array.concat([message_code, body])
}

/// Parse position from message data
/// This is a simplified parser - actual implementation would handle all IB TWS message formats
pub fn parse_position(data: BitArray) -> Result(Position, String) {
  // Simplified parsing - in production, this would parse actual IB message format
  // For now, return an error as this is a placeholder
  Error(
    "Position parsing not yet implemented - requires full message handler integration",
  )
}

/// Parse account value from message data
/// This is a simplified parser - actual implementation would handle all IB TWS message formats
pub fn parse_account_value(data: BitArray) -> Result(AccountValue, String) {
  // Simplified parsing - in production, this would parse actual IB message format
  // For now, return an error as this is a placeholder
  Error(
    "Account value parsing not yet implemented - requires full message handler integration",
  )
}

/// Calculate total position value from a list of positions
pub fn calculate_total_position_value(positions: List(Position)) -> Float {
  list.fold(positions, 0.0, fn(acc, pos) { acc +. pos.market_value })
}

/// Calculate total unrealized P&L from a list of positions
pub fn calculate_total_unrealized_pnl(positions: List(Position)) -> Float {
  list.fold(positions, 0.0, fn(acc, pos) { acc +. pos.unrealized_pnl })
}

/// Calculate total realized P&L from a list of positions
pub fn calculate_total_realized_pnl(positions: List(Position)) -> Float {
  list.fold(positions, 0.0, fn(acc, pos) { acc +. pos.realized_pnl })
}

/// Filter positions by symbol
pub fn filter_positions_by_symbol(
  positions: List(Position),
  symbol: String,
) -> List(Position) {
  list.filter(positions, fn(pos) { pos.symbol == symbol })
}

/// Filter positions by account
pub fn filter_positions_by_account(
  positions: List(Position),
  account: String,
) -> List(Position) {
  list.filter(positions, fn(pos) { pos.account == account })
}

/// Get long positions (positive size)
pub fn get_long_positions(positions: List(Position)) -> List(Position) {
  list.filter(positions, fn(pos) { pos.position >. 0.0 })
}

/// Get short positions (negative size)
pub fn get_short_positions(positions: List(Position)) -> List(Position) {
  list.filter(positions, fn(pos) { pos.position <. 0.0 })
}

/// Format position for display
pub fn format_position(position: Position) -> String {
  let position_type = case position.position >. 0.0 {
    True -> "LONG"
    False -> "SHORT"
  }

  position_type
  <> " "
  <> position.symbol
  <> "\n"
  <> "  Account: "
  <> position.account
  <> "\n"
  <> "  Size: "
  <> float.to_string(position.position)
  <> "\n"
  <> "  Avg Cost: "
  <> float.to_string(position.avg_cost)
  <> "\n"
  <> "  Market Price: "
  <> float.to_string(position.market_price)
  <> "\n"
  <> "  Market Value: "
  <> float.to_string(position.market_value)
  <> "\n"
  <> "  Unrealized P&L: "
  <> float.to_string(position.unrealized_pnl)
  <> "\n"
  <> "  Realized P&L: "
  <> float.to_string(position.realized_pnl)
}

/// Format account value for display
pub fn format_account_value(account_value: AccountValue) -> String {
  account_value.key
  <> ": "
  <> account_value.value
  <> " "
  <> account_value.currency
}

/// Create a sample position for testing
pub fn create_sample_position() -> Position {
  Position(
    account: "DU1234567",
    contract_id: 12_345,
    symbol: "AAPL",
    sec_type: "STK",
    exchange: "SMART",
    currency: "USD",
    position: 100.0,
    avg_cost: 150.0,
    market_price: 155.0,
    market_value: 15_500.0,
    realized_pnl: 500.0,
    unrealized_pnl: 500.0,
  )
}

/// Create a sample account value for testing
pub fn create_sample_account_value() -> AccountValue {
  AccountValue(
    account: "DU1234567",
    key: "NetLiquidation",
    value: "100000.00",
    currency: "USD",
  )
}

/// Helper: Convert integer to IB protocol field (big-endian int)
fn int_to_field(n: Int) -> BitArray {
  <<n:32>>
}

/// Helper: Convert string to IB protocol field
fn string_to_field(s: String) -> BitArray {
  bit_array.from_string(s)
}
