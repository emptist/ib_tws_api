import gleam/bit_array
import gleam/float
import gleam/list
import gleam/string

/// Market Depth (Level 2) data for order book analysis
/// Provides bid/ask depth with price and size information
/// Market depth operation type
pub type MarketDepthOperation {
  /// Insert new price level
  Insert
  /// Update existing price level
  Update
  /// Remove price level
  Delete
}

/// Market depth row - represents a single price level in the order book
pub type MarketDepthRow {
  MarketDepthRow(
    price: Float,
    size: Float,
    /// Market maker ID (if available)
    market_maker: String,
    /// True for bid, False for ask
    is_bid: Bool,
  )
}

/// Market depth request configuration
pub type MarketDepthConfig {
  MarketDepthConfig(
    /// Number of rows to request (usually 5-20)
    num_rows: Int,
    /// True for smart depth (aggregated), False for regular
    is_smart_depth: Bool,
  )
}

/// Create default market depth configuration
pub fn default_market_depth_config() -> MarketDepthConfig {
  MarketDepthConfig(num_rows: 5, is_smart_depth: True)
}

/// Create market depth configuration with custom parameters
pub fn market_depth_config(
  num_rows: Int,
  is_smart_depth: Bool,
) -> MarketDepthConfig {
  MarketDepthConfig(num_rows: num_rows, is_smart_depth: is_smart_depth)
}

/// Request market depth data (Level 2)
/// ticker_id: Unique ID for the request (used to track responses)
/// contract_id: Contract ID to request market depth for
/// config: Market depth configuration
/// Returns: Message bytes to send to IB TWS
pub fn request_market_depth(
  ticker_id: Int,
  contract_id: Int,
  config: MarketDepthConfig,
) -> BitArray {
  // Message format for REQ_MKT_DEPTH (MsgCode 10):
  // version (int) + ticker_id (int) + contract_id (int) + num_rows (int) + is_smart_depth (int)

  let version = 5
  let is_smart_depth_int = case config.is_smart_depth {
    True -> 1
    False -> 0
  }

  // Build message using IB protocol format
  let fields = [
    int_to_field(version),
    int_to_field(ticker_id),
    int_to_field(contract_id),
    int_to_field(config.num_rows),
    int_to_field(is_smart_depth_int),
  ]

  // Add message code (10 for REQ_MKT_DEPTH)
  let message_code = <<10>>

  // Combine fields with null separators
  let body =
    list.map(fields, fn(f) { f })
    |> list.intersperse(<<0>>)
    |> bit_array.concat

  bit_array.concat([message_code, body])
}

/// Cancel market depth data request
/// ticker_id: The ticker ID from the original request
/// Returns: Message bytes to send to IB TWS
pub fn cancel_market_depth(ticker_id: Int) -> BitArray {
  // Message format for CANCEL_MKT_DEPTH (MsgCode 11):
  // ticker_id (int)

  let fields = [
    int_to_field(ticker_id),
  ]

  // Add message code (11 for CANCEL_MKT_DEPTH)
  let message_code = <<11>>

  // Combine fields with null separators
  let body =
    list.map(fields, fn(f) { f })
    |> list.intersperse(<<0>>)
    |> bit_array.concat

  bit_array.concat([message_code, body])
}

/// Parse market depth row from message data
/// This is a simplified parser - actual implementation would handle all IB TWS message formats
pub fn parse_market_depth_row(data: BitArray) -> Result(MarketDepthRow, String) {
  // Simplified parsing - in production, this would parse the actual IB message format
  // For now, return an error as this is a placeholder
  Error(
    "Market depth parsing not yet implemented - requires full message handler integration",
  )
}

/// Convert market depth operation to string
pub fn market_depth_operation_to_string(op: MarketDepthOperation) -> String {
  case op {
    Insert -> "0"
    Update -> "1"
    Delete -> "2"
  }
}

/// Parse market depth operation from string
pub fn parse_market_depth_operation(
  s: String,
) -> Result(MarketDepthOperation, String) {
  case s {
    "0" -> Ok(Insert)
    "1" -> Ok(Update)
    "2" -> Ok(Delete)
    _ -> Error("Invalid market depth operation: " <> s)
  }
}

/// Helper: Convert integer to IB protocol field (big-endian int)
fn int_to_field(n: Int) -> BitArray {
  <<n:32>>
}

/// Helper: Convert float to IB protocol field (big-endian double)
fn float_to_field(f: Float) -> BitArray {
  <<f:64-float>>
}

/// Create a market depth row for testing
pub fn create_market_depth_row(
  price: Float,
  size: Float,
  market_maker: String,
  is_bid: Bool,
) -> MarketDepthRow {
  MarketDepthRow(
    price: price,
    size: size,
    market_maker: market_maker,
    is_bid: is_bid,
  )
}

/// Calculate total liquidity from a list of market depth rows
pub fn calculate_total_liquidity(rows: List(MarketDepthRow)) -> Float {
  list.fold(rows, 0.0, fn(acc, row) { acc +. row.size })
}

/// Calculate weighted average price from a list of market depth rows
pub fn calculate_weighted_avg_price(rows: List(MarketDepthRow)) -> Float {
  let total_size = calculate_total_liquidity(rows)
  case total_size {
    0.0 -> 0.0
    _ -> {
      let total_value =
        list.fold(rows, 0.0, fn(acc, row) { acc +. row.size *. row.price })
      total_value /. total_size
    }
  }
}

/// Get best bid price from a list of market depth rows
pub fn get_best_bid(rows: List(MarketDepthRow)) -> Result(Float, String) {
  let bid_rows = list.filter(rows, fn(r) { r.is_bid })

  case bid_rows {
    [] -> Error("No bid prices available")
    _ -> {
      let best_bid =
        list.fold(bid_rows, 0.0, fn(acc, row) {
          case acc >. row.price {
            True -> acc
            False -> row.price
          }
        })
      Ok(best_bid)
    }
  }
}

/// Get best ask price from a list of market depth rows
pub fn get_best_ask(rows: List(MarketDepthRow)) -> Result(Float, String) {
  let ask_rows = list.filter(rows, fn(r) { !r.is_bid })

  case ask_rows {
    [] -> Error("No ask prices available")
    _ -> {
      let best_ask =
        list.fold(ask_rows, 0.0, fn(acc, row) {
          case acc <. row.price || acc == 0.0 {
            True -> row.price
            False -> acc
          }
        })
      Ok(best_ask)
    }
  }
}

/// Calculate spread between best bid and ask
pub fn calculate_spread(rows: List(MarketDepthRow)) -> Result(Float, String) {
  case get_best_bid(rows), get_best_ask(rows) {
    Ok(best_bid), Ok(best_ask) -> Ok(best_ask -. best_bid)
    _, _ -> Error("Cannot calculate spread - missing bid or ask")
  }
}

/// Format market depth row for display
pub fn format_market_depth_row(row: MarketDepthRow) -> String {
  let side = case row.is_bid {
    True -> "BID"
    False -> "ASK"
  }
  side
  <> " Price: "
  <> float.to_string(row.price)
  <> " Size: "
  <> float.to_string(row.size)
  <> " MM: "
  <> row.market_maker
}
