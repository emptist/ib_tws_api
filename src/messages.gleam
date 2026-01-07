import gleam/bit_array
import gleam/int
import gleam/io

/// IB TWS API Message Codes
/// These are the first 2 bytes of every message (big-endian)
pub type MessageCode {
  /// Error message
  ErrorMessageCode
  /// Tick price (bid/ask/last)
  TickPriceCode
  /// Tick size
  TickSizeCode
  /// Order status
  OrderStatusCode
  /// Position data
  PositionCode
  /// Account summary
  AccountSummaryCode
}

/// Error message from IB TWS
pub type ErrorData {
  ErrorData(error_id: Int, error_code: Int, error_message: String)
}

/// Tick price message
pub type TickPriceData {
  TickPriceData(ticker_id: Int, tick_type: Int, price: Float, size: Int)
}

/// Tick size message
pub type TickSizeData {
  TickSizeData(ticker_id: Int, tick_type: Int, size: Int)
}

/// Order status message
pub type OrderStatusData {
  OrderStatusData(
    order_id: Int,
    status: String,
    filled: Int,
    remaining: Int,
    avg_fill_price: Float,
  )
}

/// Position message
pub type PositionData {
  PositionData(
    account: String,
    contract_id: Int,
    symbol: String,
    position: Float,
    avg_cost: Float,
  )
}

/// Account summary message
pub type AccountSummaryData {
  AccountSummaryData(
    account_id: String,
    tag: String,
    value: String,
    currency: String,
  )
}

/// Real-time bar message
pub type RealTimeBarData {
  RealTimeBarData(
    req_id: Int,
    time: Int,
    open: Float,
    high: Float,
    low: Float,
    close: Float,
    volume: Int,
    wap: Float,
    count: Int,
  )
}

/// Historical bar message
pub type HistoricalBarData {
  HistoricalBarData(
    req_id: Int,
    date: String,
    open: Float,
    high: Float,
    low: Float,
    close: Float,
    volume: Int,
    count: Int,
    wap: Float,
  )
}

/// Parsed IB TWS message
pub type Message {
  ErrorMsg(ErrorData)
  TickPrice(TickPriceData)
  TickSize(TickSizeData)
  OrderStatus(OrderStatusData)
  Position(PositionData)
  AccountSummary(AccountSummaryData)
  RealTimeBar(RealTimeBarData)
  HistoricalBar(HistoricalBarData)
  Unknown(String)
}

/// Parse a complete message from received data
/// Returns parsed message or error
/// Note: This is a simplified parser that will be expanded as needed
pub fn parse_message(data: String) -> Result(Message, String) {
  let bytes = bit_array.from_string(data)
  let size = bit_array.byte_size(bytes)

  // Check minimum size for message code
  case size < 2 {
    True -> Error("Message too short for code")
    False -> {
      case bytes {
        <<code:16, _rest:bytes>> -> {
          io.println("[DEBUG] Message code: " <> int.to_string(code))

          case code {
            4 -> {
              io.println("[DEBUG] Parsing error message")
              Ok(
                ErrorMsg(ErrorData(
                  error_id: 0,
                  error_code: 0,
                  error_message: data,
                )),
              )
            }
            1 -> {
              io.println("[DEBUG] Parsing tick price message")
              Ok(
                TickPrice(TickPriceData(
                  ticker_id: 0,
                  tick_type: 0,
                  price: 0.0,
                  size: 0,
                )),
              )
            }
            2 -> {
              io.println("[DEBUG] Parsing tick size message")
              Ok(TickSize(TickSizeData(ticker_id: 0, tick_type: 0, size: 0)))
            }
            9 -> {
              io.println("[DEBUG] Parsing order status message")
              Ok(
                OrderStatus(OrderStatusData(
                  order_id: 0,
                  status: "Unknown",
                  filled: 0,
                  remaining: 0,
                  avg_fill_price: 0.0,
                )),
              )
            }
            61 -> {
              io.println("[DEBUG] Parsing position message")
              Ok(
                Position(PositionData(
                  account: "Unknown",
                  contract_id: 0,
                  symbol: "Unknown",
                  position: 0.0,
                  avg_cost: 0.0,
                )),
              )
            }
            _other -> {
              io.println(
                "[DEBUG] Unknown message code: " <> int.to_string(code),
              )
              Ok(Unknown(data))
            }
          }
        }
        _other -> {
          io.println("[DEBUG] Invalid byte format")
          Ok(Unknown(data))
        }
      }
    }
  }
}
