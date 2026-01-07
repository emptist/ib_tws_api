import gleam/bit_array
import gleam/float
import gleam/int
import gleam/io
import gleam/string

/// Bar size for real-time bars
pub type BarSize {
  /// 5 seconds
  FiveSeconds
  /// 10 seconds
  TenSeconds
  /// 15 seconds
  FifteenSeconds
  /// 30 seconds
  ThirtySeconds
  /// 1 minute
  OneMinute
  /// 2 minutes
  TwoMinutes
  /// 3 minutes
  ThreeMinutes
  /// 5 minutes
  FiveMinutes
  /// 10 minutes
  TenMinutes
  /// 15 minutes
  FifteenMinutes
  /// 20 minutes
  TwentyMinutes
  /// 30 minutes
  ThirtyMinutes
  /// 1 hour
  OneHour
  /// 2 hours
  TwoHours
  /// 3 hours
  ThreeHours
  /// 4 hours
  FourHours
  /// 8 hours
  EightHours
  /// 1 day
  OneDay
  /// 1 week
  OneWeek
  /// 1 month
  OneMonth
}

/// What data to show in bars
pub type WhatToShow {
  /// Trades
  Trades
  /// Bid
  Bid
  /// Ask
  Ask
  /// Midpoint
  Midpoint
  /// Bid/Ask
  BidAsk
  /// Historical volatility
  HistoricalVolatility
  /// Implied volatility
  ImpliedVolatility
}

/// Real-time bar data
pub type RealTimeBar {
  RealTimeBar(
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

/// Convert BarSize to string for IB TWS API
fn bar_size_to_string(bar_size: BarSize) -> String {
  case bar_size {
    FiveSeconds -> "5 secs"
    TenSeconds -> "10 secs"
    FifteenSeconds -> "15 secs"
    ThirtySeconds -> "30 secs"
    OneMinute -> "1 min"
    TwoMinutes -> "2 mins"
    ThreeMinutes -> "3 mins"
    FiveMinutes -> "5 mins"
    TenMinutes -> "10 mins"
    FifteenMinutes -> "15 mins"
    TwentyMinutes -> "20 mins"
    ThirtyMinutes -> "30 mins"
    OneHour -> "1 hour"
    TwoHours -> "2 hours"
    ThreeHours -> "3 hours"
    FourHours -> "4 hours"
    EightHours -> "8 hours"
    OneDay -> "1 day"
    OneWeek -> "1 week"
    OneMonth -> "1 month"
  }
}

/// Convert WhatToShow to string for IB TWS API
fn what_to_show_to_string(what: WhatToShow) -> String {
  case what {
    Trades -> "TRADES"
    Bid -> "BID"
    Ask -> "ASK"
    Midpoint -> "MIDPOINT"
    BidAsk -> "BID_ASK"
    HistoricalVolatility -> "HISTORICAL_VOLATILITY"
    ImpliedVolatility -> "IMPLIED_VOLATILITY"
  }
}

/// Request real-time bars
/// Message code: 50 (REQ_REAL_TIME_BARS)
///
/// Parameters:
/// - ticker_id: Unique identifier for this request
/// - contract_id: Contract ID from market_data.create_stock_contract
/// - exchange: Exchange name (e.g., "SMART")
/// - symbol: Symbol name (e.g., "NVDA")
/// - sec_type: Security type (e.g., "STK")
/// - currency: Currency (e.g., "USD")
/// - bar_size: Bar size for the data
/// - what_to_show: Type of data to show
/// - use_rth: Use regular trading hours (1 = yes, 0 = no)
///
/// Returns: BitArray containing the real-time bars request message
pub fn request_real_time_bars(
  ticker_id: Int,
  contract_id: Int,
  exchange: String,
  symbol: String,
  sec_type: String,
  currency: String,
  bar_size: BarSize,
  what_to_show: WhatToShow,
  use_rth: Bool,
) -> BitArray {
  let bar_size_str = bar_size_to_string(bar_size)
  let what_str = what_to_show_to_string(what_to_show)

  let exchange_len = string.length(exchange)
  let symbol_len = string.length(symbol)
  let sec_type_len = string.length(sec_type)
  let currency_len = string.length(currency)
  let bar_size_len = string.length(bar_size_str)
  let what_len = string.length(what_str)

  let use_rth_int = case use_rth {
    True -> 1
    False -> 0
  }

  // REQ_REAL_TIME_BARS message format (version 3):
  // Message code: 50 (2 bytes, big-endian)
  // Version: 3 (4 bytes)
  // Ticker ID: (4 bytes)
  // Contract ID: (4 bytes)
  // Exchange length: (1 byte)
  // Exchange: (variable)
  // Symbol length: (1 byte)
  // Symbol: (variable)
  // SecType length: (1 byte)
  // SecType: (variable)
  // Currency length: (1 byte)
  // Currency: (variable)
  // BarSize length: (1 byte)
  // BarSize: (variable)
  // WhatToShow length: (1 byte)
  // WhatToShow: (variable)
  // UseRTH: (1 byte)
  <<
    50:16,
    3:32,
    ticker_id:32,
    contract_id:32,
    exchange_len:8,
    exchange:utf8,
    symbol_len:8,
    symbol:utf8,
    sec_type_len:8,
    sec_type:utf8,
    currency_len:8,
    currency:utf8,
    bar_size_len:8,
    bar_size_str:utf8,
    what_len:8,
    what_str:utf8,
    use_rth_int:8,
  >>
}

/// Cancel real-time bars
/// Message code: 51 (CANCEL_REAL_TIME_BARS)
///
/// Parameters:
/// - ticker_id: The ticker ID of the subscription to cancel
///
/// Returns: BitArray containing the cancel message
pub fn cancel_real_time_bars(ticker_id: Int) -> BitArray {
  // CANCEL_REAL_TIME_BARS message format:
  // Message code: 51 (2 bytes, big-endian)
  // Ticker ID: (4 bytes)
  <<51:16, ticker_id:32>>
}

/// Debug print real-time bar details
pub fn debug_real_time_bar(bar: RealTimeBar) {
  io.println("Real-Time Bar:")
  io.println("  Request ID: " <> int.to_string(bar.req_id))
  io.println("  Time: " <> int.to_string(bar.time))
  io.println("  Open: " <> float.to_string(bar.open))
  io.println("  High: " <> float.to_string(bar.high))
  io.println("  Low: " <> float.to_string(bar.low))
  io.println("  Close: " <> float.to_string(bar.close))
  io.println("  Volume: " <> int.to_string(bar.volume))
  io.println("  WAP: " <> float.to_string(bar.wap))
  io.println("  Count: " <> int.to_string(bar.count))
}

/// Debug print request details
pub fn debug_real_time_bars_request(
  ticker_id: Int,
  contract_id: Int,
  exchange: String,
  symbol: String,
  sec_type: String,
  currency: String,
  bar_size: BarSize,
  what_to_show: WhatToShow,
  use_rth: Bool,
) {
  let msg =
    request_real_time_bars(
      ticker_id,
      contract_id,
      exchange,
      symbol,
      sec_type,
      currency,
      bar_size,
      what_to_show,
      use_rth,
    )

  let size = bit_array.byte_size(msg)

  io.println("=== Real-Time Bars Request ===")
  io.println("Message size: " <> int.to_string(size) <> " bytes")
  io.println("Message code: 50 (REQ_REAL_TIME_BARS)")
  io.println("Ticker ID: " <> int.to_string(ticker_id))
  io.println("Contract ID: " <> int.to_string(contract_id))
  io.println("Exchange: " <> exchange)
  io.println("Symbol: " <> symbol)
  io.println("SecType: " <> sec_type)
  io.println("Currency: " <> currency)
  io.println("Bar Size: " <> bar_size_to_string(bar_size))
  io.println("What to Show: " <> what_to_show_to_string(what_to_show))
  let rth_str = case use_rth {
    True -> "Yes"
    False -> "No"
  }
  io.println("Use RTH: " <> rth_str)
  io.println("")
}
