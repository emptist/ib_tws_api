import gleam/bit_array
import gleam/string

/// Historical data bar sizes
pub type BarSize {
  OneSecond
  FiveSeconds
  TenSeconds
  FifteenSeconds
  ThirtySeconds
  OneMinute
  TwoMinutes
  ThreeMinutes
  FiveMinutes
  TenMinutes
  FifteenMinutes
  TwentyMinutes
  ThirtyMinutes
  OneHour
  TwoHours
  ThreeHours
  FourHours
  EightHours
  OneDay
  OneWeek
  OneMonth
}

/// Historical data types (what to show)
pub type WhatToShow {
  Trades
  Midpoint
  Bid
  Ask
  BidAsk
  HistoricalVolatility
  OptionImpliedVolatility
  FeeRate
}

/// Historical bar data received from TWS
pub type HistoricalBar {
  HistoricalBar(
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

/// Convert BarSize to string for IB TWS protocol
pub fn bar_size_to_string(bar_size: BarSize) -> String {
  case bar_size {
    OneSecond -> "1 sec"
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

/// Convert WhatToShow to string for IB TWS protocol
pub fn what_to_show_to_string(what: WhatToShow) -> String {
  case what {
    Trades -> "TRADES"
    Midpoint -> "MIDPOINT"
    Bid -> "BID"
    Ask -> "ASK"
    BidAsk -> "BID_ASK"
    HistoricalVolatility -> "HISTORICAL_VOLATILITY"
    OptionImpliedVolatility -> "OPTION_IMPLIED_VOLATILITY"
    FeeRate -> "FEE_RATE"
  }
}

/// Create historical data request message
/// Message code: 20 (REQ_HISTORICAL_DATA)
/// Protocol version: 6
pub fn request_historical_data(
  ticker_id: Int,
  contract_id: Int,
  exchange: String,
  symbol: String,
  sec_type: String,
  currency: String,
  end_date_time: String,
  // Format: YYYYMMDD HH:MM:SS
  duration_str: String,
  // Format: S (seconds), D (days), W (weeks), M (months), Y (years)
  bar_size: BarSize,
  what_to_show: WhatToShow,
  use_rth: Bool,
  format_date: Int,
  // 1 = yyyymmdd HH:mm:ss, 2 = seconds since epoch
) -> BitArray {
  let version = 6
  let bar_size_str = bar_size_to_string(bar_size)
  let what_str = what_to_show_to_string(what_to_show)
  let exchange_len = string.length(exchange)
  let symbol_len = string.length(symbol)
  let sec_type_len = string.length(sec_type)
  let currency_len = string.length(currency)
  let end_date_len = string.length(end_date_time)
  let duration_len = string.length(duration_str)
  let bar_size_len = string.length(bar_size_str)
  let what_len = string.length(what_str)
  let use_rth_int = case use_rth {
    True -> 1
    False -> 0
  }

  // REQ_HISTORICAL_DATA message format (version 6)
  <<
    20:16,
    version:32,
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
    end_date_len:8,
    end_date_time:utf8,
    duration_len:8,
    duration_str:utf8,
    bar_size_len:8,
    bar_size_str:utf8,
    what_len:8,
    what_str:utf8,
    use_rth_int:8,
    format_date:32,
  >>
}

/// Create cancel historical data message
/// Message code: 25 (CANCEL_HISTORICAL_DATA)
pub fn cancel_historical_data(ticker_id: Int) -> BitArray {
  // CANCEL_HISTORICAL_DATA message format
  <<25:16, ticker_id:32>>
}
