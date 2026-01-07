import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/string

/// Market Scanner Module
/// 
/// This module provides functionality to scan the market for trading opportunities
/// using IB's market scanner service. The scanner can find instruments matching
/// various criteria like top gainers, most active, highest volume, etc.
/// Scanner subscription parameters
pub type ScannerSubscription {
  ScannerSubscription(
    /// Number of rows to return (1-100)
    number_of_rows: Int,
    /// Instrument type (STK, OPT, FUT, etc.)
    instrument: String,
    /// Location code (e.g., "STK.US.MAJOR")
    location_code: String,
    /// Scan code (e.g., "TOP_PERC_GAIN", "MOST_ACTIVE", etc.)
    scan_code: String,
    /// Volume type (e.g., "TRADES", "VOLUME")
    volume_type: String,
    /// Above price filter
    above_price: Float,
    /// Below price filter
    below_price: Float,
    /// Above volume filter
    above_volume: Int,
    /// Average option volume filter
    average_option_volume: Int,
    /// Market cap filter (in millions)
    market_cap_above: Float,
    /// Market cap filter (in millions)
    market_cap_below: Float,
    /// Moody's rating filter
    moody_rating_above: String,
    /// Moody's rating filter
    moody_rating_below: String,
    /// S&P rating filter
    sp_rating_above: String,
    /// S&P rating filter
    sp_rating_below: String,
    /// Maturity date filter
    maturity_date_above: String,
    /// Maturity date filter
    maturity_date_below: String,
    /// Coupon rate filter (in percentage)
    coupon_rate_above: Float,
    /// Coupon rate filter (in percentage)
    coupon_rate_below: Float,
    /// Exclude convertible bonds
    exclude_convertible: Bool,
    /// Option: scanner setting pairs
    scanner_setting_pairs: List(String),
    /// Stock type filter (e.g., "ALL", "STOCK", "ETF")
    stock_type_filter: String,
  )
}

/// Default scanner subscription
pub fn default_scanner_subscription() -> ScannerSubscription {
  ScannerSubscription(
    number_of_rows: 10,
    instrument: "STK",
    location_code: "STK.US.MAJOR",
    scan_code: "TOP_PERC_GAIN",
    volume_type: "TRADES",
    above_price: 0.0,
    below_price: 999_999.0,
    above_volume: 0,
    average_option_volume: 0,
    market_cap_above: 0.0,
    market_cap_below: 999_999_999_999.0,
    moody_rating_above: "",
    moody_rating_below: "",
    sp_rating_above: "",
    sp_rating_below: "",
    maturity_date_above: "",
    maturity_date_below: "",
    coupon_rate_above: 0.0,
    coupon_rate_below: 100.0,
    exclude_convertible: False,
    scanner_setting_pairs: [],
    stock_type_filter: "ALL",
  )
}

/// Common scan codes
pub fn scan_code_top_perc_gain() -> String {
  "TOP_PERC_GAIN"
}

pub fn scan_code_most_active() -> String {
  "MOST_ACTIVE"
}

pub fn scan_code_top_open() -> String {
  "TOP_OPEN"
}

pub fn scan_code_highest_volume() -> String {
  "HIGHEST_VOLUME"
}

pub fn scan_code_lowest_price() -> String {
  "LOWEST_PRICE"
}

pub fn scan_code_highest_price() -> String {
  "HIGHEST_PRICE"
}

pub fn scan_code_top_volatility() -> String {
  "TOP_VOLATILITY"
}

pub fn scan_code_most_actives_by_volume() -> String {
  "MOST_ACTIVES_BY_VOLUME"
}

/// Scanner result row
pub type ScannerRow {
  ScannerRow(
    /// Rank of this row
    rank: Int,
    /// Contract ID
    contract_id: Int,
    /// Symbol
    symbol: String,
    /// Bid price
    bid: Float,
    /// Ask price
    ask: Float,
    /// Last price
    last: Float,
    /// Volume
    volume: Int,
    /// Closing price
    close: Float,
    /// Date/time of data
    time: String,
  )
}

/// Request market scanner subscription
pub fn request_scanner_subscription(
  ticker_id: Int,
  subscription: ScannerSubscription,
) -> BitArray {
  // Message format for REQ_SCANNER_SUBSCRIPTION (MsgCode 310)
  // ticker_id, number_of_rows, instrument, location_code, scan_code,
  // above_price, below_price, above_volume, average_option_volume,
  // market_cap_above, market_cap_below, moody_rating_above, moody_rating_below,
  // sp_rating_above, sp_rating_below, maturity_date_above, maturity_date_below,
  // coupon_rate_above, coupon_rate_below, exclude_convertible,
  // scanner_setting_pairs (comma separated), stock_type_filter

  let setting_pairs_string = case subscription.scanner_setting_pairs {
    [] -> ""
    list -> string.join(list, ",")
  }

  let exclude_convertible_int = case subscription.exclude_convertible {
    True -> 1
    False -> 0
  }

  bit_array.concat([
    <<310:size(16)>>,
    // MsgCode: REQ_SCANNER_SUBSCRIPTION
    int_to_field(ticker_id),
    int_to_field(subscription.number_of_rows),
    string_to_field(subscription.instrument),
    string_to_field(subscription.location_code),
    string_to_field(subscription.scan_code),
    float_to_field(subscription.above_price),
    float_to_field(subscription.below_price),
    int_to_field(subscription.above_volume),
    int_to_field(subscription.average_option_volume),
    float_to_field(subscription.market_cap_above),
    float_to_field(subscription.market_cap_below),
    string_to_field(subscription.moody_rating_above),
    string_to_field(subscription.moody_rating_below),
    string_to_field(subscription.sp_rating_above),
    string_to_field(subscription.sp_rating_below),
    string_to_field(subscription.maturity_date_above),
    string_to_field(subscription.maturity_date_below),
    float_to_field(subscription.coupon_rate_above),
    float_to_field(subscription.coupon_rate_below),
    <<exclude_convertible_int:size(8)>>,
    string_to_field(setting_pairs_string),
    string_to_field(subscription.stock_type_filter),
  ])
}

/// Cancel market scanner subscription
pub fn cancel_scanner_subscription(ticker_id: Int) -> BitArray {
  // Message format for CANCEL_SCANNER_SUBSCRIPTION (MsgCode 311)
  bit_array.concat([
    <<311:size(16)>>,
    // MsgCode: CANCEL_SCANNER_SUBSCRIPTION
    int_to_field(ticker_id),
  ])
}

/// Request scanner parameters
pub fn request_scanner_parameters() -> BitArray {
  // Message format for REQ_SCANNER_PARAMETERS (MsgCode 315)
  <<315:size(16)>>
  // MsgCode: REQ_SCANNER_PARAMETERS
}

/// Analysis Functions
/// Get symbols from scanner results
pub fn get_symbols(rows: List(ScannerRow)) -> List(String) {
  list.map(rows, fn(row) { row.symbol })
}

/// Filter rows by minimum volume
pub fn filter_by_min_volume(
  rows: List(ScannerRow),
  min_volume: Int,
) -> List(ScannerRow) {
  list.filter(rows, fn(row) { row.volume >= min_volume })
}

/// Filter rows by price range
pub fn filter_by_price_range(
  rows: List(ScannerRow),
  min_price: Float,
  max_price: Float,
) -> List(ScannerRow) {
  list.filter(rows, fn(row) { row.last >=. min_price && row.last <=. max_price })
}

/// Calculate spread for a row
pub fn calculate_spread(row: ScannerRow) -> Result(Float, String) {
  case row.ask >. 0.0 && row.bid >. 0.0 {
    True -> Ok(row.ask -. row.bid)
    False -> Error("Invalid bid/ask prices")
  }
}

/// Calculate percentage change from close
pub fn calculate_percent_change(row: ScannerRow) -> Result(Float, String) {
  case row.close >. 0.0 {
    True -> {
      let diff = row.last -. row.close
      let pct = diff /. row.close *. 100.0
      Ok(pct)
    }
    False -> Error("Invalid close price")
  }
}

/// Get top N rows by volume
pub fn get_top_by_volume(rows: List(ScannerRow), n: Int) -> List(ScannerRow) {
  let sorted = list.sort(rows, fn(a, b) { int.compare(a.volume, b.volume) })

  list.take(sorted, n)
}

/// Get top N rows by percentage change
pub fn get_top_by_percent_change(
  rows: List(ScannerRow),
  n: Int,
) -> List(ScannerRow) {
  let get_percent_change = fn(row) {
    case calculate_percent_change(row) {
      Ok(pct) -> pct
      Error(_) -> 0.0
    }
  }

  let sorted =
    list.sort(rows, fn(a, b) {
      float.compare(get_percent_change(a), get_percent_change(b))
    })

  list.take(sorted, n)
}

/// Formatting Functions
/// Format scanner row for display
pub fn format_scanner_row(row: ScannerRow) -> String {
  let pct_change = case calculate_percent_change(row) {
    Ok(pct) -> float.to_string(pct) <> "%"
    Error(_) -> "N/A"
  }

  let spread = case calculate_spread(row) {
    Ok(s) -> float.to_string(s)
    Error(_) -> "N/A"
  }

  "
Rank: " <> int.to_string(row.rank) <> "
Symbol: " <> row.symbol <> "
Last: " <> float.to_string(row.last) <> "
Bid/Ask: " <> float.to_string(row.bid) <> " / " <> float.to_string(row.ask) <> "
Spread: " <> spread <> "
Volume: " <> int.to_string(row.volume) <> "
Close: " <> float.to_string(row.close) <> "
Change: " <> pct_change <> "
Time: " <> row.time
}

/// Format scanner row as CSV
pub fn format_scanner_row_csv(row: ScannerRow) -> String {
  let pct_change = case calculate_percent_change(row) {
    Ok(pct) -> float.to_string(pct)
    Error(_) -> "N/A"
  }

  let spread = case calculate_spread(row) {
    Ok(s) -> float.to_string(s)
    Error(_) -> "N/A"
  }

  int.to_string(row.rank)
  <> ","
  <> row.symbol
  <> ","
  <> float.to_string(row.last)
  <> ","
  <> float.to_string(row.bid)
  <> ","
  <> float.to_string(row.ask)
  <> ","
  <> spread
  <> ","
  <> int.to_string(row.volume)
  <> ","
  <> float.to_string(row.close)
  <> ","
  <> pct_change
  <> ","
  <> row.time
}

/// Format scanner subscription for display
pub fn format_scanner_subscription(sub: ScannerSubscription) -> String {
  "
Scanner Subscription:
  Instrument: " <> sub.instrument <> "
  Location: " <> sub.location_code <> "
  Scan Code: " <> sub.scan_code <> "
  Rows: " <> int.to_string(sub.number_of_rows) <> "
  Price Range: " <> float.to_string(sub.above_price) <> " - " <> float.to_string(
    sub.below_price,
  ) <> "
  Min Volume: " <> int.to_string(sub.above_volume) <> "
  Volume Type: " <> sub.volume_type <> "
  Stock Type: " <> sub.stock_type_filter
}

// Helper functions (will be implemented in message parsing)

fn int_to_field(value: Int) -> BitArray {
  let bytes = bit_array.from_string(int.to_string(value))
  let length = bit_array.byte_size(bytes)
  bit_array.concat([<<length:size(32)>>, bytes])
}

fn float_to_field(value: Float) -> BitArray {
  let bytes = bit_array.from_string(float.to_string(value))
  let length = bit_array.byte_size(bytes)
  bit_array.concat([<<length:size(32)>>, bytes])
}

fn string_to_field(value: String) -> BitArray {
  let bytes = bit_array.from_string(value)
  let length = bit_array.byte_size(bytes)
  bit_array.concat([<<length:size(32)>>, bytes])
}

/// Parse scanner row (placeholder - to be implemented in message_handler)
pub fn parse_scanner_row(data: BitArray) -> Result(ScannerRow, String) {
  // This will be implemented when parsing actual scanner data
  Error("Not yet implemented - will be added to message_handler")
}
