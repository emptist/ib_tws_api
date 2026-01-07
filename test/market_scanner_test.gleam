import gleam/bit_array
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import market_scanner

pub fn main() {
  gleeunit.main()
}

// TEST 1: Scanner Subscription Configuration

pub fn test_default_scanner_subscription() {
  let sub = market_scanner.default_scanner_subscription()

  io.println("\n1.1: Creating default scanner subscription")
  io.println("✓ Default subscription created:")
  io.println("   Rows: " <> int.to_string(sub.number_of_rows))
  io.println("   Instrument: " <> sub.instrument)
  io.println("   Location: " <> sub.location_code)
  io.println("   Scan Code: " <> sub.scan_code)
}

pub fn test_custom_scanner_subscription() {
  let sub =
    market_scanner.ScannerSubscription(
      number_of_rows: 20,
      instrument: "STK",
      location_code: "STK.US.MAJOR",
      scan_code: "MOST_ACTIVE",
      volume_type: "VOLUME",
      above_price: 10.0,
      below_price: 500.0,
      above_volume: 1_000_000,
      average_option_volume: 0,
      market_cap_above: 1000.0,
      market_cap_below: 999_999.0,
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

  io.println("\n1.2: Creating custom scanner subscription")
  io.println("✓ Custom subscription created:")
  io.println("   Rows: " <> int.to_string(sub.number_of_rows))
  io.println("   Scan Code: " <> sub.scan_code)
  io.println("   Min Price: " <> float.to_string(sub.above_price))
  io.println("   Max Price: " <> float.to_string(sub.below_price))
  io.println("   Min Volume: " <> int.to_string(sub.above_volume))
}

pub fn test_scan_codes() {
  io.println("\n1.3: Testing scan codes")
  io.println("✓ Top % Gain: " <> market_scanner.scan_code_top_perc_gain())
  io.println("✓ Most Active: " <> market_scanner.scan_code_most_active())
  io.println("✓ Top Open: " <> market_scanner.scan_code_top_open())
  io.println("✓ Highest Volume: " <> market_scanner.scan_code_highest_volume())
  io.println("✓ Lowest Price: " <> market_scanner.scan_code_lowest_price())
  io.println("✓ Highest Price: " <> market_scanner.scan_code_highest_price())
  io.println("✓ Top Volatility: " <> market_scanner.scan_code_top_volatility())
  io.println(
    "✓ Most Actives by Volume: "
    <> market_scanner.scan_code_most_actives_by_volume(),
  )
}

// TEST 2: Scanner Request Messages

pub fn test_scanner_subscription_request() {
  let sub = market_scanner.default_scanner_subscription()
  let message = market_scanner.request_scanner_subscription(100, sub)
  let size = bit_array.byte_size(message)

  io.println("\n2.1: Creating scanner subscription request")
  io.println(
    "✓ Scanner subscription request created: "
    <> int.to_string(size)
    <> " bytes",
  )
  io.println("   Ticker ID: 100")
}

pub fn test_cancel_scanner_subscription() {
  let message = market_scanner.cancel_scanner_subscription(100)
  let size = bit_array.byte_size(message)

  io.println("\n2.2: Creating cancel scanner subscription request")
  io.println(
    "✓ Cancel scanner subscription created: " <> int.to_string(size) <> " bytes",
  )
}

pub fn test_scanner_parameters_request() {
  let message = market_scanner.request_scanner_parameters()
  let size = bit_array.byte_size(message)

  io.println("\n2.3: Creating scanner parameters request")
  io.println(
    "✓ Scanner parameters request created: " <> int.to_string(size) <> " bytes",
  )
}

// TEST 3: Scanner Row Creation and Analysis

pub fn test_scanner_row_creation() {
  let row1 =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "AAPL",
      bid: 150.0,
      ask: 150.5,
      last: 150.25,
      volume: 1_000_000,
      close: 148.0,
      time: "2024-01-07 10:00:00",
    )

  let row2 =
    market_scanner.ScannerRow(
      rank: 2,
      contract_id: 12_346,
      symbol: "TSLA",
      bid: 200.0,
      ask: 200.5,
      last: 200.25,
      volume: 500_000,
      close: 195.0,
      time: "2024-01-07 10:00:00",
    )

  let rows = [row1, row2]

  io.println("\n3.1: Creating scanner rows")
  io.println(
    "✓ Created " <> int.to_string(list.length(rows)) <> " scanner rows",
  )
}

pub fn test_scanner_row_spread() {
  let row =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "AAPL",
      bid: 150.0,
      ask: 150.5,
      last: 150.25,
      volume: 1_000_000,
      close: 148.0,
      time: "2024-01-07 10:00:00",
    )

  let spread = market_scanner.calculate_spread(row)

  io.println("\n3.2: Calculating spread")
  case spread {
    Ok(s) -> io.println("✓ Spread: " <> float.to_string(s))
    Error(e) -> io.println("✗ Error: " <> e)
  }
}

pub fn test_scanner_row_percent_change() {
  let row =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "AAPL",
      bid: 150.0,
      ask: 150.5,
      last: 150.25,
      volume: 1_000_000,
      close: 148.0,
      time: "2024-01-07 10:00:00",
    )

  let pct_change = market_scanner.calculate_percent_change(row)

  io.println("\n3.3: Calculating percentage change")
  case pct_change {
    Ok(pct) -> io.println("✓ Percent change: " <> float.to_string(pct) <> "%")
    Error(e) -> io.println("✗ Error: " <> e)
  }
}

// TEST 4: Scanner Row Filtering

pub fn test_filter_by_min_volume() {
  let row1 =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "AAPL",
      bid: 150.0,
      ask: 150.5,
      last: 150.25,
      volume: 1_000_000,
      close: 148.0,
      time: "2024-01-07 10:00:00",
    )

  let row2 =
    market_scanner.ScannerRow(
      rank: 2,
      contract_id: 12_346,
      symbol: "TSLA",
      bid: 200.0,
      ask: 200.5,
      last: 200.25,
      volume: 500_000,
      close: 195.0,
      time: "2024-01-07 10:00:00",
    )

  let rows = [row1, row2]
  let filtered = market_scanner.filter_by_min_volume(rows, 750_000)

  io.println("\n4.1: Filtering by minimum volume")
  io.println(
    "✓ Found "
    <> int.to_string(list.length(filtered))
    <> " rows with volume >= 750000",
  )
}

pub fn test_filter_by_price_range() {
  let row1 =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "AAPL",
      bid: 150.0,
      ask: 150.5,
      last: 150.25,
      volume: 1_000_000,
      close: 148.0,
      time: "2024-01-07 10:00:00",
    )

  let row2 =
    market_scanner.ScannerRow(
      rank: 2,
      contract_id: 12_346,
      symbol: "TSLA",
      bid: 200.0,
      ask: 200.5,
      last: 200.25,
      volume: 500_000,
      close: 195.0,
      time: "2024-01-07 10:00:00",
    )

  let rows = [row1, row2]
  let filtered = market_scanner.filter_by_price_range(rows, 140.0, 160.0)

  io.println("\n4.2: Filtering by price range")
  io.println(
    "✓ Found "
    <> int.to_string(list.length(filtered))
    <> " rows in price range $140-$160",
  )
}

pub fn test_get_symbols() {
  let row1 =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "AAPL",
      bid: 150.0,
      ask: 150.5,
      last: 150.25,
      volume: 1_000_000,
      close: 148.0,
      time: "2024-01-07 10:00:00",
    )

  let row2 =
    market_scanner.ScannerRow(
      rank: 2,
      contract_id: 12_346,
      symbol: "TSLA",
      bid: 200.0,
      ask: 200.5,
      last: 200.25,
      volume: 500_000,
      close: 195.0,
      time: "2024-01-07 10:00:00",
    )

  let rows = [row1, row2]
  let symbols = market_scanner.get_symbols(rows)

  io.println("\n4.3: Extracting symbols")
  io.println("✓ Symbols: " <> string.join(symbols, ", "))
}

// TEST 5: Scanner Row Sorting

pub fn test_get_top_by_volume() {
  let row1 =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "AAPL",
      bid: 150.0,
      ask: 150.5,
      last: 150.25,
      volume: 1_000_000,
      close: 148.0,
      time: "2024-01-07 10:00:00",
    )

  let row2 =
    market_scanner.ScannerRow(
      rank: 2,
      contract_id: 12_346,
      symbol: "TSLA",
      bid: 200.0,
      ask: 200.5,
      last: 200.25,
      volume: 500_000,
      close: 195.0,
      time: "2024-01-07 10:00:00",
    )

  let row3 =
    market_scanner.ScannerRow(
      rank: 3,
      contract_id: 12_347,
      symbol: "NVDA",
      bid: 250.0,
      ask: 250.5,
      last: 250.25,
      volume: 2_000_000,
      close: 240.0,
      time: "2024-01-07 10:00:00",
    )

  let rows = [row1, row2, row3]
  let top = market_scanner.get_top_by_volume(rows, 2)

  io.println("\n5.1: Getting top 2 by volume")
  io.println(
    "✓ Found " <> int.to_string(list.length(top)) <> " top volume rows",
  )
}

pub fn test_get_top_by_percent_change() {
  let row1 =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "AAPL",
      bid: 150.0,
      ask: 150.5,
      last: 150.25,
      volume: 1_000_000,
      close: 148.0,
      time: "2024-01-07 10:00:00",
    )

  let row2 =
    market_scanner.ScannerRow(
      rank: 2,
      contract_id: 12_346,
      symbol: "TSLA",
      bid: 200.0,
      ask: 200.5,
      last: 200.25,
      volume: 500_000,
      close: 195.0,
      time: "2024-01-07 10:00:00",
    )

  let row3 =
    market_scanner.ScannerRow(
      rank: 3,
      contract_id: 12_347,
      symbol: "NVDA",
      bid: 250.0,
      ask: 250.5,
      last: 250.25,
      volume: 2_000_000,
      close: 240.0,
      time: "2024-01-07 10:00:00",
    )

  let rows = [row1, row2, row3]
  let top = market_scanner.get_top_by_percent_change(rows, 2)

  io.println("\n5.2: Getting top 2 by percent change")
  io.println(
    "✓ Found " <> int.to_string(list.length(top)) <> " top percent change rows",
  )
}

// TEST 6: Scanner Row Formatting

pub fn test_format_scanner_row() {
  let row =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "AAPL",
      bid: 150.0,
      ask: 150.5,
      last: 150.25,
      volume: 1_000_000,
      close: 148.0,
      time: "2024-01-07 10:00:00",
    )

  let formatted = market_scanner.format_scanner_row(row)

  io.println("\n6.1: Formatting scanner row")
  io.println("✓ Scanner row formatted:")
  io.println(formatted)
}

pub fn test_format_scanner_row_csv() {
  let row =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "AAPL",
      bid: 150.0,
      ask: 150.5,
      last: 150.25,
      volume: 1_000_000,
      close: 148.0,
      time: "2024-01-07 10:00:00",
    )

  let formatted = market_scanner.format_scanner_row_csv(row)

  io.println("\n6.2: Formatting scanner row as CSV")
  io.println("✓ Scanner row CSV:")
  io.println(formatted)
}

pub fn test_format_scanner_subscription() {
  let sub = market_scanner.default_scanner_subscription()
  let formatted = market_scanner.format_scanner_subscription(sub)

  io.println("\n6.3: Formatting scanner subscription")
  io.println("✓ Scanner subscription formatted:")
  io.println(formatted)
}

// TEST 7: Edge Cases

pub fn test_edge_cases() {
  // Test invalid bid/ask
  let invalid_row =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "TEST",
      bid: 0.0,
      ask: 0.0,
      last: 100.0,
      volume: 1000,
      close: 95.0,
      time: "2024-01-07 10:00:00",
    )

  let spread = market_scanner.calculate_spread(invalid_row)

  io.println("\n7.1: Testing invalid bid/ask")
  case spread {
    Ok(_) -> io.println("✗ Should have returned error")
    Error(_) -> io.println("✓ Correctly returns error for invalid bid/ask")
  }

  // Test invalid close price
  let invalid_close_row =
    market_scanner.ScannerRow(
      rank: 1,
      contract_id: 12_345,
      symbol: "TEST",
      bid: 100.0,
      ask: 101.0,
      last: 100.5,
      volume: 1000,
      close: 0.0,
      time: "2024-01-07 10:00:00",
    )

  let pct_change = market_scanner.calculate_percent_change(invalid_close_row)

  io.println("\n7.2: Testing invalid close price")
  case pct_change {
    Ok(_) -> io.println("✗ Should have returned error")
    Error(_) -> io.println("✓ Correctly returns error for invalid close price")
  }

  // Test empty list filtering
  let empty: List(market_scanner.ScannerRow) = []
  let filtered = market_scanner.filter_by_min_volume(empty, 1000)

  io.println("\n7.3: Testing empty list filtering")
  io.println(
    "✓ Empty list filter returns "
    <> int.to_string(list.length(filtered))
    <> " rows",
  )

  // Test empty list sorting
  let top_empty = market_scanner.get_top_by_volume(empty, 5)

  io.println("\n7.4: Testing empty list sorting")
  io.println(
    "✓ Empty list sort returns "
    <> int.to_string(list.length(top_empty))
    <> " rows",
  )
}
