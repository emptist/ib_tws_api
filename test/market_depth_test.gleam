import gleam/bit_array
import gleam/io
import gleam/list
import gleeunit
import gleeunit/should
import market_depth

// This is main test entry point
pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Test 1: Market Depth Configuration
// ============================================================================

pub fn market_depth_config_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 1: Market Depth Configuration")
  io.println(repeat_char("=", 70))

  // Test 1.1: Create default market depth config
  io.println("\n1.1: Creating default market depth config")
  let default_config = market_depth.default_market_depth_config()
  should.equal(default_config.num_rows, 5)
  should.equal(default_config.is_smart_depth, True)
  io.println("✓ Default config created:")
  io.println("   Num rows: " <> int.to_string(default_config.num_rows))
  io.println(
    "   Smart depth: " <> bool.to_string(default_config.is_smart_depth),
  )

  // Test 1.2: Create custom market depth config
  io.println("\n1.2: Creating custom market depth config")
  let custom_config = market_depth.market_depth_config(10, False)
  should.equal(custom_config.num_rows, 10)
  should.equal(custom_config.is_smart_depth, False)
  io.println("✓ Custom config created:")
  io.println("   Num rows: " <> int.to_string(custom_config.num_rows))
  io.println("   Smart depth: " <> bool.to_string(custom_config.is_smart_depth))

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Market depth configuration - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 2: Market Depth Request Messages
// ============================================================================

pub fn market_depth_request_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 2: Market Depth Request Messages")
  io.println(repeat_char("=", 70))

  // Test 2.1: Create market depth request
  io.println("\n2.1: Creating market depth request")
  let config = market_depth.default_market_depth_config()
  let request_msg = market_depth.request_market_depth(100, 12_345, config)
  let request_size = bit_array.byte_size(request_msg)
  should.be_true(request_size > 0)
  io.println(
    "✓ Market depth request created: "
    <> int.to_string(request_size)
    <> " bytes",
  )
  io.println("   Ticker ID: 100")
  io.println("   Contract ID: 12345")

  // Test 2.2: Create cancel market depth request
  io.println("\n2.2: Creating cancel market depth request")
  let cancel_msg = market_depth.cancel_market_depth(100)
  let cancel_size = bit_array.byte_size(cancel_msg)
  should.be_true(cancel_size > 0)
  io.println(
    "✓ Cancel market depth request created: "
    <> int.to_string(cancel_size)
    <> " bytes",
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Market depth request messages - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 3: Market Depth Operations
// ============================================================================

pub fn market_depth_operations_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 3: Market Depth Operations")
  io.println(repeat_char("=", 70))

  // Test 3.1: Test market depth operation conversion
  io.println("\n3.1: Testing market depth operation conversion")
  let insert_str =
    market_depth.market_depth_operation_to_string(market_depth.Insert)
  should.equal(insert_str, "0")
  io.println("✓ Insert operation: " <> insert_str)

  let update_str =
    market_depth.market_depth_operation_to_string(market_depth.Update)
  should.equal(update_str, "1")
  io.println("✓ Update operation: " <> update_str)

  let delete_str =
    market_depth.market_depth_operation_to_string(market_depth.Delete)
  should.equal(delete_str, "2")
  io.println("✓ Delete operation: " <> delete_str)

  // Test 3.2: Test parsing market depth operations
  io.println("\n3.2: Testing parsing market depth operations")
  case market_depth.parse_market_depth_operation("0") {
    Ok(market_depth.Insert) -> io.println("✓ Insert operation parsed correctly")
    _ -> should.fail()
  }

  case market_depth.parse_market_depth_operation("1") {
    Ok(market_depth.Update) -> io.println("✓ Update operation parsed correctly")
    _ -> should.fail()
  }

  case market_depth.parse_market_depth_operation("2") {
    Ok(market_depth.Delete) -> io.println("✓ Delete operation parsed correctly")
    _ -> should.fail()
  }

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Market depth operations - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 4: Market Depth Row Creation and Analysis
// ============================================================================

pub fn market_depth_row_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 4: Market Depth Row Creation and Analysis")
  io.println(repeat_char("=", 70))

  // Test 4.1: Create market depth rows
  io.println("\n4.1: Creating market depth rows")
  let bid_row1 =
    market_depth.create_market_depth_row(150.0, 100.0, "IBKR", True)
  let bid_row2 =
    market_depth.create_market_depth_row(149.5, 200.0, "CSFB", True)
  let ask_row1 =
    market_depth.create_market_depth_row(150.5, 150.0, "IBKR", False)
  let ask_row2 =
    market_depth.create_market_depth_row(151.0, 100.0, "CSFB", False)

  let rows = [bid_row1, bid_row2, ask_row1, ask_row2]
  io.println(
    "✓ Created " <> int.to_string(list.length(rows)) <> " market depth rows",
  )

  // Test 4.2: Calculate total liquidity
  io.println("\n4.2: Calculating total liquidity")
  let total_liquidity = market_depth.calculate_total_liquidity(rows)
  should.be_true(total_liquidity >. 0.0)
  io.println("✓ Total liquidity: " <> float.to_string(total_liquidity))

  // Test 4.3: Calculate weighted average price
  io.println("\n4.3: Calculating weighted average price")
  let weighted_avg = market_depth.calculate_weighted_avg_price(rows)
  should.be_true(weighted_avg >. 0.0)
  io.println("✓ Weighted average price: " <> float.to_string(weighted_avg))

  // Test 4.4: Get best bid
  io.println("\n4.4: Getting best bid price")
  case market_depth.get_best_bid(rows) {
    Ok(best_bid) -> {
      should.be_true(best_bid >. 0.0)
      io.println("✓ Best bid: " <> float.to_string(best_bid))
    }
    Error(_) -> should.fail()
  }

  // Test 4.5: Get best ask
  io.println("\n4.5: Getting best ask price")
  case market_depth.get_best_ask(rows) {
    Ok(best_ask) -> {
      should.be_true(best_ask >. 0.0)
      io.println("✓ Best ask: " <> float.to_string(best_ask))
    }
    Error(_) -> should.fail()
  }

  // Test 4.6: Calculate spread
  io.println("\n4.6: Calculating spread")
  case market_depth.calculate_spread(rows) {
    Ok(spread) -> {
      should.be_true(spread >. 0.0)
      io.println("✓ Spread: " <> float.to_string(spread))
    }
    Error(_) -> should.fail()
  }

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Market depth row creation and analysis - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 5: Market Depth Row Formatting
// ============================================================================

pub fn market_depth_formatting_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 5: Market Depth Row Formatting")
  io.println(repeat_char("=", 70))

  // Test 5.1: Format bid row
  io.println("\n5.1: Formatting bid row")
  let bid_row = market_depth.create_market_depth_row(150.0, 100.0, "IBKR", True)
  let bid_str = market_depth.format_market_depth_row(bid_row)
  should.be_true(string.contains(bid_str, "BID"))
  should.be_true(string.contains(bid_str, "150.0"))
  should.be_true(string.contains(bid_str, "100.0"))
  io.println("✓ Bid row formatted:")
  io.println("   " <> bid_str)

  // Test 5.2: Format ask row
  io.println("\n5.2: Formatting ask row")
  let ask_row =
    market_depth.create_market_depth_row(150.5, 150.0, "CSFB", False)
  let ask_str = market_depth.format_market_depth_row(ask_row)
  should.be_true(string.contains(ask_str, "ASK"))
  should.be_true(string.contains(ask_str, "150.5"))
  should.be_true(string.contains(ask_str, "150.0"))
  io.println("✓ Ask row formatted:")
  io.println("   " <> ask_str)

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Market depth row formatting - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 6: Edge Cases
// ============================================================================

pub fn market_depth_edge_cases_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 6: Market Depth Edge Cases")
  io.println(repeat_char("=", 70))

  // Test 6.1: Empty list for best bid
  io.println("\n6.1: Testing empty list for best bid")
  case market_depth.get_best_bid([]) {
    Error(_) -> io.println("✓ Correctly returns error for empty bid list")
    _ -> should.fail()
  }

  // Test 6.2: Empty list for best ask
  io.println("\n6.2: Testing empty list for best ask")
  case market_depth.get_best_ask([]) {
    Error(_) -> io.println("✓ Correctly returns error for empty ask list")
    _ -> should.fail()
  }

  // Test 6.3: Invalid operation string
  io.println("\n6.3: Testing invalid operation string")
  case market_depth.parse_market_depth_operation("99") {
    Error(_) -> io.println("✓ Correctly returns error for invalid operation")
    _ -> should.fail()
  }

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Market depth edge cases - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Helper Functions
// ============================================================================

fn repeat_char(char: String, times: Int) -> String {
  list.range(0, times - 1)
  |> list.map(fn(_) { char })
  |> string.concat
}

fn string_contains(haystack: String, needle: String) -> Bool {
  case string.split(haystack, needle) {
    [_] -> False
    _ -> True
  }
}

import gleam/bool
import gleam/float
import gleam/int
import gleam/string
