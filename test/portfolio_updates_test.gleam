import gleam/bit_array
import gleam/io
import gleam/list
import gleeunit
import gleeunit/should
import portfolio_updates

// This is main test entry point
pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Test 1: Portfolio Request Messages
// ============================================================================

pub fn portfolio_request_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 1: Portfolio Request Messages")
  io.println(repeat_char("=", 70))

  // Test 1.1: Request portfolio updates
  io.println("\n1.1: Requesting portfolio updates")
  let request_msg = portfolio_updates.request_portfolio()
  let request_size = bit_array.byte_size(request_msg)
  should.equal(request_size, 1)
  io.println(
    "✓ Portfolio request created: " <> int.to_string(request_size) <> " byte",
  )

  // Test 1.2: Cancel portfolio updates
  io.println("\n1.2: Canceling portfolio updates")
  let cancel_msg = portfolio_updates.cancel_portfolio()
  let cancel_size = bit_array.byte_size(cancel_msg)
  should.equal(cancel_size, 1)
  io.println(
    "✓ Cancel portfolio request created: "
    <> int.to_string(cancel_size)
    <> " byte",
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Portfolio request messages - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 2: Account Update Messages
// ============================================================================

pub fn account_update_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 2: Account Update Messages")
  io.println(repeat_char("=", 70))

  // Test 2.1: Request account updates (subscribe)
  io.println("\n2.1: Requesting account updates (subscribe)")
  let subscribe_msg = portfolio_updates.request_account_updates(True)
  let subscribe_size = bit_array.byte_size(subscribe_msg)
  should.be_true(subscribe_size > 0)
  io.println(
    "✓ Subscribe to account updates: "
    <> int.to_string(subscribe_size)
    <> " bytes",
  )

  // Test 2.2: Cancel account updates (unsubscribe)
  io.println("\n2.2: Canceling account updates (unsubscribe)")
  let unsubscribe_msg = portfolio_updates.request_account_updates(False)
  let unsubscribe_size = bit_array.byte_size(unsubscribe_msg)
  should.be_true(unsubscribe_size > 0)
  io.println(
    "✓ Unsubscribe from account updates: "
    <> int.to_string(unsubscribe_size)
    <> " bytes",
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Account update messages - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 3: Account Summary Messages
// ============================================================================

pub fn account_summary_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 3: Account Summary Messages")
  io.println(repeat_char("=", 70))

  // Test 3.1: Request account summary
  io.println("\n3.1: Requesting account summary")
  let tags = ["NetLiquidation", "TotalCashBalance", "BuyingPower"]
  let summary_msg = portfolio_updates.request_account_summary(100, "All", tags)
  let summary_size = bit_array.byte_size(summary_msg)
  should.be_true(summary_size > 0)
  io.println(
    "✓ Account summary request created: "
    <> int.to_string(summary_size)
    <> " bytes",
  )
  io.println("   Request ID: 100")
  io.println("   Group: All")
  io.println("   Tags: " <> int.to_string(list.length(tags)))

  // Test 3.2: Cancel account summary
  io.println("\n3.2: Canceling account summary")
  let cancel_msg = portfolio_updates.cancel_account_summary(100)
  let cancel_size = bit_array.byte_size(cancel_msg)
  should.be_true(cancel_size > 0)
  io.println(
    "✓ Cancel account summary created: "
    <> int.to_string(cancel_size)
    <> " bytes",
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Account summary messages - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 4: Position Analysis
// ============================================================================

pub fn position_analysis_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 4: Position Analysis")
  io.println(repeat_char("=", 70))

  // Test 4.1: Create sample positions
  io.println("\n4.1: Creating sample positions")
  let pos1 = portfolio_updates.create_sample_position()
  let pos2 =
    portfolio_updates.Position(
      account: "DU1234567",
      contract_id: 67_890,
      symbol: "TSLA",
      sec_type: "STK",
      exchange: "SMART",
      currency: "USD",
      position: -50.0,
      avg_cost: 200.0,
      market_price: 195.0,
      market_value: -9750.0,
      realized_pnl: -250.0,
      unrealized_pnl: 250.0,
    )

  let positions = [pos1, pos2]
  io.println(
    "✓ Created " <> int.to_string(list.length(positions)) <> " positions",
  )

  // Test 4.2: Calculate total position value
  io.println("\n4.2: Calculating total position value")
  let total_value = portfolio_updates.calculate_total_position_value(positions)
  should.be_true(total_value >. 0.0)
  io.println("✓ Total position value: " <> float.to_string(total_value))

  // Test 4.3: Calculate total unrealized P&L
  io.println("\n4.3: Calculating total unrealized P&L")
  let total_unrealized =
    portfolio_updates.calculate_total_unrealized_pnl(positions)
  should.be_true(total_unrealized >. 0.0)
  io.println("✓ Total unrealized P&L: " <> float.to_string(total_unrealized))

  // Test 4.4: Calculate total realized P&L
  io.println("\n4.4: Calculating total realized P&L")
  let total_realized = portfolio_updates.calculate_total_realized_pnl(positions)
  should.be_true(total_realized >. 0.0)
  io.println("✓ Total realized P&L: " <> float.to_string(total_realized))

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Position analysis - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 5: Position Filtering
// ============================================================================

pub fn position_filtering_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 5: Position Filtering")
  io.println(repeat_char("=", 70))

  // Test 5.1: Create multiple positions
  io.println("\n5.1: Creating multiple positions")
  let pos1 = portfolio_updates.create_sample_position()
  let pos2 =
    portfolio_updates.Position(
      account: "DU1234567",
      contract_id: 67_890,
      symbol: "TSLA",
      sec_type: "STK",
      exchange: "SMART",
      currency: "USD",
      position: -50.0,
      avg_cost: 200.0,
      market_price: 195.0,
      market_value: -9750.0,
      realized_pnl: -250.0,
      unrealized_pnl: 250.0,
    )
  let pos3 =
    portfolio_updates.Position(
      account: "DU1234567",
      contract_id: 12_345,
      symbol: "AAPL",
      sec_type: "STK",
      exchange: "SMART",
      currency: "USD",
      position: 50.0,
      avg_cost: 150.0,
      market_price: 155.0,
      market_value: 7750.0,
      realized_pnl: 250.0,
      unrealized_pnl: 250.0,
    )

  let positions = [pos1, pos2, pos3]
  io.println(
    "✓ Created " <> int.to_string(list.length(positions)) <> " positions",
  )

  // Test 5.2: Filter by symbol
  io.println("\n5.2: Filtering positions by symbol")
  let aapl_positions =
    portfolio_updates.filter_positions_by_symbol(positions, "AAPL")
  should.equal(list.length(aapl_positions), 2)
  io.println(
    "✓ Found "
    <> int.to_string(list.length(aapl_positions))
    <> " AAPL positions",
  )

  // Test 5.3: Filter by account
  io.println("\n5.3: Filtering positions by account")
  let account_positions =
    portfolio_updates.filter_positions_by_account(positions, "DU1234567")
  should.equal(list.length(account_positions), 3)
  io.println(
    "✓ Found "
    <> int.to_string(list.length(account_positions))
    <> " positions for account",
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Position filtering - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 6: Long and Short Positions
// ============================================================================

pub fn long_short_positions_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 6: Long and Short Positions")
  io.println(repeat_char("=", 70))

  // Test 6.1: Create mixed positions
  io.println("\n6.1: Creating mixed long and short positions")
  let pos1 = portfolio_updates.create_sample_position()
  // Long
  let pos2 =
    portfolio_updates.Position(
      account: "DU1234567",
      contract_id: 67_890,
      symbol: "TSLA",
      sec_type: "STK",
      exchange: "SMART",
      currency: "USD",
      position: -50.0,
      avg_cost: 200.0,
      market_price: 195.0,
      market_value: -9750.0,
      realized_pnl: -250.0,
      unrealized_pnl: 250.0,
    )

  let positions = [pos1, pos2]
  io.println(
    "✓ Created " <> int.to_string(list.length(positions)) <> " positions",
  )

  // Test 6.2: Get long positions
  io.println("\n6.2: Getting long positions")
  let long_positions = portfolio_updates.get_long_positions(positions)
  should.equal(list.length(long_positions), 1)
  io.println(
    "✓ Found "
    <> int.to_string(list.length(long_positions))
    <> " long positions",
  )

  // Test 6.3: Get short positions
  io.println("\n6.3: Getting short positions")
  let short_positions = portfolio_updates.get_short_positions(positions)
  should.equal(list.length(short_positions), 1)
  io.println(
    "✓ Found "
    <> int.to_string(list.length(short_positions))
    <> " short positions",
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Long and short positions - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 7: Position Formatting
// ============================================================================

pub fn position_formatting_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 7: Position Formatting")
  io.println(repeat_char("=", 70))

  // Test 7.1: Format long position
  io.println("\n7.1: Formatting long position")
  let long_pos = portfolio_updates.create_sample_position()
  let long_formatted = portfolio_updates.format_position(long_pos)
  should.be_true(string.contains(long_formatted, "LONG"))
  should.be_true(string.contains(long_formatted, "AAPL"))
  io.println("✓ Long position formatted:")
  io.println(long_formatted)

  // Test 7.2: Format short position
  io.println("\n7.2: Formatting short position")
  let short_pos =
    portfolio_updates.Position(
      account: "DU1234567",
      contract_id: 67_890,
      symbol: "TSLA",
      sec_type: "STK",
      exchange: "SMART",
      currency: "USD",
      position: -50.0,
      avg_cost: 200.0,
      market_price: 195.0,
      market_value: -9750.0,
      realized_pnl: -250.0,
      unrealized_pnl: 250.0,
    )
  let short_formatted = portfolio_updates.format_position(short_pos)
  should.be_true(string.contains(short_formatted, "SHORT"))
  should.be_true(string.contains(short_formatted, "TSLA"))
  io.println("✓ Short position formatted:")
  io.println(short_formatted)

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Position formatting - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 8: Account Value Formatting
// ============================================================================

pub fn account_value_formatting_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 8: Account Value Formatting")
  io.println(repeat_char("=", 70))

  // Test 8.1: Format account value
  io.println("\n8.1: Formatting account value")
  let account_value = portfolio_updates.create_sample_account_value()
  let formatted = portfolio_updates.format_account_value(account_value)

  should.be_true(string.contains(formatted, "NetLiquidation"))
  should.be_true(string.contains(formatted, "100000.00"))
  should.be_true(string.contains(formatted, "USD"))
  io.println("✓ Account value formatted:")
  io.println("   " <> formatted)

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Account value formatting - PASS")
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
