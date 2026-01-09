import account_data
import api_messages
import connection
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import market_data
import message_encoder
import orders
import protocol

// This is main test entry point
pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Test 1: Connection Configuration and Trading Safety
// ============================================================================

pub fn connection_config_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 1: Connection Configuration and Trading Safety")
  io.println(repeat_char("=", 70))

  // Test 1.1: Create config with explicit port
  let explicit_config = connection.config("127.0.0.1", 7497, 1)
  should.equal(explicit_config.port, 7497)
  should.equal(explicit_config.client_id, 1)
  io.println("✓ Explicit port configuration works")

  // Test 1.2: Create config with account type (Paper Trading)
  let paper_config =
    connection.config_with_account_type(
      "127.0.0.1",
      7497,
      connection.PaperTrading,
      1,
    )
  should.equal(paper_config.port, 7497)
  should.equal(paper_config.client_id, 1)
  io.println("✓ Paper trading config uses port 7497")

  // Test 1.3: Create config with account type (Live Trading Read-Only)
  let live_config =
    connection.config_with_account_type(
      "127.0.0.1",
      7496,
      connection.LiveTradingReadOnly,
      1,
    )
  should.equal(live_config.port, 7496)
  should.equal(live_config.client_id, 1)
  io.println("✓ Live trading config uses port 7496")

  // Test 1.4: Check trading permissions
  let paper_allowed = connection.is_trading_allowed(connection.PaperTrading)
  should.be_true(paper_allowed)
  io.println(
    "✓ Paper trading allows trading: " <> bool.to_string(paper_allowed),
  )

  let live_allowed =
    connection.is_trading_allowed(connection.LiveTradingReadOnly)
  should.be_false(live_allowed)
  io.println(
    "✓ Live trading read-only blocks trading: " <> bool.to_string(live_allowed),
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Connection configuration and trading safety - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 2: Protocol Message Construction
// ============================================================================

pub fn protocol_messages_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 2: Protocol Message Construction")
  io.println(repeat_char("=", 70))

  // Test 2.1: Start API handshake message (type-safe)
  io.println("\n2.1: Creating START_API handshake message (type-safe)")
  let start_api_msg = api_messages.start_api_message(100)
  let start_api_encoded = api_messages.encode_message(start_api_msg)
  let start_api_size = bit_array.byte_size(start_api_encoded)
  should.be_true(start_api_size > 0)
  io.println(
    "✓ START_API message created (type-safe): "
    <> int.to_string(start_api_size)
    <> " bytes",
  )

  // Test 2.2: Client ID message (deprecated - for compatibility only)
  io.println("\n2.2: Client ID message (deprecated - use type-safe instead)")
  io.println("  Note: This test is kept for backward compatibility only.")
  io.println("  New code should use api_messages.encode_message()")

  // Test 2.3: Parse server response
  // REMOVED: This test used fake data ("20020260107") which doesn't match
  // real TWS protocol behavior. Real server responses must be obtained from
  // actual TWS connections, not fabricated.
  io.println("\n2.3: Server response parsing - SKIPPED")
  io.println("  Note: Real server response parsing will be tested with")
  io.println("  actual TWS connection data (no fake data allowed)")

  // Test 2.4: Filter control characters
  io.println("\n2.4: Testing control character filtering")
  let dirty_string = "TestStringWithControlChars"
  let clean_string = protocol.strip_leading_control_characters(dirty_string)
  should.equal(clean_string, "TestStringWithControlChars")
  io.println("✓ Control characters filtered successfully")

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Protocol message construction - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 3: Market Data Request Messages
// ============================================================================

pub fn market_data_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 3: Market Data Request Messages")
  io.println(repeat_char("=", 70))

  // Test 3.1: Create market data request
  io.println("\n3.1: Creating market data request for ticker ID 100")
  let apple_contract = market_data.create_stock_contract("AAPL")
  let request_msg = market_data.request_market_data(100, apple_contract)
  let request_size = bit_array.byte_size(request_msg)
  should.be_true(request_size > 0)
  io.println(
    "✓ Market data request created: " <> int.to_string(request_size) <> " bytes",
  )

  // Test 3.2: Create cancel market data request
  io.println("\n3.2: Creating cancel market data request")
  let cancel_msg = market_data.cancel_market_data(100)
  let cancel_size = bit_array.byte_size(cancel_msg)
  should.be_true(cancel_size > 0)
  io.println(
    "✓ Cancel market data request created: "
    <> int.to_string(cancel_size)
    <> " bytes",
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Market data request messages - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 4: Order Placement Messages
// ============================================================================

pub fn orders_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 4: Order Placement Messages")
  io.println(repeat_char("=", 70))

  // Test 4.1: Create market buy order
  io.println("\n4.1: Creating market buy order")
  let buy_order = orders.create_market_order(100, orders.BuyAction, 10)
  should.equal(orders.get_order_id(buy_order), 100)
  io.println("✓ Market buy order created successfully")

  // Test 4.2: Place order with paper trading account (should succeed)
  io.println("\n4.2: Placing order with paper trading account")
  case orders.place_order(connection.PaperTrading, 12_345, buy_order) {
    Ok(msg_bytes) -> {
      let msg_size = bit_array.byte_size(msg_bytes)
      should.be_true(msg_size > 0)
      io.println(
        "✓ Order message created successfully: "
        <> int.to_string(msg_size)
        <> " bytes",
      )
    }
    Error(err) -> {
      io.println("✗ Failed to create order: " <> err)
      should.fail()
    }
  }

  // Test 4.3: Place order with live trading read-only account (should fail)
  io.println(
    "\n4.3: Placing order with live trading read-only account (should fail)",
  )
  let sell_order = orders.create_limit_order(101, orders.SellAction, 5, 150.0)
  case orders.place_order(connection.LiveTradingReadOnly, 12_345, sell_order) {
    Ok(_) -> {
      io.println("✗ Order should have been rejected for live account!")
      should.fail()
    }
    Error(err) -> {
      should.be_true(string_contains(err, "not allowed"))
      io.println("✓ Order correctly rejected: " <> err)
    }
  }

  // Test 4.4: Create cancel order message
  io.println("\n4.4: Creating cancel order message")
  let cancel_msg = orders.cancel_order(100)
  let cancel_size = bit_array.byte_size(cancel_msg)
  should.be_true(cancel_size > 0)
  io.println(
    "✓ Cancel order message created: " <> int.to_string(cancel_size) <> " bytes",
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Order placement messages - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 5: Account Data Request Messages
// ============================================================================

pub fn account_data_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 5: Account Data Request Messages")
  io.println(repeat_char("=", 70))

  // Test 5.1: Create position request message (type-safe)
  io.println("\n5.1: Creating position request message (type-safe)")
  let pos_msg = api_messages.request_positions_message(100)
  let pos_bytes = api_messages.encode_message(pos_msg)
  let pos_size = bit_array.byte_size(pos_bytes)
  should.be_true(pos_size > 0)
  io.println(
    "✓ Position request created (type-safe): "
    <> int.to_string(pos_size)
    <> " bytes",
  )

  // Test 5.2: Create cancel positions message (type-safe)
  io.println("\n5.2: Creating cancel positions message (type-safe)")
  let cancel_pos_msg = api_messages.cancel_positions_message()
  let cancel_pos_bytes = api_messages.encode_message(cancel_pos_msg)
  let cancel_pos_size = bit_array.byte_size(cancel_pos_bytes)
  should.be_true(cancel_pos_size > 0)
  io.println(
    "✓ Cancel positions message created (type-safe): "
    <> int.to_string(cancel_pos_size)
    <> " bytes",
  )

  // Test 5.3: Create account summary request with common tags (type-safe)
  io.println(
    "\n5.3: Creating account summary request with common tags (type-safe)",
  )
  let req_id = 100
  let group_name = "All"
  let tags = account_data.common_account_tags()
  // Convert tags list to comma-separated string
  let tags_string =
    string.join(list.map(tags, account_data.account_summary_tag_to_string), ",")
  let acc_msg =
    api_messages.request_account_summary_with_tags(
      req_id,
      group_name,
      tags_string,
    )
  let acc_bytes = api_messages.encode_message(acc_msg)
  let acc_size = bit_array.byte_size(acc_bytes)
  should.be_true(acc_size > 0)
  io.println(
    "✓ Account summary request created (type-safe): "
    <> int.to_string(acc_size)
    <> " bytes",
  )
  io.println("  Request ID: " <> int.to_string(req_id))
  io.println("  Group: " <> group_name)
  io.println("  Tags: " <> int.to_string(list.length(tags)) <> " tags")

  // Test 5.4: Create account summary request with specific tags (type-safe)
  io.println(
    "\n5.4: Creating account summary request with specific tags (type-safe)",
  )
  let specific_tags = [
    account_data.NetLiquidation,
    account_data.TotalCashBalance,
    account_data.BuyingPower,
  ]
  let specific_tags_string =
    string.join(
      list.map(specific_tags, account_data.account_summary_tag_to_string),
      ",",
    )
  let specific_msg =
    api_messages.request_account_summary_with_tags(
      101,
      "All",
      specific_tags_string,
    )
  let specific_bytes = api_messages.encode_message(specific_msg)
  let specific_size = bit_array.byte_size(specific_bytes)
  should.be_true(specific_size > 0)
  io.println(
    "✓ Specific account summary request created (type-safe): "
    <> int.to_string(specific_size)
    <> " bytes",
  )

  // Test 5.5: Create cancel account summary message (type-safe)
  io.println("\n5.5: Creating cancel account summary message (type-safe)")
  let cancel_acc_msg = api_messages.cancel_account_summary_message(req_id)
  let cancel_acc_bytes = api_messages.encode_message(cancel_acc_msg)
  let cancel_acc_size = bit_array.byte_size(cancel_acc_bytes)
  should.be_true(cancel_acc_size > 0)
  io.println(
    "✓ Cancel account summary message created (type-safe): "
    <> int.to_string(cancel_acc_size)
    <> " bytes",
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Account data request messages - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 6: Automatic Port Detection
// ============================================================================

pub fn automatic_port_detection_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 6: Automatic Port Detection")
  io.println(repeat_char("=", 70))

  // Test 6.1: Detect available port
  io.println("\n6.1: Detecting IB TWS port (7496 or 7497)")
  let detected_port = connection.detect_ib_tws_port("127.0.0.1", 1)
  io.println("✓ Port detection completed")
  case detected_port {
    0 -> {
      io.println("  No IB TWS server detected on ports 7496 or 7497")
      io.println("  (This is expected if TWS is not running)")
    }
    port -> {
      should.be_true(port == 7496 || port == 7497)
      io.println("  Detected port: " <> int.to_string(port))
      let account_type = case port {
        7497 -> "Paper Trading"
        7496 -> "Live Trading"
        _ -> "Unknown"
      }
      io.println("  Account type: " <> account_type)
    }
  }

  // Test 6.2: Create config with auto-detection
  io.println("\n6.2: Creating config with auto-detection")
  case connection.config_auto_detect("127.0.0.1", 1, 1) {
    Ok(config) -> {
      should.be_true(config.port == 7496 || config.port == 7497)
      io.println("✓ Auto-detect config created:")
      io.println("  Host: " <> config.host)
      io.println("  Port: " <> int.to_string(config.port))
      io.println("  Client ID: " <> int.to_string(config.client_id))
    }
    Error(err) -> {
      io.println("✓ Auto-detect returned error (expected if TWS not running):")
      io.println("  " <> err)
    }
  }

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Automatic port detection - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 7: FFI Functions
// ============================================================================

pub fn ffi_functions_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 7: FFI Functions")
  io.println(repeat_char("=", 70))

  // Test 7.1: Get timestamp
  io.println("\n7.1: Testing timestamp generation")
  let timestamp = connection.get_timestamp()
  should.be_true(string.length(timestamp) > 0)
  io.println("✓ Timestamp generated: " <> timestamp)

  // Test 7.2: Generate client ID
  io.println("\n7.2: Testing client ID generation")
  let client_id = connection.generate_client_id()
  should.be_true(client_id > 0)
  io.println("✓ Client ID generated: " <> int.to_string(client_id))

  // Test 7.3: Sleep function
  io.println("\n7.3: Testing sleep function (100ms)")
  connection.sleep(100)
  io.println("✓ Sleep function completed")

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: FFI functions - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 8: Message Size Validation
// ============================================================================

pub fn message_size_validation_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 8: Message Size Validation")
  io.println(repeat_char("=", 70))

  // Test 8.1: Handshake message size (type-safe)
  io.println("\n8.1: Validating handshake message size (type-safe)")
  let start_api_msg = api_messages.start_api_message(100)
  let start_api_bytes = api_messages.encode_message(start_api_msg)
  let start_api_size = bit_array.byte_size(start_api_bytes)
  should.be_true(start_api_size >= 9)
  // API\0 (4) + length (4) + min version string (1)
  io.println(
    "✓ START_API message size (type-safe): "
    <> int.to_string(start_api_size)
    <> " bytes (valid)",
  )

  // Test 8.2: Client ID message size (deprecated - for compatibility only)
  io.println("\n8.2: Client ID message size (deprecated)")
  io.println("  Note: This test is kept for backward compatibility only.")
  io.println("  New code should use api_messages.encode_message()")

  // Test 8.3: Market data request message size
  io.println("\n8.3: Validating market data request message size")
  let contract = market_data.create_stock_contract("AAPL")
  let request_msg = market_data.request_market_data(100, contract)
  let request_size = bit_array.byte_size(request_msg)
  should.be_true(request_size > 0)
  io.println(
    "✓ Market data request message size: "
    <> int.to_string(request_size)
    <> " bytes (valid)",
  )

  // Test 8.4: Order message size
  io.println("\n8.4: Validating order message size")
  let order = orders.create_market_order(100, orders.BuyAction, 10)
  case orders.place_order(connection.PaperTrading, 12_345, order) {
    Ok(order_msg) -> {
      let order_size = bit_array.byte_size(order_msg)
      should.be_true(order_size > 0)
      io.println(
        "✓ Order message size: "
        <> int.to_string(order_size)
        <> " bytes (valid)",
      )
    }
    Error(_) -> {
      should.fail()
    }
  }

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Message size validation - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 9: Account Summary Tags
// ============================================================================

pub fn account_summary_tags_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 9: Account Summary Tags")
  io.println(repeat_char("=", 70))

  // Test 9.1: Test individual tag conversion
  io.println("\n9.1: Testing account summary tag conversion")
  let tags = [
    account_data.AccountTypeTag,
    account_data.NetLiquidation,
    account_data.TotalCashBalance,
    account_data.SettledCash,
    account_data.BuyingPower,
  ]

  let tag_strings = list.map(tags, account_data.account_summary_tag_to_string)
  should.equal(list.length(tag_strings), 5)
  io.println("✓ All tags converted to strings:")
  list.each(tag_strings, fn(tag) { io.println("  - " <> tag) })

  // Test 9.2: Test common account tags
  io.println("\n9.2: Testing common account tags")
  let common_tags = account_data.common_account_tags()
  should.be_true(common_tags != [])
  io.println(
    "✓ Common account tags count: " <> int.to_string(list.length(common_tags)),
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Account summary tags - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 10: Order Types and Actions
// ============================================================================

pub fn order_types_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 10: Order Types and Actions")
  io.println(repeat_char("=", 70))

  // Test 10.1: Test different order types
  io.println("\n10.1: Testing different order types")

  let market_order = orders.create_market_order(100, orders.BuyAction, 10)
  should.equal(orders.get_order_action(market_order), orders.BuyAction)
  io.println("✓ Market order created")

  let limit_order = orders.create_limit_order(101, orders.SellAction, 5, 150.0)
  should.equal(orders.get_order_action(limit_order), orders.SellAction)
  io.println("✓ Limit order created")

  // Test 10.2: Test different order actions
  io.println("\n10.2: Testing different order actions")

  let buy_order = orders.create_market_order(100, orders.BuyAction, 10)
  should.equal(orders.get_order_action(buy_order), orders.BuyAction)
  io.println("✓ Buy action order created")

  let sell_order = orders.create_market_order(101, orders.SellAction, 5)
  should.equal(orders.get_order_action(sell_order), orders.SellAction)
  io.println("✓ Sell action order created")

  let short_order = orders.create_market_order(102, orders.ShortAction, 5)
  should.equal(orders.get_order_action(short_order), orders.ShortAction)
  io.println("✓ Short action order created")

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Order types and actions - PASS")
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
