import connection
import gleam/int
import gleam/io

/// Test automatic port switching and trading safety features
pub fn main() {
  io.println("=== Automatic Port Switching & Trading Safety Test ===")
  io.println("")

  // Test 1: Paper trading account (should use port 7497, trading allowed)
  io.println("Test 1: Paper Trading Account")
  let paper_config =
    connection.config_with_account_type("127.0.0.1", connection.PaperTrading, 1)
  io.println("  Account Type: PaperTrading")
  io.println("  Expected Port: 7497")
  io.println("  Actual Port: " <> int.to_string(paper_config.port))
  io.println(
    "  Trading Allowed: "
    <> bool_to_string(connection.is_trading_allowed(connection.PaperTrading)),
  )
  let paper_port_test = paper_config.port == 7497
  let paper_trading_test =
    connection.is_trading_allowed(connection.PaperTrading) == True
  io.println("  Port Test: " <> bool_to_string(paper_port_test))
  io.println("  Trading Test: " <> bool_to_string(paper_trading_test))
  io.println("")

  // Test 2: Live trading read-only account (should use port 7496, NO TRADING)
  io.println("Test 2: Live Trading Read-Only Account (Development Mode)")
  let live_readonly_config =
    connection.config_with_account_type(
      "127.0.0.1",
      connection.LiveTradingReadOnly,
      1,
    )
  io.println("  Account Type: LiveTradingReadOnly")
  io.println("  Expected Port: 7496")
  io.println("  Actual Port: " <> int.to_string(live_readonly_config.port))
  io.println(
    "  Trading Allowed: "
    <> bool_to_string(connection.is_trading_allowed(
      connection.LiveTradingReadOnly,
    )),
  )
  io.println("  ⚠️ SAFETY: Trading operations should be blocked ⚠️")
  let live_readonly_port_test = live_readonly_config.port == 7496
  let live_readonly_trading_test =
    connection.is_trading_allowed(connection.LiveTradingReadOnly) == False
  io.println("  Port Test: " <> bool_to_string(live_readonly_port_test))
  io.println(
    "  Trading Safety Test: " <> bool_to_string(live_readonly_trading_test),
  )
  io.println("")

  // Test 3: Live trading account with full permissions (should use port 7496, trading allowed)
  io.println("Test 3: Live Trading Account (Production Mode)")
  let live_config =
    connection.config_with_account_type("127.0.0.1", connection.LiveTrading, 1)
  io.println("  Account Type: LiveTrading")
  io.println("  Expected Port: 7496")
  io.println("  Actual Port: " <> int.to_string(live_config.port))
  io.println(
    "  Trading Allowed: "
    <> bool_to_string(connection.is_trading_allowed(connection.LiveTrading)),
  )
  io.println("  ⚠️ PRODUCTION: Only use for deployment ⚠️")
  let live_port_test = live_config.port == 7496
  let live_trading_test =
    connection.is_trading_allowed(connection.LiveTrading) == True
  io.println("  Port Test: " <> bool_to_string(live_port_test))
  io.println("  Trading Test: " <> bool_to_string(live_trading_test))
  io.println("")

  // Test 4: Explicit port (still works)
  io.println("Test 4: Explicit Port Configuration")
  let explicit_config = connection.config("127.0.0.1", 7498, 1)
  io.println("  Explicit Port: 7498")
  io.println("  Actual Port: " <> int.to_string(explicit_config.port))
  let explicit_test = explicit_config.port == 7498
  io.println("  Result: " <> bool_to_string(explicit_test))
  io.println("")

  // Summary
  io.println("=== Test Summary ===")
  let all_passed =
    paper_port_test
    && paper_trading_test
    && live_readonly_port_test
    && live_readonly_trading_test
    && live_port_test
    && live_trading_test
    && explicit_test
  case all_passed {
    True -> {
      io.println("✓ All tests passed!")
      io.println("")
      io.println("Safety Features Verified:")
      io.println("  ✓ PaperTrading allows trading (port 7497)")
      io.println("  ✓ LiveTradingReadOnly blocks trading (port 7496)")
      io.println("  ✓ LiveTrading allows trading (port 7496)")
      io.println("")
      io.println("Recommended Usage:")
      io.println("  • Development: Use PaperTrading or LiveTradingReadOnly")
      io.println("  • Production: Use LiveTrading only when ready to trade")
    }
    False -> io.println("✗ Some tests failed")
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "✓ PASS"
    False -> "✗ FAIL"
  }
}
