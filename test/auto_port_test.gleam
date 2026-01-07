import connection
import gleam/int
import gleam/io
import protocol

/// Test automatic port switching based on account type
/// This demonstrates the new config_with_account_type() function
pub fn main() {
  io.println("=== Automatic Port Switching Test ===")
  io.println("")

  // Test 1: Paper trading account (should use port 7497)
  io.println("Test 1: Paper Trading Account")
  let paper_config =
    connection.config_with_account_type("127.0.0.1", connection.PaperTrading, 1)
  io.println("  Account Type: PaperTrading")
  io.println("  Expected Port: 7497")
  io.println("  Actual Port: " <> int.to_string(paper_config.port))
  let paper_test = paper_config.port == 7497
  io.println("  Result: " <> bool_to_string(paper_test))
  io.println("")

  // Test 2: Live trading account (should use port 7496)
  io.println("Test 2: Live Trading Account")
  let live_config =
    connection.config_with_account_type("127.0.0.1", connection.LiveTrading, 1)
  io.println("  Account Type: LiveTrading")
  io.println("  Expected Port: 7496")
  io.println("  Actual Port: " <> int.to_string(live_config.port))
  let live_test = live_config.port == 7496
  io.println("  Result: " <> bool_to_string(live_test))
  io.println("")

  // Test 3: Explicit port (still works)
  io.println("Test 3: Explicit Port Configuration")
  let explicit_config = connection.config("127.0.0.1", 7498, 1)
  io.println("  Explicit Port: 7498")
  io.println("  Actual Port: " <> int.to_string(explicit_config.port))
  let explicit_test = explicit_config.port == 7498
  io.println("  Result: " <> bool_to_string(explicit_test))
  io.println("")

  // Summary
  io.println("=== Test Summary ===")
  let all_passed = paper_test && live_test && explicit_test
  case all_passed {
    True -> io.println("✓ All tests passed!")
    False -> io.println("✗ Some tests failed")
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "✓ PASS"
    False -> "✗ FAIL"
  }
}
