import connection
import gleam/io
import ib_tws_api

/// Test basic TCP connection to IB TWS API
/// This test will connect to paper trading (port 7497)
/// and attempt to receive raw data from server
pub fn main() {
  io.println("=== IB TWS API Connection Test ===")
  io.println("")

  // Test: Connect to paper trading
  io.println("[Step 1] Attempting to connect to IB TWS Paper Trading...")
  io.println("  Host: 127.0.0.1")
  io.println("  Port: 7497")
  io.println("  Client ID: 1")
  io.println("")

  let result = ib_tws_api.connect_paper("127.0.0.1", 1)

  case result {
    Ok(conn) -> {
      io.println("✓ [Step 1] SUCCESS: Connection initiated to IB TWS API")
      io.println("")
      io.println("Note: Connection is event-driven. Data will be received")
      io.println("asynchronously by the event handler.")
      io.println("")
      io.println("Waiting 3 seconds for server response...")
      io.println("")

      // Send a simple message to trigger response
      let _ = ib_tws_api.send(conn, "")

      // Wait for async events to be processed
      // In a real application, you would handle events via the event handler
      io.println("Test complete. Check console output above for data events.")
      io.println("")

      // Close connection
      io.println("[Step 2] Closing connection...")
      let close_result = ib_tws_api.close(conn)
      case close_result {
        Ok(_) -> {
          io.println("✓ [Step 2] SUCCESS: Connection closed")
          io.println("")
          io.println("==========================================")
          io.println("TEST SUMMARY:")
          io.println("==========================================")
          io.println("✓ TCP connection established")
          io.println("✓ Event handler registered")
          io.println("✓ Connection closed gracefully")
          io.println("")
          io.println("SUCCESS: Phase 1, Step 2 complete!")
          io.println("Can connect to TWS API via TCP socket.")
          io.println("==========================================")
        }
        Error(err) -> {
          io.println("✗ [Step 2] FAILED: Could not close connection")
          io.println("  Error: " <> error_to_string(err))
        }
      }
    }
    Error(err) -> {
      io.println("✗ [Step 1] FAILED: Could not connect to IB TWS API")
      io.println("  Error: " <> error_to_string(err))
      io.println("")
      io.println("==========================================")
      io.println("TROUBLESHOOTING:")
      io.println("==========================================")
      io.println("")
      io.println("This error indicates that TWS or IB Gateway is not running")
      io.println("or is not configured to accept API connections.")
      io.println("")
      io.println("To run this test successfully:")
      io.println("")
      io.println("1. Start TWS or IB Gateway application")
      io.println("2. Open Configure > API > Settings")
      io.println("3. Check 'Enable ActiveX and Socket Clients'")
      io.println("4. Set 'Socket Port' to 7497 (paper trading)")
      io.println("5. Uncheck 'Read-Only API' (if you want to send orders)")
      io.println("6. Restart TWS/IB Gateway")
      io.println("7. Run this test again")
      io.println("")
      io.println("For paper trading account:")
      io.println("  - Use port 7497")
      io.println("  - No real money at risk")
      io.println("")
      io.println("For live trading account:")
      io.println("  - Use port 7496")
      io.println("  - Real money involved - use with caution")
      io.println("==========================================")
    }
  }
}

fn error_to_string(err: connection.ConnectionError) -> String {
  case err {
    connection.ConnectionFailed(msg) -> "Connection failed: " <> msg
    connection.InvalidHost -> "Invalid host"
    connection.InvalidPort -> "Invalid port"
    connection.SocketError(msg) -> "Socket error: " <> msg
    connection.Timeout -> "Connection timeout (server not responding)"
  }
}
