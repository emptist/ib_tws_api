import connection
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api

/// Test basic TCP connection to IB TWS API
/// This test will connect to paper trading (port 7497)
/// and attempt to receive raw data from the server
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
      io.println("✓ [Step 1] SUCCESS: Connected to IB TWS API")
      io.println("")

      // Test: Send a simple message (empty string to trigger server response)
      io.println("[Step 2] Sending data to server...")
      io.println("  Data: (empty string to trigger response)")
      let send_result = ib_tws_api.send(conn, "")
      case send_result {
        Ok(_) -> {
          io.println("✓ [Step 2] SUCCESS: Data sent to server")
          io.println("")

          // Test: Receive raw data from server
          io.println("[Step 3] Waiting to receive data from server...")
          io.println("  Timeout: 5 seconds")
          io.println("")

          let receive_result = ib_tws_api.receive(conn)
          case receive_result {
            Ok(data) -> {
              io.println("✓ [Step 3] SUCCESS: Received data from server")
              io.println("")
              io.println("==========================================")
              io.println("RAW DATA RECEIVED FROM IB TWS API:")
              io.println("==========================================")
              io.println("")
              io.println(data)
              io.println("")
              io.println("==========================================")
              io.println("END OF RAW DATA")
              io.println("==========================================")
              io.println("")
              io.println(
                "Data length: "
                <> int.to_string(string.length(data))
                <> " bytes",
              )
              io.println("")

              // Test: Close connection
              io.println("[Step 4] Closing connection...")
              let close_result = ib_tws_api.close(conn)
              case close_result {
                Ok(_) -> {
                  io.println("✓ [Step 4] SUCCESS: Connection closed")
                  io.println("")
                  io.println("==========================================")
                  io.println("ALL TESTS PASSED!")
                  io.println("==========================================")
                }
                Error(err) -> {
                  io.println("✗ [Step 4] FAILED: Could not close connection")
                  io.println("  Error: " <> error_to_string(err))
                }
              }
            }
            Error(err) -> {
              io.println("✗ [Step 3] FAILED: Could not receive data")
              io.println("  Error: " <> error_to_string(err))
              io.println("")
              io.println("Note: IB TWS API typically requires proper handshake")
              io.println("before sending data. This is expected for a minimal")
              io.println("TCP connection test without protocol implementation.")
              io.println("")

              // Still try to close the connection
              io.println("[Cleanup] Attempting to close connection...")
              let _ = ib_tws_api.close(conn)
              io.println("✓ Connection closed")
            }
          }
        }
        Error(err) -> {
          io.println("✗ [Step 2] FAILED: Could not send data")
          io.println("  Error: " <> error_to_string(err))
          io.println("")

          // Try to close the connection
          io.println("[Cleanup] Attempting to close connection...")
          let _ = ib_tws_api.close(conn)
          io.println("✓ Connection closed")
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
