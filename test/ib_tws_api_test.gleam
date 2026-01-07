import connection
import gleam/io
import ib_tws_api

/// Test basic TCP connection to IB TWS API
/// This test will connect to paper trading (port 7497)
/// and attempt to receive raw data from the server
pub fn main() {
  io.println("=== IB TWS API Connection Test ===")
  io.println("")

  // Test: Connect to paper trading
  io.println("Step 1: Connecting to IB TWS Paper Trading (port 7497)...")
  let result = ib_tws_api.connect_paper("127.0.0.1", 1)

  case result {
    Ok(conn) -> {
      io.println("✓ Step 1 PASSED: Connected successfully")
      io.println("")

      // Test: Send a simple message (empty string to trigger server response)
      io.println("Step 2: Sending empty data to trigger server response...")
      let send_result = ib_tws_api.send(conn, "")
      case send_result {
        Ok(_) -> {
          io.println("✓ Step 2 PASSED: Data sent successfully")
          io.println("")

          // Test: Receive raw data from server
          io.println("Step 3: Receiving raw data from server...")
          let receive_result = ib_tws_api.receive(conn)
          case receive_result {
            Ok(data) -> {
              io.println("✓ Step 3 PASSED: Received raw data")
              io.println("")
              io.println("--- Raw Data Received ---")
              io.println(data)
              io.println("--- End of Raw Data ---")
              io.println("")

              // Test: Close connection
              io.println("Step 4: Closing connection...")
              let close_result = ib_tws_api.close(conn)
              case close_result {
                Ok(_) -> {
                  io.println("✓ Step 4 PASSED: Connection closed successfully")
                  io.println("")
                  io.println("=== All Tests PASSED ===")
                }
                Error(_) -> {
                  io.println("✗ Step 4 FAILED: Could not close connection")
                }
              }
            }
            Error(err) -> {
              io.println("✗ Step 3 FAILED: Could not receive data")
              io.println("Error: " <> error_to_string(err))
              io.println("")
              io.println(
                "Note: IB TWS API may not send data without proper handshake",
              )
              io.println(
                "This is expected behavior for a minimal TCP connection test",
              )
            }
          }
        }
        Error(err) -> {
          io.println("✗ Step 2 FAILED: Could not send data")
          io.println("Error: " <> error_to_string(err))
        }
      }
    }
    Error(err) -> {
      io.println("✗ Step 1 FAILED: Could not connect")
      io.println("Error: " <> error_to_string(err))
      io.println("")
      io.println("Note: This is expected if TWS Gateway is not running")
      io.println("To run this test with actual connection:")
      io.println("1. Start TWS or IB Gateway")
      io.println("2. Enable API connections on port 7497")
      io.println("3. Run this test again")
    }
  }
}

fn error_to_string(err: connection.ConnectionError) -> String {
  case err {
    connection.ConnectionFailed(msg) -> "Connection failed: " <> msg
    connection.InvalidHost -> "Invalid host"
    connection.InvalidPort -> "Invalid port"
    connection.SocketError(msg) -> "Socket error: " <> msg
    connection.Timeout -> "Connection timeout"
  }
}
