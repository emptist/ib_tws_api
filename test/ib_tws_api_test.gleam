import gleam/io
import ib_tws_api

/// Simple test function to verify basic connection
pub fn main() {
  io.println("=== IB TWS API Test Suite ===")
  io.println("")

  // Test 1: Connect to paper trading
  io.println("Test 1: Connecting to paper trading...")
  let result = ib_tws_api.connect_paper("127.0.0.1", 1)

  case result {
    Ok(conn) -> {
      io.println("✓ Test 1 PASSED: Connected successfully")
      io.println("")

      // Test 2: Send data
      io.println("Test 2: Sending data...")
      let send_result = ib_tws_api.send(conn, "test data")
      case send_result {
        Ok(_) -> {
          io.println("✓ Test 2 PASSED: Data sent successfully")
          io.println("")

          // Test 3: Receive data
          io.println("Test 3: Receiving data...")
          let receive_result = ib_tws_api.receive(conn)
          case receive_result {
            Ok(data) -> {
              io.println("✓ Test 3 PASSED: Received data: " <> data)
              io.println("")

              // Test 4: Close connection
              io.println("Test 4: Closing connection...")
              let close_result = ib_tws_api.close(conn)
              case close_result {
                Ok(_) -> {
                  io.println("✓ Test 4 PASSED: Connection closed successfully")
                  io.println("")
                  io.println("=== All Tests PASSED ===")
                }
                Error(_) -> {
                  io.println("✗ Test 4 FAILED: Could not close connection")
                }
              }
            }
            Error(_) -> {
              io.println("✗ Test 3 FAILED: Could not receive data")
            }
          }
        }
        Error(_) -> {
          io.println("✗ Test 2 FAILED: Could not send data")
        }
      }
    }
    Error(_) -> {
      io.println("✗ Test 1 FAILED: Could not connect")
      io.println("")
      io.println("Note: This is expected if TWS Gateway is not running")
      io.println("To run tests with actual connection:")
      io.println("1. Start TWS Gateway")
      io.println("2. Enable API connections on port 7497")
      io.println("3. Run this test again")
    }
  }
}
