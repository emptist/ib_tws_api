import gleam/io
import ib_tws_api

pub fn main() {
  io.println("=== IB TWS API Basic Connection Example ===")
  io.println("")

  // Connect to paper trading account (port 7497)
  io.println("Attempting to connect to IB TWS Paper Trading...")
  io.println("Make sure TWS Gateway is running on port 7497")
  io.println("")

  let result = ib_tws_api.connect_paper("127.0.0.1", 1)

  case result {
    Ok(conn) -> {
      io.println("✓ Connected successfully!")
      io.println("")

      // Send some test data
      io.println("Sending test data...")
      let send_result = ib_tws_api.send(conn, "test message")
      case send_result {
        Ok(_) -> io.println("✓ Data sent successfully")
        Error(_) -> io.println("✗ Send failed")
      }
      io.println("")

      // Receive data
      io.println("Receiving data...")
      let receive_result = ib_tws_api.receive(conn)
      case receive_result {
        Ok(data) -> {
          io.println("✓ Data received:")
          io.println("  Raw data: " <> data)
        }
        Error(_) -> io.println("✗ Receive failed")
      }
      io.println("")

      // Close connection
      io.println("Closing connection...")
      let close_result = ib_tws_api.close(conn)
      case close_result {
        Ok(_) -> io.println("✓ Connection closed successfully")
        Error(_) -> io.println("✗ Close failed")
      }
      io.println("")

      io.println("=== Example Complete ===")
    }
    Error(_) -> {
      io.println("✗ Connection failed")
      io.println("")
      io.println("Troubleshooting:")
      io.println("1. Make sure TWS Gateway is running")
      io.println("2. Check that API connections are enabled in TWS")
      io.println("3. Verify port 7497 is not blocked by firewall")
      io.println("4. Check that no other client is using client ID 1")
    }
  }
}
