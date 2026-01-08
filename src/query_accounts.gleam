import account_data
import connection
import gleam/io
import gleam/list
import message_encoder

/// Query and display all available accounts from IB TWS API
pub fn main() {
  io.println("========================================")
  io.println("Querying Available Accounts")
  io.println("========================================")

  // Connect to IB TWS API (paper trading)
  let config = connection.config("127.0.0.1", 7497, 1001)

  io.println("\nConnecting to IB TWS API...")
  case connection.connect(config) {
    Ok(conn) -> {
      io.println("✓ Connected successfully!")

      // Request account summary with common tags
      io.println("\nRequesting account summary...")
      let tags = account_data.common_account_tags()
      let msg = account_data.request_account_summary(5001, "All", tags)
      let msg_bytes = message_encoder.add_length_prefix_to_string(msg)

      case connection.send_bytes(conn, msg_bytes) {
        Ok(_) -> {
          io.println("✓ Account summary request sent")
          io.println("\nNote: Account data will be received via callbacks")
          io.println("Please check your TWS GUI for account information")
          io.println("\nExpected account format: UXXXXXX or DUXXXXXX")
        }
        Error(_) -> io.println("✗ Error sending request")
      }

      // Request positions
      io.println("\nRequesting positions...")
      let pos_msg = account_data.request_positions(5002)
      // Added request_id parameter
      let pos_bytes = message_encoder.add_length_prefix_to_string(pos_msg)

      case connection.send_bytes(conn, pos_bytes) {
        Ok(_) -> {
          io.println("✓ Positions request sent")
          io.println("\nPosition data will be streamed")
        }
        Error(_) -> io.println("✗ Error sending request")
      }

      // Keep connection open for a moment to receive data
      io.println("\nWaiting for account data...")
      connection.sleep(3000)

      // Close connection
      io.println("\nClosing connection...")
      case connection.close(conn) {
        Ok(_) -> io.println("✓ Connection closed")
        Error(_) -> io.println("✗ Error closing connection")
      }
    }
    Error(_) -> {
      io.println("✗ Connection failed")
      io.println("\nPlease ensure:")
      io.println("  - IB TWS is running")
      io.println("  - API connections are enabled in TWS settings")
      io.println("  - Port 7497 is available (paper trading)")
    }
  }

  io.println("\n========================================")
  io.println("Query Complete")
  io.println("========================================")
}
