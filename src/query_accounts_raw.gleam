import account_data
import connection
import gleam/int
import gleam/io
import gleam/option.{Some}
import gleam/string
import protocol

/// Query and display raw account data from IB TWS API
/// This will show all received data including account IDs
pub fn main() {
  io.println("========================================")
  io.println("Querying Available Accounts (Raw)")
  io.println("========================================\n")

  // Create a callback to capture received data
  let data_callback = fn(data: String) {
    io.println("\n>>> RECEIVED DATA <<<")
    io.println(data)
    io.println(">>> END DATA <<<\n")
  }

  // Connect to IB TWS API (paper trading) with callback
  let config = connection.config("127.0.0.1", 7497, 1001)

  io.println("Connecting to IB TWS API...")
  io.println("Port: " <> int.to_string(config.port))
  io.println("")

  case connection.connect_with_callback(config, Some(data_callback)) {
    Ok(conn) -> {
      io.println("✓ Connected successfully!")

      // Perform IB TWS V100+ handshake
      io.println("\nPerforming IB TWS V100+ handshake...")
      let handshake_msg = protocol.start_api_message(100, 200)

      case connection.send_bytes(conn, handshake_msg) {
        Ok(_) -> io.println("✓ Handshake message sent")
        Error(_) -> io.println("✗ Error sending handshake")
      }

      // Wait for server response
      io.println("Waiting for server response (2 seconds)...")
      connection.sleep(2000)

      // Send client ID
      io.println("\nSending client ID...")
      let client_id_msg = protocol.client_id_message(config.client_id)

      case connection.send_bytes(conn, client_id_msg) {
        Ok(_) -> io.println("✓ Client ID sent")
        Error(_) -> io.println("✗ Error sending client ID")
      }

      // Wait a moment for connection to be ready
      io.println("Waiting for connection to be ready (1 second)...")
      connection.sleep(1000)

      // Try both managed accounts and account summary
      io.println("\nRequesting managed accounts list...")
      let managed_accts_msg = account_data.request_managed_accounts()

      case connection.send_bytes(conn, managed_accts_msg) {
        Ok(_) -> io.println("✓ Managed accounts request sent")
        Error(_) -> io.println("✗ Error sending request")
      }

      io.println("\nAlso requesting account summary...")
      let req_id = 5001
      let tags = account_data.common_account_tags()
      let acc_summary_msg =
        account_data.request_account_summary(req_id, "All", tags)

      case connection.send_bytes(conn, acc_summary_msg) {
        Ok(_) -> io.println("✓ Account summary request sent")
        Error(_) -> io.println("✗ Error sending request")
      }

      // Keep connection open longer to receive all data
      io.println("\nWaiting for account data (30 seconds)...")
      io.println(
        "Account IDs should appear below (format: UXXXXXX or DUXXXXXX)",
      )
      io.println("Any error messages will also appear below")
      connection.sleep(30_000)

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
  io.println("")
  io.println(
    "If you see account IDs above, please verify they match your TWS GUI",
  )
  io.println("")
}
