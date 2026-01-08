import connection
import gleam/bool
import gleam/int
import gleam/io
import gleam/option.{Some}
import gleam/result
import protocol

/// Capture and display ALL raw data from IB TWS API
/// This script listens for several minutes to understand API behavior
pub fn main() {
  io.println("========================================")
  io.println("IB TWS API Data Capture")
  io.println("========================================")
  io.println("")
  io.println("This will capture ALL incoming data with timestamps")
  io.println("Run for 5 minutes to observe API behavior")
  io.println("")

  // Create a callback to capture received data with timestamp
  // Also save to log file for investigation
  let data_callback = fn(data: String) {
    let timestamp = connection.get_timestamp()
    let log_message =
      "["
      <> timestamp
      <> "] RECEIVED DATA:\n"
      <> data
      <> "\n"
      <> "["
      <> timestamp
      <> "] END DATA\n"
      <> "========================================\n"

    io.println("\n" <> log_message)

    // Write to log file for investigation
    case connection.write_to_file("api_capture_log.txt", log_message, True) {
      Ok(_) -> io.println("✓ Log written to api_capture_log.txt")
      Error(error) -> io.println("✗ Error writing to log file: " <> error)
    }
  }

  // Connect to IB TWS API (paper trading)
  let config = connection.config("127.0.0.1", 7497, 1001)

  io.println("Connecting to IB TWS API...")
  io.println("Host: " <> config.host)
  io.println("Port: " <> int.to_string(config.port))
  io.println("Client ID: " <> int.to_string(config.client_id))
  io.println("")

  case connection.connect_with_callback(config, Some(data_callback)) {
    Ok(conn) -> {
      io.println("✓ Connected successfully!")
      io.println("")

      // Perform IB TWS V100+ handshake
      io.println("Performing IB TWS V100+ handshake...")
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

      io.println("")
      io.println("========================================")
      io.println("NOW LISTENING FOR ALL DATA")
      io.println("========================================")
      io.println("")
      io.println("Duration: 30 seconds (reduced for demo)")
      io.println("All incoming data will be printed with timestamps")
      io.println("Logs saved to: api_capture_log.txt")
      io.println("========================================")
      io.println("")

      // Listen for 30 seconds for demo purposes
      connection.sleep(30_000)

      io.println("")
      io.println("========================================")
      io.println("Capture Complete")
      io.println("========================================")
      io.println("")

      // Close connection
      io.println("Closing connection...")
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

  io.println("")
  io.println("========================================")
  io.println("Session Complete")
  io.println("========================================")
  io.println("")
}
