import connection
import gleam/int
import gleam/io
import gleam/option.{Some}
import protocol

/// Diagnostic connection test
/// Minimal connection to understand why it's closing
pub fn main() {
  io.println("========================================")
  io.println("IB TWS API Diagnostic Test")
  io.println("========================================")
  io.println("")

  let data_callback = fn(data: String) {
    let timestamp = connection.get_timestamp()
    io.println("\n[" <> timestamp <> "] RECEIVED:")
    io.println(data)
  }

  let config = connection.config("127.0.0.1", 7497, 1001)

  io.println("Connecting to IB TWS API...")
  io.println("Host: " <> config.host)
  io.println("Port: " <> int.to_string(config.port))
  io.println("Client ID: " <> int.to_string(config.client_id))
  io.println("")

  case connection.connect_with_callback(config, Some(data_callback)) {
    Ok(conn) -> {
      io.println("✓ Connected!")
      io.println("")

      // Step 1: Send handshake
      io.println("STEP 1: Sending handshake...")
      let handshake = protocol.start_api_message(100, 200)

      case connection.send_bytes(conn, handshake) {
        Ok(_) -> io.println("✓ Handshake sent")
        Error(_) -> io.println("✗ Error")
      }

      io.println("Waiting 3 seconds...")
      connection.sleep(3000)

      // Step 2: Send client ID
      io.println("\nSTEP 2: Sending client ID...")
      let client_id_msg = protocol.client_id_message(config.client_id)

      case connection.send_bytes(conn, client_id_msg) {
        Ok(_) -> io.println("✓ Client ID sent")
        Error(_) -> io.println("✗ Error")
      }

      io.println("")
      io.println("Keeping connection open for 60 seconds...")
      io.println("Watching for any additional data...")
      io.println("")

      // Keep connection open for 1 minute
      connection.sleep(60_000)

      io.println("\nClosing connection...")
      case connection.close(conn) {
        Ok(_) -> io.println("✓ Closed")
        Error(_) -> io.println("✗ Error")
      }
    }
    Error(_) -> {
      io.println("✗ Connection failed")
    }
  }

  io.println("")
  io.println("========================================")
  io.println("Test Complete")
  io.println("========================================")
}
