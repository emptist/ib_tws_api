import account_data
import connection
import gleam/int
import gleam/io
import gleam/option.{Some}
import gleam/result
import message_encoder
import protocol_fixed as protocol

pub fn main() {
  io.println("========================================")
  io.println("IB TWS API Immediate Response Test")
  io.println("========================================")
  io.println("")

  // Create connection config
  let config = connection.config("127.0.0.1", 7497, 1001)

  io.println("Connecting to IB TWS API...")
  io.println("Host: " <> config.host)
  io.println("Port: " <> int.to_string(config.port))
  io.println("Client ID: " <> int.to_string(config.client_id))
  io.println("")

  // Connect with callback to capture all received data
  let data_callback = fn(data: String) {
    let timestamp = connection.get_timestamp()
    io.println("\n[" <> timestamp <> "] DATA CALLBACK:")
    io.println(data)
    io.println("[" <> timestamp <> "] END DATA\n")
  }

  case connection.connect_with_callback(config, Some(data_callback)) {
    Ok(conn) -> {
      io.println("✓ Connected!")
      io.println("")

      // Step 1: Send handshake IMMEDIATELY
      io.println("STEP 1: Sending handshake...")
      let handshake = message_encoder.start_api_message(config.client_id)
      let handshake_bytes =
        message_encoder.add_length_prefix_to_string(handshake)

      case connection.send_bytes(conn, handshake_bytes) {
        Ok(_) -> io.println("✓ Handshake sent")
        Error(_) -> io.println("✗ Error sending handshake")
      }

      // Wait briefly for server response (500ms)
      io.println("Waiting for server response...")
      connection.sleep(500)

      // Step 2: Send client ID IMMEDIATELY after handshake response
      io.println("\nSTEP 2: Sending client ID immediately...")
      let client_id_msg = protocol.client_id_message(config.client_id)

      case connection.send_bytes(conn, client_id_msg) {
        Ok(_) -> io.println("✓ Client ID sent")
        Error(_) -> io.println("✗ Error sending client ID")
      }

      // Step 3: Request managed accounts
      io.println("\nSTEP 3: Requesting managed accounts...")
      let msg = account_data.request_managed_accounts()
      let msg_bytes = message_encoder.add_length_prefix_to_string(msg)

      case connection.send_bytes(conn, msg_bytes) {
        Ok(_) -> io.println("✓ Managed accounts request sent")
        Error(_) -> io.println("✗ Error sending request")
      }

      io.println("")
      io.println("Keeping connection open for 60 seconds...")
      io.println("Watching for account data...")
      io.println("")

      // Keep connection open for 1 minute to receive data
      connection.sleep(60_000)

      // Close connection
      io.println("")
      io.println("Closing connection...")
      case connection.close(conn) {
        Ok(_) -> io.println("✓ Closed")
        Error(_) -> io.println("✗ Error closing")
      }
    }
    Error(error) -> {
      io.println("✗ Connection failed: " <> error_to_string(error))
    }
  }

  io.println("")
  io.println("========================================")
  io.println("Test Complete")
  io.println("========================================")
}

fn error_to_string(error: connection.ConnectionError) -> String {
  case error {
    connection.ConnectionFailed(msg) -> "Connection failed: " <> msg
    connection.InvalidHost -> "Invalid host"
    connection.InvalidPort -> "Invalid port"
    connection.SocketError(msg) -> "Socket error: " <> msg
    connection.Timeout -> "Timeout"
  }
}
