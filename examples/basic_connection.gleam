import connection
import gleam/int
import gleam/io
import gleam/option
import protocol

/// Basic connection example
/// Demonstrates how to connect to IB TWS, perform handshake, and receive data
pub fn main() {
  io.println("=== Basic Connection Example ===")
  io.println("")

  // Generate a unique client ID
  let client_id = connection.generate_client_id()
  io.println("Generated client ID: " <> int.to_string(client_id))
  io.println("")

  // Create configuration with paper trading account (port 7497)
  let config =
    connection.config_with_account_type(
      "127.0.0.1",
      7497,
      connection.PaperTrading,
      client_id,
    )

  io.println("Connecting to IB TWS...")
  io.println("Host: " <> config.host)
  io.println("Port: " <> int.to_string(config.port))
  io.println("Client ID: " <> int.to_string(config.client_id))
  io.println("")

  // Connect with a callback to handle incoming messages
  let result =
    connection.connect_with_callback(
      config,
      Some(fn(data) { io.println("Received: " <> data) }),
    )

  case result {
    Ok(conn) -> {
      io.println("✓ Connected successfully")
      io.println("")

      // Send V100+ handshake
      io.println("Sending V100+ handshake...")
      let handshake = protocol.start_api_message()
      case connection.send_bytes(conn, handshake) {
        Ok(_) -> io.println("✓ Handshake sent")
        Error(err) -> io.println("✗ Handshake failed: " <> err)
      }
      io.println("")

      // Wait for server response
      io.println("Waiting for server response...")
      connection.sleep(2000)
      io.println("")

      // Send client ID
      io.println("Sending client ID...")
      let client_id_msg = protocol.client_id_message(config.client_id)
      case connection.send_bytes(conn, client_id_msg) {
        Ok(_) -> io.println("✓ Client ID sent")
        Error(err) -> io.println("✗ Client ID failed: " <> err)
      }
      io.println("")

      // Wait for a bit to receive data
      io.println("Waiting for data (5 seconds)...")
      connection.sleep(5000)
      io.println("")

      // Close connection
      io.println("Closing connection...")
      case connection.close(conn) {
        Ok(_) -> io.println("✓ Connection closed")
        Error(err) -> io.println("✗ Close failed: " <> err)
      }
    }
    Error(err) -> {
      io.println("✗ Connection failed: " <> err)
      io.println("")
      io.println("Make sure IB TWS or IB Gateway is running with API enabled:")
      io.println("- Paper Trading: Port 7497")
      io.println("- Live Trading: Port 7496")
    }
  }

  io.println("")
  io.println("=== Example Complete ===")
}
