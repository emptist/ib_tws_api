import connection
import gleam/int
import gleam/io
import gleam/option.{Some}
import protocol_fixed as protocol

pub fn main() {
  io.println("=== PERSISTENT CONNECTION TEST ===")
  io.println("This test maintains connection to TWS and displays incoming data")
  io.println("Connection remains open until TWS closes it or data is received")
  io.println("")

  // Create config for paper trading
  let config = connection.config("127.0.0.1", 7497, 1)

  // Data callback to display incoming messages
  let data_callback = fn(data: String) {
    let timestamp = connection.get_timestamp()
    io.println("\n📥 [" <> timestamp <> "] SERVER DATA:")
    io.println("📥 " <> data)
    io.println("📥 [" <> timestamp <> "] END DATA\n")
  }

  io.println("1. Establishing connection...")
  case connection.connect_with_callback(config, Some(data_callback)) {
    Ok(conn) -> {
      io.println("✅ CONNECTION ESTABLISHED!")
      io.println("Client ID: " <> int.to_string(config.client_id))

      // Send API handshake
      io.println("2. Sending API handshake...")
      let handshake = protocol.start_api_message(100, 200)
      case connection.send_bytes(conn, handshake) {
        Ok(_) -> io.println("   ✅ API handshake sent")
        Error(e) -> io.println("   ❌ Error: " <> error_to_string(e))
      }

      connection.sleep(1000)

      // Send client ID
      io.println("3. Sending client ID...")
      let client_id_msg = protocol.client_id_message(config.client_id)
      case connection.send_bytes(conn, client_id_msg) {
        Ok(_) -> io.println("   ✅ Client ID sent")
        Error(e) -> io.println("   ❌ Error: " <> error_to_string(e))
      }

      connection.sleep(2000)

      io.println("")
      io.println("=== CONNECTION MAINTAINED ===")
      io.println("The connection will remain open for 5 minutes")
      io.println("Incoming data will be displayed automatically")
      io.println("Check TWS API settings if no data is received")
      io.println("")

      // Wait for data (5 minutes)
      connection.sleep(300_000)

      io.println("")
      io.println("⏰ 5 minutes completed - connection remains open")
      io.println("If no data received, check TWS API configuration")
    }
    Error(error) -> {
      io.println("❌ CONNECTION FAILED: " <> error_to_string(error))
    }
  }
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
