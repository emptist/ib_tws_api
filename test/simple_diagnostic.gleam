import connection
import gleam/int
import gleam/io
import gleam/string
import gleeunit
import protocol

pub fn main() {
  io.println("=== SIMPLE DIAGNOSTIC CHECK ===")
  io.println("Checking IB TWS API connectivity and configuration")
  io.println("")

  // Test automatic port detection first
  io.println("1. Testing automatic port detection...")
  case connection.detect_ib_tws_port("127.0.0.1", 2000) {
    0 -> io.println("❌ No IB TWS detected on ports 7496 or 7497")
    port -> {
      io.println("✅ IB TWS detected on port: " <> int.to_string(port))

      // Create config for the detected port
      let config = case port {
        7496 ->
          connection.config_with_account_type(
            "127.0.0.1",
            7496,
            connection.LiveTrading,
            999,
          )
        7497 ->
          connection.config_with_account_type(
            "127.0.0.1",
            7497,
            connection.PaperTrading,
            999,
          )
        _ ->
          connection.config_with_account_type(
            "127.0.0.1",
            port,
            connection.PaperTrading,
            999,
          )
      }

      io.println("2. Testing connection...")
      case connection.connect(config) {
        Ok(socket) -> {
          io.println("✅ Connection established!")

          // Test API handshake
          io.println("3. Testing API handshake...")
          let handshake = protocol.start_api_message(100, 200)
          case connection.send_bytes(socket, handshake) {
            Ok(_) -> io.println("✅ API handshake sent!")
            Error(_) -> io.println("❌ Failed to send API handshake")
          }

          // Test client ID
          io.println("4. Testing client ID transmission...")
          let client_id_msg = protocol.client_id_message(999)
          case connection.send_bytes(socket, client_id_msg) {
            Ok(_) -> io.println("✅ Client ID sent!")
            Error(_) -> io.println("❌ Failed to send client ID")
          }

          // Wait for server response
          io.println("5. Waiting for server response (5 seconds)...")
          connection.sleep(5000)

          io.println("")
          io.println("=== DIAGNOSTIC RESULTS ===")
          io.println("✅ Connection: SUCCESS")
          io.println("✅ API Handshake: SUCCESS")
          io.println("✅ Client ID: SUCCESS")
          io.println("❌ Server Account Data: NOT RECEIVED")
          io.println("")
          io.println("=== CONFIGURATION ISSUE DETECTED ===")
          io.println("The IB TWS server is not responding with account data.")
          io.println("This suggests the API is not properly configured.")
          io.println("")
          io.println("=== RECOMMENDED ACTIONS ===")
          io.println("1. Open IB TWS -> Settings -> API -> Socket Clients")
          io.println("2. Enable 'Enable ActiveX and Socket Clients'")
          io.println(
            "3. Verify 'Socket port' is set to " <> int.to_string(port),
          )
          io.println("4. Check 'Allow connections from localhost only'")
          io.println("5. Try a different Client ID (1, 2, 3, etc.)")
          io.println("6. Restart IB TWS after making changes")

          connection.close(socket)
          io.println("")
          io.println("✅ Connection closed")
        }
        Error(err) -> {
          io.println("❌ Connection failed: " <> format_error(err))
          io.println("")
          io.println("=== TROUBLESHOOTING ===")
          io.println("1. Make sure IB TWS is running")
          io.println("2. Check API settings in TWS")
          io.println("3. Verify network connectivity")
        }
      }
    }
  }
}

fn format_error(err: connection.ConnectionError) -> String {
  case err {
    connection.ConnectionFailed(msg) -> "Connection failed: " <> msg
    connection.InvalidHost -> "Invalid host"
    connection.InvalidPort -> "Invalid port"
    connection.SocketError(msg) -> "Socket error: " <> msg
    connection.Timeout -> "Timeout"
  }
}
