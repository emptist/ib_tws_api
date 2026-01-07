import connection
import gleam/int
import gleam/io
import gleam/option.{Some}
import protocol

pub fn main() {
  io.println("=== Automatic Port Detection Test ===")
  io.println("")

  io.println("Detecting available IB TWS port...")
  io.println("")

  // Detect which port is available (timeout in seconds)
  let detected_port = connection.detect_ib_tws_port("127.0.0.1", 1)

  case detected_port {
    0 -> {
      io.println("❌ No IB TWS server detected on ports 7496 or 7497")
      io.println("")
      io.println("Please ensure:")
      io.println("  1. IB TWS or IB Gateway is running")
      io.println("  2. API connections are enabled in TWS configuration")
      io.println("  3. Port 7496 (live) or 7497 (paper) is configured")
    }
    port -> {
      io.println("✓ Detected IB TWS server on port: " <> int.to_string(port))
      io.println("")

      // Determine account type based on port
      let account_type = case port {
        7497 -> "Paper Trading"
        7496 -> "Live Trading"
        _ -> "Unknown"
      }

      io.println("Account Type: " <> account_type)
      io.println("")

      // Test connection with detected port
      io.println("Testing connection to detected port...")

      let client_id = connection.generate_client_id()
      io.println("Client ID: " <> int.to_string(client_id))
      io.println("")

      let config = case
        connection.config_auto_detect("127.0.0.1", client_id, 1)
      {
        Ok(cfg) -> cfg
        Error(err) -> {
          io.println("❌ " <> err)
          io.println("")
          // Exit early if config failed
          panic
        }
      }

      // Create a callback to handle server response
      let data_callback = fn(data: String) {
        io.println("✓ Server response received: " <> data)

        // Try to parse the server response
        case protocol.parse_server_response(data) {
          Ok(#(version, timestamp)) -> {
            io.println("")
            io.println("✓ Server response parsed:")
            io.println("  Server Version: " <> int.to_string(version))
            io.println("  Server Time: " <> timestamp)
            io.println("")
          }
          Error(err) -> {
            io.println("")
            io.println("⚠ Could not parse server response:")
            io.println("  " <> err)
            io.println("")
          }
        }
      }

      case connection.connect_with_callback(config, Some(data_callback)) {
        Ok(conn) -> {
          io.println("✓ Connected to TWS (detected port)")
          io.println("")

          // Send V100+ handshake
          let handshake = protocol.start_api_message(0, 200)
          case connection.send_bytes(conn, handshake) {
            Ok(_) -> {
              io.println("✓ Handshake sent")
              io.println("")
            }
            Error(err) -> {
              io.println(
                "❌ Failed to send handshake: "
                <> connection_error_to_string(err),
              )
            }
          }

          // Wait for server response
          io.println("Waiting for server response (via callback)...")
          connection.sleep(3000)
          io.println("")

          // Send client ID after receiving server response
          io.println("Sending client ID message...")
          let client_id_msg = protocol.client_id_message(client_id)
          case connection.send_bytes(conn, client_id_msg) {
            Ok(_) -> {
              io.println("✓ Client ID message sent")
              io.println("")
            }
            Error(err) -> {
              io.println(
                "❌ Failed to send client ID: "
                <> connection_error_to_string(err),
              )
            }
          }

          io.println("✓ Handshake complete!")
          io.println("✓ Check TWS GUI to confirm client ID appears")
          io.println("")

          // Keep connection alive briefly
          io.println("Keeping connection alive for 3 seconds...")
          connection.sleep(3000)
          io.println("")

          // Close connection
          io.println("Closing connection...")
          case connection.close(conn) {
            Ok(_) -> io.println("✓ Connection closed")
            Error(_) -> io.println("⚠ Connection close warning")
          }
        }
        Error(err) -> {
          io.println("❌ Failed to connect: " <> connection_error_to_string(err))
        }
      }
    }
  }
}

fn connection_error_to_string(err: connection.ConnectionError) -> String {
  case err {
    connection.ConnectionFailed(msg) -> "Connection failed: " <> msg
    connection.InvalidHost -> "Invalid host"
    connection.InvalidPort -> "Invalid port"
    connection.SocketError(msg) -> "Socket error: " <> msg
    connection.Timeout -> "Connection timeout"
  }
}
