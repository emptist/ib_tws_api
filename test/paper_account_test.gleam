import connection
import gleam/int
import gleam/io
import gleam/option.{Some}
import protocol

/// Test handshake on paper trading account using automatic port selection
/// Uses connection.PaperTrading which allows trading
pub fn main() {
  io.println("=== Paper Trading Account Handshake Test ===")
  io.println("Using automatic port selection (PaperTrading -> port 7497)")
  io.println("Client ID: 1")
  io.println("")

  // Check trading permissions
  let trading_allowed = connection.is_trading_allowed(connection.PaperTrading)
  io.println("Trading Permissions: " <> bool_to_yes_no(trading_allowed))
  io.println("")

  // Use automatic port selection
  let config =
    connection.config_with_account_type("127.0.0.1", connection.PaperTrading, 1)

  io.println("Configuration:")
  io.println("  Host: " <> config.host)
  io.println("  Port: " <> int.to_string(config.port))
  io.println("  Client ID: " <> int.to_string(config.client_id))
  io.println("")

  // Use callback to handle async data
  let data_callback = fn(data: String) {
    io.println("")
    io.println("[CALLBACK] Data received from server:")
    io.println(data)

    // Parse server response
    case protocol.parse_server_response(data) {
      Ok(#(version, timestamp)) -> {
        io.println("")
        io.println("✓ Server response parsed:")
        io.println("  Version: " <> int.to_string(version))
        io.println("  Timestamp: " <> timestamp)
        io.println("")
        io.println("[CALLBACK] Handshake successful!")
        io.println("[CALLBACK] Client ID should be sent immediately after this")
      }
      Error(err) -> {
        io.println("")
        io.println("[CALLBACK] Failed to parse server response:")
        io.println("- Error: " <> err)
      }
    }
  }

  // Connect to TWS with callback
  case connection.connect_with_callback(config, Some(data_callback)) {
    Ok(conn) -> {
      io.println("✓ Connected to TWS (Paper Trading)")

      // Send handshake message
      let handshake = protocol.start_api_message(0, 200)
      io.println("Sending handshake message...")
      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("✓ Handshake sent")
          io.println("")
          io.println("Waiting for server response (via callback)...")
          io.println("")

          // Wait for server to respond - callback will be invoked automatically
          io.println("Waiting 3 seconds for server response...")
          connection.sleep(3000)

          // After callback processes server response, send client ID
          io.println("")
          io.println(
            "Sending client ID message (ID: "
            <> int.to_string(config.client_id)
            <> ")...",
          )
          let client_id_msg = protocol.client_id_message(config.client_id)
          case connection.send_bytes(conn, client_id_msg) {
            Ok(_) -> {
              io.println("✓ Client ID message sent")
              io.println("")
              io.println("✓ Handshake complete!")
              io.println("✓ Check TWS GUI to confirm client ID appears")
              io.println("")
              io.println("✓ Trading is ALLOWED on this account type")
              io.println("Keeping connection alive for 5 seconds...")
              connection.sleep(5000)
            }
            Error(err) -> {
              let err_msg = case err {
                connection.ConnectionFailed(msg) -> msg
                connection.InvalidHost -> "Invalid host"
                connection.InvalidPort -> "Invalid port"
                connection.SocketError(msg) -> msg
                connection.Timeout -> "Timeout"
              }
              io.println("✗ Failed to send client ID: " <> err_msg)
            }
          }

          io.println("")
          io.println("Closing connection...")
          let _ = connection.close(conn)
          io.println("✓ Connection closed")
        }
        Error(err) -> {
          let err_msg = case err {
            connection.ConnectionFailed(msg) -> msg
            connection.InvalidHost -> "Invalid host"
            connection.InvalidPort -> "Invalid port"
            connection.SocketError(msg) -> msg
            connection.Timeout -> "Timeout"
          }
          io.println("✗ Failed to send handshake: " <> err_msg)
        }
      }
    }
    Error(err) -> {
      let err_msg = case err {
        connection.ConnectionFailed(msg) -> msg
        connection.InvalidHost -> "Invalid host"
        connection.InvalidPort -> "Invalid port"
        connection.SocketError(msg) -> msg
        connection.Timeout -> "Timeout"
      }
      io.println("✗ Failed to connect: " <> err_msg)
    }
  }
}

fn bool_to_yes_no(b: Bool) -> String {
  case b {
    True -> "YES ✓"
    False -> "NO ✗"
  }
}
