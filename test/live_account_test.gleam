import connection
import gleam/int
import gleam/io
import protocol

/// ⚠️ WARNING: This test connects to a LIVE TRADING ACCOUNT ⚠️
/// 
/// IMPORTANT SAFETY NOTES:
/// - This uses LiveTradingReadOnly account type (port 7496)
/// - Trading operations are BLOCKED at type level
/// - Use only for connection testing and data retrieval
/// - Paper trading (port 7497) should be used for development
/// 
/// When ready for production, switch to connection.LiveTrading
pub fn main() {
  io.println("⚠️ ⚠️ ⚠️ WARNING ⚠️ ⚠️ ⚠️")
  io.println("This is a LIVE ACCOUNT connection test")
  io.println("")
  io.println("=== Live Trading Account Handshake Test ===")
  io.println(
    "Using automatic port selection (LiveTradingReadOnly -> port 7496)",
  )
  io.println("Client ID: 1")
  io.println("")

  // Use LiveTradingReadOnly for development safety
  let config =
    connection.config_with_account_type(
      "127.0.0.1",
      7496,
      connection.LiveTradingReadOnly,
      1,
    )

  // Check trading permissions
  let trading_allowed =
    connection.is_trading_allowed(connection.LiveTradingReadOnly)
  io.println("Trading Permissions: " <> bool_to_yes_no(trading_allowed))
  io.println("⚠️ Trading operations are BLOCKED for safety ⚠️")
  io.println("")

  io.println("Configuration:")
  io.println("  Host: " <> config.host)
  io.println("  Port: " <> int.to_string(config.port) <> " (LIVE TRADING)")
  io.println("  Client ID: " <> int.to_string(config.client_id))
  io.println("  Account Type: LiveTradingReadOnly (Development Mode)")
  io.println("")

  // Connect to TWS
  case connection.connect(config) {
    Ok(conn) -> {
      io.println("✓ Connected to TWS (LIVE TRADING)")
      io.println("")

      // Send handshake message
      let handshake = protocol.start_api_message(0, 200)
      io.println("Sending handshake message...")
      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("✓ Handshake sent")
          io.println("")
          io.println("Waiting for server response (up to 5 seconds)...")
          io.println("")

          // Wait for server response
          connection.sleep(5000)

          // Check if we received any data
          let received_data = connection.receive(conn)
          case received_data {
            Error(_) -> {
              io.println("⚠ No data received within timeout")
              io.println(
                "This is normal - data may have arrived via event handler",
              )
            }
            Ok(data) -> {
              io.println("✓ Received data: " <> data)

              // Parse server response
              case protocol.parse_server_response(data) {
                Ok(#(version, timestamp)) -> {
                  io.println("")
                  io.println("✓ Server response parsed:")
                  io.println("  Version: " <> int.to_string(version))
                  io.println("  Timestamp: " <> timestamp)
                  io.println("")

                  // Now send client ID as separate message
                  let client_id_msg =
                    protocol.client_id_message(config.client_id)
                  io.println(
                    "Sending client ID message (ID: "
                    <> int.to_string(config.client_id)
                    <> ")...",
                  )
                  case connection.send_bytes(conn, client_id_msg) {
                    Ok(_) -> {
                      io.println("✓ Client ID message sent")
                      io.println("")
                      io.println("✓ Handshake complete!")
                      io.println("✓ Check TWS GUI to confirm client ID appears")
                      io.println("")
                      io.println("✓ Trading is BLOCKED (LiveTradingReadOnly)")
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
                }
                Error(e) -> {
                  io.println("⚠ Could not parse server response: " <> e)
                  io.println("Raw data: " <> data)
                }
              }
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

  io.println("")
  io.println("⚠️ Test completed - NO trading operations performed ⚠️")
  io.println("⚠️ Trading was blocked by LiveTradingReadOnly type ⚠️")
  io.println("")
  io.println("When ready for production:")
  io.println(
    "  Change connection.LiveTradingReadOnly to connection.LiveTrading",
  )
}

fn bool_to_yes_no(b: Bool) -> String {
  case b {
    True -> "YES ✓"
    False -> "NO ✗"
  }
}
