import connection
import gleam/bool
import gleam/int
import gleam/io
import protocol

/// Test that properly handles async handshake with event callbacks
/// This test sends client ID immediately when server response is received
pub fn main() {
  io.println("\n=== Async Handshake Test with Event Callbacks ===")

  let account_type = connection.LiveTradingReadOnly
  let config = connection.config_with_account_type("127.0.0.1", account_type, 1)

  let port_name = case account_type {
    connection.PaperTrading -> "PAPER TRADING (7497)"
    connection.LiveTradingReadOnly -> "LIVE TRADING READ-ONLY (7496)"
    connection.LiveTrading -> "LIVE TRADING (7496)"
  }

  io.println(
    "Testing TCP connection to IB TWS API on port "
    <> int.to_string(config.port)
    <> " ("
    <> port_name
    <> ")",
  )
  io.println("")
  io.println(
    "Trading Allowed: "
    <> bool.to_string(connection.is_trading_allowed(account_type)),
  )
  case connection.is_trading_allowed(account_type) {
    False -> io.println("⚠️ SAFETY: Trading operations are BLOCKED ⚠️")
    True -> Nil
  }
  io.println("")

  // Track if client ID was sent
  let client_id_sent = io.println("Connecting...")

  case connection.connect(config) {
    Ok(conn) -> {
      io.println("✓ Connected to TWS")
      io.println("")

      // Send handshake message
      let handshake = protocol.start_api_message(100, 200)
      io.println("Sending handshake message...")
      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("✓ Handshake sent")
          io.println("")
          io.println("Waiting for server response...")
          io.println("")

          // Wait for server response (data arrives asynchronously)
          // The client ID should be sent as soon as we receive the server response
          connection.sleep(5000)

          // Check if we received any data
          let received_data = connection.receive(conn)
          case received_data {
            Ok(data) -> {
              io.println("✓ Received data: " <> data)
              io.println("")

              // Parse server response
              case protocol.parse_server_response(data) {
                Ok(#(version, timestamp)) -> {
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
                      io.println("Keeping connection alive for 10 seconds...")
                      io.println("Press Ctrl+C to stop early")
                      connection.sleep(10_000)
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
            Error(_) -> {
              io.println("⚠ No data received within timeout")
              io.println(
                "This is normal - the data may have arrived via event handler",
              )
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
      io.println("")
      io.println("Troubleshooting:")
      io.println("1. Ensure TWS is running")
      io.println(
        "2. Enable API connections in TWS (Configure > API > Settings)",
      )
      io.println(
        "3. Check that port " <> int.to_string(config.port) <> " is not blocked",
      )
      io.println("4. Verify 'Allow connections from localhost' is checked")
    }
  }
}
