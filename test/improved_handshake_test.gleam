import connection
import gleam/int
import gleam/io
import protocol

/// Improved test that properly handles async message reception
/// This test waits for server response before sending client ID
pub fn main() {
  io.println("=== Improved IB TWS API Handshake Test ===")
  io.println("Target: Paper Trading Account (port 7497)")
  io.println("Client ID: 1")
  io.println("")

  let config =
    connection.ConnectionConfig(host: "127.0.0.1", port: 7497, client_id: 1)

  // Connect to TWS with custom event handlers
  case connection.connect(config) {
    Ok(conn) -> {
      io.println("✓ Connected to TWS")

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
                "This is normal - the data may have arrived via event handler",
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
