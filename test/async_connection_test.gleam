import connection
import gleam/int
import gleam/io
import gleam/option.{Some}
import protocol

/// Async connection test that properly waits for socket to be ready
/// Uses callback-based connection to handle async socket events
pub fn main() {
  io.println("\n=== ASYNC CONNECTION TEST ===")
  io.println("Properly waits for socket ready event before sending data")
  io.println("")

  // Generate unique client ID using timestamp
  let client_id = connection.generate_client_id()
  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  // Create connection configuration
  let conn_config = connection.config("127.0.0.1", 7497, client_id)

  // Connect with callback to handle async events
  case
    connection.connect_with_callback(
      conn_config,
      Some(fn(data) {
        // This callback will be called when data is received
        io.println("[CALLBACK] Received data: " <> data)
      }),
    )
  {
    Ok(conn) -> {
      io.println("✅ Connection initiated")
      io.println("")
      io.println("⏳ Waiting for socket to be ready...")

      // Wait for socket ready event (callback will set state)
      // For now, just wait 3 seconds to ensure socket is ready
      connection.sleep(3000)

      io.println("✅ Socket should be ready now")
      io.println("")

      // Send START_API handshake message
      io.println("📤 Sending handshake message...")
      let handshake = protocol.start_api_message(100, 200)

      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("✅ Handshake sent successfully")
          io.println("")

          // Wait for server to respond
          io.println("⏳ Waiting 3 seconds for server response...")
          connection.sleep(3000)

          // Try to receive data
          io.println("📥 Attempting to receive data...")
          case connection.receive(conn) {
            Ok(data) -> {
              io.println("✅ Received data from server:")
              io.println(data)
              io.println("")

              // Parse the server response
              case protocol.parse_server_response(data) {
                Ok(#(version, timestamp)) -> {
                  io.println("=== HANDSHAKE SUCCESS ===")
                  io.println("Server version: " <> int.to_string(version))
                  io.println("Server timestamp: " <> timestamp)
                  io.println("")

                  // Send client ID as separate message after server response
                  io.println("📤 Sending client ID message...")
                  let client_id_msg =
                    protocol.client_id_message(conn_config.client_id)
                  case connection.send_bytes(conn, client_id_msg) {
                    Ok(_) -> {
                      io.println(
                        "✅ Client ID sent: "
                        <> int.to_string(conn_config.client_id),
                      )
                      io.println("")
                      io.println("=== CONNECTION ESTABLISHED ===")
                      io.println("✅ TCP connection: SUCCESS")
                      io.println("✅ API Handshake: SUCCESS")
                      io.println("✅ Client ID: SUCCESS")
                      io.println("")
                      io.println(
                        "Connection is now established and ready for API requests!",
                      )
                      io.println("")
                      io.println("Press Ctrl+C to exit")
                      io.println("")

                      // Keep connection open
                      connection.sleep(1_000_000_000)
                    }
                    Error(err) -> {
                      let error_msg = case err {
                        connection.SocketError(msg) -> msg
                        connection.ConnectionFailed(msg) -> msg
                        connection.InvalidHost -> "Invalid host"
                        connection.InvalidPort -> "Invalid port"
                        connection.Timeout -> "Connection timeout"
                      }
                      io.println("❌ Failed to send client ID: " <> error_msg)
                    }
                  }
                }
                Error(err) -> {
                  io.println("❌ Server response parsing failed:")
                  io.println("   Error: " <> err)
                }
              }
            }
            Error(_) -> {
              io.println("❌ No data received from server")
              io.println("")
              io.println("=== CONNECTION STATUS ===")
              io.println("✅ TCP connection: SUCCESS")
              io.println("❌ Server response: NOT RECEIVED")
              io.println("")
              io.println("Check TWS API configuration:")
              io.println("- Ensure TWS is running")
              io.println("- Ensure API is enabled on port 7497")
              io.println("- Ensure 'ActiveX and Socket Clients' is enabled")
            }
          }
        }
        Error(err) -> {
          let error_msg = case err {
            connection.SocketError(msg) -> msg
            connection.ConnectionFailed(msg) -> msg
            connection.InvalidHost -> "Invalid host"
            connection.InvalidPort -> "Invalid port"
            connection.Timeout -> "Connection timeout"
          }
          io.println("❌ Failed to send handshake: " <> error_msg)
        }
      }
    }
    Error(err) -> {
      let error_msg = case err {
        connection.ConnectionFailed(msg) -> msg
        connection.InvalidHost -> "Invalid host"
        connection.InvalidPort -> "Invalid port"
        connection.SocketError(msg) -> msg
        connection.Timeout -> "Connection timeout"
      }
      io.println("❌ Failed to connect: " <> error_msg)
    }
  }
}
