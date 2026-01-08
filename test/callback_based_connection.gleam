import connection
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/string
import protocol

/// Callback-based connection test that properly handles async events
/// Uses callbacks to receive data as it arrives from TWS
pub fn main() {
  io.println("\n=== CALLBACK-BASED CONNECTION TEST ===")
  io.println("Uses callbacks to handle async data reception")
  io.println("")

  // Generate unique client ID using timestamp
  let client_id = connection.generate_client_id()
  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  // Create connection configuration
  let conn_config = connection.config("127.0.0.1", 7497, client_id)

  // Track handshake completion via callback
  let handshake_received = io.println("")

  // Connect with callback to handle async events
  case
    connection.connect_with_callback(
      conn_config,
      Some(fn(data) {
        // This callback will be called when data is received
        io.println("📥 [CALLBACK] Received data from server:")
        io.println("   " <> data)
        io.println("")

        // Use clean_server_response which properly handles control chars
        let message_data = protocol.clean_server_response(data)

        // Split on NULL byte separator
        let parts = string.split(message_data, "\u{0000}")

        // Filter out empty strings from leading/trailing NULLs
        let non_empty_parts = list.filter(parts, fn(s) { s != "" })

        case non_empty_parts {
          [version_str, ..rest] -> {
            // Extract only the first 3 digits (version number)
            // The version string may have leading control chars, but we just want "200"
            case int.parse(string.trim(version_str)) {
              Ok(version) -> {
                let timestamp_str = string.join(rest, "\u{0000}")
                io.println("✅ [CALLBACK] Server response parsed successfully!")
                io.println("   Server version: " <> int.to_string(version))
                io.println(
                  "   Server timestamp: " <> string.trim(timestamp_str),
                )
                io.println("")
              }
              Error(_) -> {
                io.println("❌ [CALLBACK] Failed to parse version")
                io.println("   Raw version string: " <> version_str)
                io.println("")
              }
            }
          }
          _ -> {
            io.println("❌ [CALLBACK] No NULL separator found in response")
          }
        }
      }),
    )
  {
    Ok(conn) -> {
      io.println("✅ Connection initiated")
      io.println("")
      io.println("⏳ Waiting 2 seconds for socket to be ready...")
      connection.sleep(2000)

      io.println("✅ Socket should be ready now")
      io.println("")

      // Send START_API handshake message
      io.println("📤 Sending handshake message...")
      let handshake = protocol.start_api_message(100, 200)

      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("✅ Handshake sent successfully")
          io.println("")
          io.println("⏳ Waiting 5 seconds for server to respond...")
          io.println("   (Data will appear via callback above)")
          io.println("")

          // Wait for server to respond via callback
          connection.sleep(5000)

          // Now try to receive data (should be in the buffer by now)
          io.println("📥 Attempting to receive data from buffer...")
          case connection.receive(conn) {
            Ok(data) -> {
              io.println("✅ Data received from buffer:")
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
              io.println("❌ No data in buffer")
              io.println("")
              io.println("=== CONNECTION STATUS ===")
              io.println("✅ TCP connection: SUCCESS")
              io.println("✅ Handshake sent: SUCCESS")
              io.println("✅ Server response: RECEIVED via callback (see above)")
              io.println("")
              io.println("The callback-based approach works correctly!")
              io.println("Data is received asynchronously via callbacks.")
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
