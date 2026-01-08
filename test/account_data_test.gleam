import account_data
import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/string
import message_encoder
import protocol

/// Comprehensive test that connects, handshakes, and requests account data
/// This will answer game questions about positions, funds, and orders
pub fn main() {
  io.println("\n=== ACCOUNT DATA TEST ===")
  io.println("Connects to IB TWS API and requests account information")
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
            // Parse version number
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
                io.println("")
              }
            }
          }
          _ -> {
            io.println("ℹ️  [CALLBACK] Non-handshake data received")
            io.println("   Data: " <> message_data)
            io.println("")
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
      let handshake = message_encoder.start_api_message(conn_config.client_id)

      // Add length prefix and convert to bytes
      let handshake_bytes =
        message_encoder.add_length_prefix_to_string(handshake)

      case connection.send_bytes(conn, handshake_bytes) {
        Ok(_) -> {
          io.println("✅ Handshake sent successfully")
          io.println("")
          io.println("⏳ Waiting 3 seconds for server to respond...")
          io.println("")

          // Wait for server to respond via callback
          connection.sleep(3000)

          // Wait for nextValidId event (connection ready signal)
          io.println(
            "⏳ Waiting for nextValidId event (connection ready signal)...",
          )
          io.println("⏳ This indicates TWS is ready to accept API requests")
          io.println("")

          // Wait for connection to be ready
          case connection.wait_for_ready(conn, 10_000) {
            True -> {
              io.println("✅ Connection is READY to accept API requests!")
              io.println("")

              // Now we can send API requests
              io.println("📤 Requesting account summary...")
              let account_summary_msg =
                account_data.request_account_summary(
                  1,
                  "All",
                  account_data.common_account_tags(),
                )

              // Add length prefix and convert to bytes
              let account_summary_bytes =
                message_encoder.add_length_prefix_to_string(account_summary_msg)
              case connection.send_bytes(conn, account_summary_bytes) {
                Ok(_) -> io.println("✅ Account summary request sent")
                Error(_) ->
                  io.println("❌ Failed to send account summary request")
              }

              io.println("")
              io.println("📤 Requesting positions...")
              let positions_msg = account_data.request_positions(1)

              // Add length prefix and convert to bytes
              let positions_bytes =
                message_encoder.add_length_prefix_to_string(positions_msg)
              case connection.send_bytes(conn, positions_bytes) {
                Ok(_) -> io.println("✅ Positions request sent")
                Error(_) -> io.println("❌ Failed to send positions request")
              }

              io.println("")
              io.println("📤 Requesting open orders...")
              let open_orders_msg = message_encoder.request_open_orders()

              // Add length prefix and convert to bytes
              let open_orders_bytes =
                message_encoder.add_length_prefix_to_string(open_orders_msg)
              case connection.send_bytes(conn, open_orders_bytes) {
                Ok(_) -> io.println("✅ Open orders request sent")
                Error(_) -> io.println("❌ Failed to send open orders request")
              }

              io.println("")
              io.println(
                "⏳ Listening for incoming data (press Ctrl+C to exit)...",
              )
              io.println("")
              io.println("Waiting for account data responses...")
              io.println("")

              // Keep connection open
              connection.sleep(1_000_000_000)
            }
            False -> {
              io.println("❌ Timeout: Connection not ready after 10 seconds")
              io.println(
                "❌ This usually means nextValidId event was not received",
              )
              io.println("❌ Check TWS settings and ensure API is enabled")
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
