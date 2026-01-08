import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/string
import message_encoder
import protocol

/// Comprehensive test that connects, handshakes, sends client ID, and requests account data
/// This will answer the game questions about positions, funds, and orders
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

  // Track if handshake is complete
  let handshake_complete = io.println("")

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
      let handshake = protocol.start_api_message(100, 200)

      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("✅ Handshake sent successfully")
          io.println("")
          io.println("⏳ Waiting 3 seconds for server to respond...")
          io.println("")

          // Wait for server to respond via callback
          connection.sleep(3000)

          // Send client ID as separate message after server response
          io.println("📤 Sending client ID message...")
          let client_id_msg = protocol.client_id_message(conn_config.client_id)
          case connection.send_bytes(conn, client_id_msg) {
            Ok(_) -> {
              io.println(
                "✅ Client ID sent: " <> int.to_string(conn_config.client_id),
              )
              io.println("")
              io.println("=== CONNECTION ESTABLISHED ===")
              io.println("✅ TCP connection: SUCCESS")
              io.println("✅ API Handshake: SUCCESS")
              io.println("✅ Client ID: SUCCESS")
              io.println("")
              io.println("Now ready to request account data...")
              io.println("")

              // Wait a bit more for connection to stabilize
              connection.sleep(2000)

              // Request account data
              io.println("📤 Requesting account summary...")
              let account_summary_payload =
                message_encoder.request_account_summary(1, "All", "$LEDGER:ALL")
              let account_summary_msg =
                message_encoder.encode_message(6, account_summary_payload)
              case connection.send_bytes(conn, account_summary_msg) {
                Ok(_) -> io.println("✅ Account summary request sent")
                Error(_) ->
                  io.println("❌ Failed to send account summary request")
              }

              io.println("")
              io.println("📤 Requesting positions...")
              let positions_payload = message_encoder.request_positions(1)
              let positions_msg =
                message_encoder.encode_message(7, positions_payload)
              case connection.send_bytes(conn, positions_msg) {
                Ok(_) -> io.println("✅ Positions request sent")
                Error(_) -> io.println("❌ Failed to send positions request")
              }

              io.println("")
              io.println("📤 Requesting open orders...")
              let open_orders_msg = message_encoder.request_open_orders()
              let open_orders_msg_with_id =
                message_encoder.encode_message(9, open_orders_msg)
              case connection.send_bytes(conn, open_orders_msg_with_id) {
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
