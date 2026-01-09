import binary_message_decoder
import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option.{Some}
import gleam/string
import message_encoder
import protocol

/// Complete handshake test with client ID
/// Follows natural IB TWS protocol flow:
/// 1. Connect
/// 2. Send handshake
/// 3. Receive server response
/// 4. Send client ID
/// 5. Receive account data, positions, etc.
pub fn main() {
  io.println("\n=== COMPLETE HANDSHAKE TEST ===")
  io.println("This test follows the full IB TWS protocol flow")
  io.println("1. Send handshake")
  io.println("2. Receive server response")
  io.println("3. Send client ID")
  io.println("4. Receive account data, positions, etc.")
  io.println("")

  // Generate unique client ID
  let client_id = connection.generate_client_id()
  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  // Create connection config for live trading (port 7496) - paper trading not available
  let conn_config =
    connection.config_with_account_type(
      "127.0.0.1",
      7496,
      connection.LiveTradingReadOnly,
      client_id,
    )

  io.println("⚠️  CONNECTING TO LIVE TRADING ACCOUNT (READ-ONLY)")
  io.println("   Port: 7496 (Live Trading)")
  io.println("   Mode: READ-ONLY (No trading allowed)")
  io.println("")

  // Connect with callback to handle data
  case
    connection.connect_with_callback(
      conn_config,
      Some(fn(data) {
        // Log raw data
        io.println("📥 [DATA RECEIVED]")
        io.println("   Length: " <> int.to_string(string.length(data)))
        io.println("   Raw: " <> data)
        io.println("")

        // Convert to bit array
        let bits = bit_array.from_string(data)

        // Try to parse as handshake response first
        case binary_message_decoder.parse_handshake_response(bits) {
          Ok(#(version, timestamp)) -> {
            io.println("✅ [HANDSHAKE RESPONSE]")
            io.println("   Version: " <> int.to_string(version))
            io.println("   Timestamp: " <> timestamp)
            io.println("")
          }
          Error(handshake_err) -> {
            // Not a handshake response - try binary message
            case binary_message_decoder.parse_message(bits) {
              Ok(msg) -> {
                io.println("📨 [BINARY MESSAGE]")
                binary_message_decoder.log_message(msg)
              }
              Error(parse_err) -> {
                io.println(
                  "ℹ️  [INFO] Cannot parse as handshake or binary message",
                )
                io.println("   Error: " <> parse_err)
                io.println("   Raw hex: " <> string.inspect(data))
              }
            }
          }
        }
      }),
    )
  {
    Ok(conn) -> {
      io.println("✅ Connection initiated")
      io.println("")

      // Wait for socket to be ready
      io.println("⏳ Waiting 2 seconds for socket to be ready...")
      connection.sleep(2000)

      io.println("✅ Socket should be ready now")
      io.println("")

      // Send handshake
      io.println("📤 [SENDING HANDSHAKE]")
      io.println("   Message: START_API (v100..200)")
      let handshake = message_encoder.start_api_message(client_id)
      let handshake_bytes =
        message_encoder.add_length_prefix_to_string(handshake)
      io.println(
        "   Handshake size: "
        <> int.to_string(bit_array.byte_size(handshake_bytes))
        <> " bytes",
      )
      io.println("")

      case connection.send_bytes(conn, handshake_bytes) {
        Ok(_) -> {
          io.println("✅ Handshake sent successfully")
          io.println("")

          // Send client ID IMMEDIATELY after handshake (no wait for response)
          io.println("📤 [SENDING CLIENT ID]")
          io.println("   Client ID: " <> int.to_string(client_id))

          let client_id_msg = protocol.client_id_message(client_id)
          io.println(
            "   Client ID message size: "
            <> int.to_string(bit_array.byte_size(client_id_msg))
            <> " bytes",
          )
          io.println("")

          case connection.send_bytes(conn, client_id_msg) {
            Ok(_) -> {
              io.println("✅ Client ID sent successfully")
              io.println("")

              // Send request for account summary to trigger TWS to send data
              io.println("📤 [SENDING ACCOUNT SUMMARY REQUEST]")
              io.println("   Request ID: 1")
              io.println("   Group: All")
              io.println("   Tags: $LEDGER:ALL")

              let account_request =
                message_encoder.request_account_summary(1, "All", "$LEDGER:ALL")
              let account_request_bytes =
                message_encoder.add_length_prefix_to_string(account_request)
              io.println(
                "   Request message size: "
                <> int.to_string(bit_array.byte_size(account_request_bytes))
                <> " bytes",
              )
              io.println("")

              case connection.send_bytes(conn, account_request_bytes) {
                Ok(_) -> {
                  io.println("✅ Account summary request sent successfully")
                  io.println("")
                  io.println(
                    "⏳ Waiting 15 seconds for account data, positions, etc...",
                  )
                  io.println("(Keeping Node.js process alive for async events)")
                  io.println("")
                  connection.keep_alive(15_000)

                  io.println("")
                  io.println("⏱️  Test complete")
                  io.println("")
                  io.println("ℹ️  SUMMARY:")
                  io.println("   - Handshake was sent")
                  io.println("   - Client ID was sent")
                  io.println("   - Account summary request was sent")
                  io.println("   - Check above for any data received from TWS")
                }
                Error(err) -> {
                  let error_msg = case err {
                    connection.SocketError(msg) -> msg
                    connection.ConnectionFailed(msg) -> msg
                    connection.InvalidHost -> "Invalid host"
                    connection.InvalidPort -> "Invalid port"
                    connection.Timeout -> "Connection timeout"
                  }
                  io.println("❌ Failed to send account request: " <> error_msg)
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
      io.println("")
      io.println("Please check:")
      io.println("  1. Is TWS application running?")
      io.println(
        "  2. Is API enabled in TWS? (File → Global Configuration → API → Settings)",
      )
      io.println("  3. Is 'Enable ActiveX and Socket Clients' checked?")
      io.println("  4. Is the port set to 7496?")
      io.println("  5. Is 'Read-Only API' unchecked?")
    }
  }

  io.println("")
  io.println("=== COMPLETE HANDSHAKE TEST COMPLETE ===")
  io.println("")
}
