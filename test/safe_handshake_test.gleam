import binary_message_decoder
import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option.{Some}
import gleam/string
import protocol

/// SAFE handshake test - ONLY sends handshake, nothing else
/// This prevents sending invalid/ill requests to TWS
/// TWS API will stop responding if it receives invalid requests
pub fn main() {
  io.println("\n=== SAFE HANDSHAKE TEST ===")
  io.println("This test ONLY sends the handshake message")
  io.println("No other messages will be sent to prevent invalid requests")
  io.println("")

  // Generate unique client ID
  let client_id = connection.generate_client_id()
  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  // Create connection config for live trading (port 7496) in READ-ONLY mode
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
  io.println("   Test: SAFE - Only handshake will be sent")
  io.println("")

  // Track what we receive
  let data_received = io.println("")

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

        // Try to parse as handshake response
        case binary_message_decoder.parse_handshake_response(bits) {
          Ok(#(version, timestamp)) -> {
            io.println("✅ [SUCCESS] Server handshake response received!")
            io.println("   Version: " <> int.to_string(version))
            io.println("   Timestamp: " <> timestamp)
            io.println("")
            io.println("🎉 HANDSHAKE COMPLETE - Connection is working!")
            io.println("")
            io.println("ℹ️  NOTE: Client ID was NOT sent (safe mode)")
            io.println(
              "   In production, client ID should be sent AFTER receiving",
            )
            io.println("   the server's handshake response")
          }
          Error(handshake_err) -> {
            io.println("ℹ️  [INFO] Not a handshake response: " <> handshake_err)
            io.println("   This might be a binary message or error")
            io.println("")
            // Try to parse as binary message
            case binary_message_decoder.parse_message(bits) {
              Ok(msg) -> {
                binary_message_decoder.log_message(msg)
              }
              Error(parse_err) -> {
                io.println("❌ [PARSE ERROR] " <> parse_err)
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

      // ONLY send the handshake - nothing else
      io.println("📤 [SENDING HANDSHAKE ONLY]")
      io.println("   Message: START_API (v100..200)")
      let handshake = protocol.start_api_message(100, 200)
      io.println(
        "   Handshake size: "
        <> int.to_string(bit_array.byte_size(handshake))
        <> " bytes",
      )
      io.println("")

      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("✅ Handshake sent successfully")
          io.println("")
          io.println("⏳ Waiting 15 seconds for server response...")
          io.println("(Keeping Node.js process alive for async events)")
          io.println("")
          connection.keep_alive(15_000)

          io.println("")
          io.println("⏱️  Test complete")
          io.println("")
          io.println("ℹ️  SUMMARY:")
          io.println("   - Handshake was sent")
          io.println("   - Client ID was NOT sent (safe mode)")
          io.println("   - No other messages were sent")
          io.println("   - Check above for server response")
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
  io.println("=== SAFE HANDSHAKE TEST COMPLETE ===")
  io.println("")
}
