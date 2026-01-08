import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option.{Some}
import gleam/string
import protocol

/// Simple handshake test - ONLY sends handshake and client ID
/// No requests - just wait to see what TWS sends naturally
pub fn main() {
  io.println("\n=== SIMPLE HANDSHAKE ONLY TEST ===")
  io.println("This test only sends handshake and client ID")
  io.println("No requests - just wait to see what TWS sends naturally")
  io.println("")

  // Generate unique client ID
  let client_id = connection.generate_client_id()
  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  // Create connection config for live trading (port 7496)
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

          // Wait a bit before sending client ID
          io.println("⏳ Waiting 1 second...")
          connection.sleep(1000)

          // Send client ID
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
              io.println("⏳ Waiting 30 seconds for TWS to send data...")
              io.println("(Keeping Node.js process alive for async events)")
              io.println("")
              connection.keep_alive(30_000)

              io.println("")
              io.println("⏱️  Test complete")
              io.println("")
              io.println("ℹ️  SUMMARY:")
              io.println("   - Handshake was sent")
              io.println("   - Client ID was sent")
              io.println("   - No requests were sent")
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
  io.println("=== SIMPLE HANDSHAKE ONLY TEST COMPLETE ===")
  io.println("")
}
