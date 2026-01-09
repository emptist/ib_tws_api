import connection
import gleam/int
import gleam/io
import gleam/option.{Some}
import protocol

/// Test ONLY the handshake - wait for server response before sending client ID
pub fn main() {
  io.println("╔══════════════════════════════════════════════════════════════╗")
  io.println(
    "║              HANDSHAKE-ONLY TEST                                  ║",
  )
  io.println("╚══════════════════════════════════════════════════════════════╝")
  io.println("")

  let client_id = connection.generate_client_id()
  let config =
    connection.config_with_account_type(
      "127.0.0.1",
      7697,
      connection.PaperTrading,
      client_id,
    )

  io.println("📋 Configuration:")
  io.println("   Client ID: " <> int.to_string(client_id))
  io.println("   Port: 7697 (Paper Trading)")
  io.println("")

  let result =
    connection.connect_with_callback(
      config,
      Some(fn(data) {
        io.println("📥 Received data: " <> data)

        // Check if this is handshake response
        case protocol.parse_server_response(data) {
          Ok(#(version, timestamp)) -> {
            io.println("")
            io.println("✅ ═══════════════════════════════════════════════════")
            io.println("✅ HANDSHAKE RESPONSE RECEIVED:")
            io.println("✅ ═══════════════════════════════════════════════════")
            io.println("   Version: " <> int.to_string(version))
            io.println("   Timestamp: " <> timestamp)
            io.println("")

            // Now send client ID
            io.println("🤝 Sending Client ID...")
            // Note: We can't send from within callback because we don't have access to conn here
            // In a real implementation, we'd need to pass conn through or use a different pattern
          }
          Error(msg) -> {
            io.println("⚠️  Not handshake response yet: " <> msg)
          }
        }
      }),
    )

  case result {
    Ok(conn) -> {
      io.println("✅ CONNECTED TO IB TWS!")
      io.println("")

      // Send handshake
      io.println("🤝 Step 1: Sending API Handshake...")
      let handshake = protocol.start_api_message(100, 200)
      let _ = connection.send_bytes(conn, handshake)
      io.println("✅ Handshake sent")
      io.println("")

      io.println("⏳ Waiting for server handshake response...")
      io.println("   (Check output above for handshake response)")
      io.println("")

      // Wait for handshake response to arrive via callback
      connection.sleep(5000)

      // Send client ID (after handshake response should have arrived)
      io.println("🤝 Step 2: Sending Client ID...")
      let client_id_msg = protocol.client_id_message(client_id)
      let _ = connection.send_bytes(conn, client_id_msg)
      io.println("✅ Client ID sent")
      io.println("")

      // Wait a bit more to see if connection stays open
      io.println("⏳ Waiting 3 more seconds to check connection stability...")
      connection.sleep(3000)

      // Close connection
      io.println("")
      io.println("🔌 Closing connection...")
      let _ = connection.close(conn)
      io.println("✅ Connection closed")
    }
    Error(err) -> {
      io.println("❌ Connection failed")
      io.println("   Please ensure TWS is running on port 7497")
    }
  }

  io.println("")
  io.println("╔══════════════════════════════════════════════════════════════╗")
  io.println(
    "║                      TEST COMPLETE                              ║",
  )
  io.println("╚══════════════════════════════════════════════════════════════╝")
}
