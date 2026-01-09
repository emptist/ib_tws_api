import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option.{Some}
import gleam/string
import protocol

pub fn main() {
  io.println("╔══════════════════════════════════════════════════════════════╗")
  io.println(
    "║           KEEP-ALIVE HANDSHAKE TEST                           ║",
  )
  io.println(
    "║           Maintain connection to receive server response          ║",
  )
  io.println("╚══════════════════════════════════════════════════════════════╝")
  io.println("")

  let client_id = 3424
  let config = connection.config("127.0.0.1", 7497, client_id)

  io.println("📋 Configuration:")
  io.println("   Client ID: " <> int.to_string(client_id))
  io.println("   Port: 7497 (Paper Trading)")
  io.println("")

  let result =
    connection.connect_with_callback(
      config,
      option.Some(fn(data) {
        io.println("📥 Server response: " <> data)
        io.println("")

        // Check if this is handshake response
        case string.contains(data, "VERSION") {
          True -> {
            io.println("✅ HANDSHAKE RESPONSE RECEIVED!")
            io.println("   Server sent: " <> data)
            io.println("")
          }
          False -> {
            io.println("   (Not a handshake response)")
            Nil
          }
        }
      }),
    )

  case result {
    Ok(conn) -> {
      io.println("✅ CONNECTED TO IB TWS!")
      io.println("")

      // Step 1: Send V100+ protocol handshake
      io.println("🤝 Step 1: Sending V100+ handshake...")
      let handshake = protocol.start_api_message(100, 200)
      let _ = connection.send_bytes(conn, handshake)
      io.println(
        "✅ Handshake sent ("
        <> int.to_string(bit_array.byte_size(handshake))
        <> " bytes)",
      )
      io.println("")

      // Step 2: Keep connection alive to receive server response
      io.println("⏳ Step 2: Keeping connection alive for 3 seconds...")
      io.println("   (Waiting for server handshake response)")
      io.println("")
      connection.keep_alive(3000)

      // Step 3: Send client ID
      io.println("🤝 Step 3: Sending Client ID...")
      let client_id_msg = protocol.client_id_message(config.client_id)
      let _ = connection.send_bytes(conn, client_id_msg)
      io.println("✅ Client ID sent (4 bytes)")
      io.println("")

      // Step 4: Keep connection alive for more responses
      io.println("⏳ Step 4: Keeping connection alive for 3 more seconds...")
      io.println("   (Waiting for server confirmation)")
      io.println("")
      connection.keep_alive(3000)

      // Close connection
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
