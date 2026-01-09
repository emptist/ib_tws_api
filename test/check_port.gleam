import connection
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/io
import gleam/result
import protocol

pub fn main() {
  io.println("╔══════════════════════════════════════════════════════════════╗")
  io.println(
    "║              PORT 7697 CHECK TEST                              ║",
  )
  io.println("╚══════════════════════════════════════════════════════════════╝")
  io.println("")

  let config = connection.config("127.0.0.1", 7697, 3424)

  io.println("📋 Configuration:")
  io.println("   Host: " <> config.host)
  io.println("   Port: " <> int.to_string(config.port))
  io.println("   Client ID: " <> int.to_string(config.client_id))
  io.println("")

  case connection.connect(config) {
    Ok(conn) -> {
      io.println("✅ Connection established to port 7697!")
      io.println("")

      // Send handshake
      io.println("🤝 Sending handshake...")
      let handshake = protocol.start_api_message(100, 200)
      let _ = connection.send_bytes(conn, handshake)
      io.println(
        "✅ Handshake sent ("
        <> int.to_string(bit_array.byte_size(handshake))
        <> " bytes)",
      )
      io.println("")

      // Wait for response
      io.println("⏳ Waiting 5 seconds for server response...")
      connection.sleep(5000)

      // Try to receive data
      io.println("📊 Checking for received data...")
      io.println("")

      case connection.receive(conn) {
        Ok(data) -> {
          io.println("📥 Received data:")
          io.println(
            "   ["
            <> int.to_string(bit_array.byte_size(bit_array.from_string(data)))
            <> " bytes]: "
            <> data,
          )
        }
        Error(err) -> {
          io.println("⚠️  No data received from server")
          io.println(
            "   Error: "
            <> case err {
              connection.ConnectionFailed(msg) -> msg
              _ -> "Unknown error"
            },
          )
        }
      }

      // Close connection
      let _ = connection.close(conn)
      io.println("")
      io.println("✅ Connection closed")
    }

    Error(err) -> {
      io.println("❌ Failed to connect to port 7697")
      io.println(
        "   Error: "
        <> case err {
          connection.ConnectionFailed(msg) -> msg
          connection.InvalidHost -> "Invalid host"
          connection.InvalidPort -> "Invalid port"
          connection.SocketError(msg) -> "Socket error: " <> msg
          connection.Timeout -> "Connection timeout"
        },
      )
      io.println("")
      io.println("💡 TROUBLESHOOTING:")
      io.println("   1. Make sure TWS API is running")
      io.println("   2. Check TWS API configuration:")
      io.println("      - Enable ActiveX and Socket Clients")
      io.println("      - Set Socket port to 7697")
      io.println("      - Check 'Allow connections from localhost'")
      io.println("   3. Restart TWS if needed")
    }
  }

  io.println("")
  io.println("╔══════════════════════════════════════════════════════════════╗")
  io.println(
    "║                      TEST COMPLETE                              ║",
  )
  io.println("╚══════════════════════════════════════════════════════════════╝")
}
