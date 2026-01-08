import connection
import gleam/int
import gleam/io

/// Simplest possible test - just try to connect and see what happens
pub fn main() {
  io.println("\n=== SIMPLE CONNECTION TEST ===")
  io.println("Testing if we can connect to TWS at all")
  io.println("")

  // Check port availability first
  io.println("Step 1: Checking if port 7497 is available...")
  let port = connection.detect_ib_tws_port("127.0.0.1", 2)

  let port_available = case port {
    7497 -> {
      io.println("✅ Port 7497 is available (TWS is listening)")
      True
    }
    0 -> {
      io.println("❌ Port 7497 is NOT available")
      io.println("")
      io.println("Please check:")
      io.println("  1. Is TWS application running?")
      io.println("  2. Is API enabled in TWS?")
      io.println("  3. Is port set to 7497?")
      io.println("")
      io.println("=== TEST ABORTED ===")
      False
    }
    _ -> {
      io.println("⚠️  Unexpected port: " <> int.to_string(port))
      False
    }
  }

  // Exit if port not available
  case port_available {
    False -> {
      io.println("")
      io.println("=== TEST COMPLETE ===")
      io.println("")
    }
    True -> {
      io.println("")

      // Try to connect
      io.println("Step 2: Attempting to connect...")
      let client_id = connection.generate_client_id()
      io.println("Client ID: " <> int.to_string(client_id))

      let config = connection.config("127.0.0.1", 7497, client_id)

      case connection.connect(config) {
        Ok(conn) -> {
          io.println("✅ Connection object created successfully")
          io.println("")
          io.println(
            "Note: This test only verifies connection object creation.",
          )
          io.println(
            "Actual data reception requires proper async event handling.",
          )
          io.println("")
          io.println("=== TEST COMPLETE ===")
        }
        Error(err) -> {
          let error_msg = case err {
            connection.ConnectionFailed(msg) -> msg
            connection.InvalidHost -> "Invalid host"
            connection.InvalidPort -> "Invalid port"
            connection.SocketError(msg) -> msg
            connection.Timeout -> "Connection timeout"
          }
          io.println("❌ Connection failed: " <> error_msg)
          io.println("")
          io.println("=== TEST FAILED ===")
        }
      }

      io.println("")
    }
  }
}
