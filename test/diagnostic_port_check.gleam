import connection
import gleam/int
import gleam/io
import gleam/option

/// Diagnostic test to check if TWS is actually listening on port 7497
pub fn main() {
  io.println("\n=== TWS PORT DIAGNOSTIC ===")
  io.println("")

  // Check if port 7497 is available
  io.println("Checking if TWS is listening on port 7497...")
  let port_7497 = connection.detect_ib_tws_port("127.0.0.1", 2)

  case port_7497 {
    7497 -> {
      io.println("✅ Port 7497 is AVAILABLE (TWS is listening)")
    }
    0 -> {
      io.println("❌ Port 7497 is NOT available")
      io.println("")
      io.println("Please check:")
      io.println("  1. Is TWS application running?")
      io.println(
        "  2. Is API enabled in TWS? (File → Global Configuration → API → Settings)",
      )
      io.println("  3. Is 'Enable ActiveX and Socket Clients' checked?")
      io.println("  4. Is the port set to 7497?")
      io.println("  5. Is 'Read-Only API' unchecked (for paper trading)?")
    }
    _ -> {
      io.println("⚠️  Unexpected result: " <> int.to_string(port_7497))
    }
  }

  io.println("")

  // Try to actually connect
  io.println("Attempting to connect to TWS...")
  let config = connection.config("127.0.0.1", 7497, 9999)

  // Use callback-based connection to properly wait for ReadyEvent
  // node_socket_client.connect() is asynchronous - it returns immediately
  // but the socket isn't actually ready until ReadyEvent fires
  case
    connection.connect_with_callback(
      config,
      option.Some(fn(data) { io.println("✅ Received data: " <> data) }),
    )
  {
    Ok(conn) -> {
      io.println("✅ Connection initiated (waiting for socket ready...)")
      io.println("")

      // CRITICAL: Wait for socket to actually connect
      // ReadyEvent will fire asynchronously
      io.println(
        "Waiting 5 seconds for socket to be ready and TWS to send data...",
      )
      io.println(
        "(Note: node_socket_client.connect() returns immediately but socket connects asynchronously)",
      )
      io.println("")

      connection.sleep(5000)

      // Now check if we received any data
      case connection.receive(conn) {
        Ok(data) -> {
          io.println("✅ Received data from TWS:")
          io.println("   " <> data)
        }
        Error(_) -> {
          io.println("❌ NO data received from TWS")
          io.println("")
          io.println("This suggests TWS is not responding to our connection.")
          io.println("Common causes:")
          io.println("  1. TWS API is not enabled")
          io.println("  2. Wrong client ID (conflict with another client)")
          io.println("  3. API connection permission issue")
          io.println("  4. Socket connected but TWS not sending data")
        }
      }

      io.println("")
      io.println("Closing connection...")
      let _ = connection.close(conn)
      Ok(Nil)
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
      Error(err)
    }
  }

  io.println("")
  io.println("=== DIAGNOSTIC COMPLETE ===")
  io.println("")
}
