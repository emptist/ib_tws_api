import connection
import gleam/int
import gleam/io
import gleam/list
import gleam/string

pub fn main() {
  io.println("╔══════════════════════════════════════════════════════════════╗")
  io.println(
    "║           IB TWS PORT DETECTION TEST                          ║",
  )
  io.println("╚══════════════════════════════════════════════════════════════╝")
  io.println("")

  let host = "127.0.0.1"
  io.println("🔍 Checking IB TWS API ports on " <> host)
  io.println("")

  let ports_to_check = [7496, 7497, 7697, 4001, 4002]

  io.println("Testing ports:")
  list.each(ports_to_check, fn(port) {
    io.println("   - Port " <> int.to_string(port))
  })
  io.println("")

  io.println("📊 Port Status:")
  io.println("")

  list.each(ports_to_check, fn(port) {
    let config = connection.config(host, port, 9999)

    case connection.connect(config) {
      Ok(conn) -> {
        let _ = connection.close(conn)
        io.println(
          "   ✅ Port "
          <> int.to_string(port)
          <> ": OPEN and accepting connections",
        )
      }
      Error(err) -> {
        let error_msg = case err {
          connection.ConnectionFailed(msg) -> msg
          connection.InvalidHost -> "Invalid host"
          connection.InvalidPort -> "Invalid port"
          connection.SocketError(msg) -> "Socket error: " <> msg
          connection.Timeout -> "Connection timeout"
        }

        // Check if it's ECONNREFUSED
        case string.contains(error_msg, "ECONNREFUSED") {
          True ->
            io.println(
              "   ❌ Port " <> int.to_string(port) <> ": CLOSED (ECONNREFUSED)",
            )
          False ->
            io.println(
              "   ⚠️  Port " <> int.to_string(port) <> ": ERROR - " <> error_msg,
            )
        }
      }
    }
  })

  io.println("")
  io.println("╔══════════════════════════════════════════════════════════════╗")
  io.println(
    "║                      TEST COMPLETE                              ║",
  )
  io.println("╚══════════════════════════════════════════════════════════════╝")
  io.println("")
  io.println("💡 TROUBLESHOOTING:")
  io.println("   1. Open TWS (Trader Workstation)")
  io.println("   2. Go to File → Global Configuration")
  io.println("   3. Navigate to API → Settings")
  io.println("   4. Check 'Enable ActiveX and Socket Clients'")
  io.println(
    "   5. Set the Socket port number (e.g., 7497 for paper, 7496 for live)",
  )
  io.println("   6. Check 'Allow connections from localhost'")
  io.println("   7. Click OK and restart TWS")
}
