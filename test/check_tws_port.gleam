import gleam/int
import gleam/io

// ═══════════════════════════════════════════════════════════════
// CHECK TWS PORT
//
// Simple test to verify if TWS is listening on ports 7496 and 7497.
// This uses Node.js net module to check if ports are open.
//
// Run with: gleam run --module check_tws_port
// ═══════════════════════════════════════════════════════════════

@external(javascript, "./connection_ffi.mjs", "check_port")
pub fn check_port(host: String, port: Int, timeout: Int) -> Bool

pub fn main() {
  io.println(
    "\n═══════════════════════════════════════════════════════════════",
  )
  io.println("CHECK TWS PORT AVAILABILITY")
  io.println(
    "═══════════════════════════════════════════════════════════════\n",
  )

  let host = "127.0.0.1"
  let timeout = 2000
  // 2 seconds

  // Check port 7496 (live trading)
  io.println("Checking port 7496 (live trading)...")
  let port_7496_open = check_port(host, 7496, timeout)
  case port_7496_open {
    True -> io.println("✅ Port 7496 is OPEN - TWS is listening")
    False -> io.println("❌ Port 7496 is CLOSED - TWS is not listening")
  }
  io.println("")

  // Check port 7497 (paper trading)
  io.println("Checking port 7497 (paper trading)...")
  let port_7497_open = check_port(host, 7497, timeout)
  case port_7497_open {
    True -> io.println("✅ Port 7497 is OPEN - TWS is listening")
    False -> io.println("❌ Port 7497 is CLOSED - TWS is not listening")
  }
  io.println("")

  io.println("")
  io.println("TROUBLESHOOTING:")
  io.println("─────────────────")
  io.println("1. If both ports are CLOSED:")
  io.println("   - Make sure TWS (Trader Workstation) is running")
  io.println("   - Open TWS → Configure → API → Settings")
  io.println("   - Check 'Enable ActiveX and Socket Clients'")
  io.println("   - Check socket port: 7496 (live) or 7497 (paper)")
  io.println(
    "   - Make sure 'Read-Only API' is unchecked if you want full access",
  )
  io.println("")
  io.println(
    "2. If connection is established but closed immediately (ECONNRESET):",
  )
  io.println("   - Check TWS API permissions")
  io.println("   - Verify 'Trusted IPs' includes 127.0.0.1")
  io.println("   - Try restarting TWS")
  io.println("")

  io.println("═══════════════════════════════════════════════════════════════")
  io.println("Check complete")
  io.println(
    "═══════════════════════════════════════════════════════════════\n",
  )
}
