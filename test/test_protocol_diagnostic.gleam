import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/string

/// Protocol Diagnostic Test
/// 
/// This test helps diagnose the TWS connection protocol by:
/// 1. Testing different handshake formats
/// 2. Verifying message encoding
/// 3. Comparing with known working implementations
pub fn main() {
  io.println("═══════════════════════════════════════════════════════════════")
  io.println("PROTOCOL DIAGNOSTIC TEST")
  io.println("═══════════════════════════════════════════════════════════════")

  // Test 1: Verify handshake message bytes
  io.println("\n📋 Test 1: Verify handshake message bytes")
  test_handshake_bytes()

  // Test 2: Test connection with minimal handshake
  io.println("\n📋 Test 2: Test connection with minimal handshake")
  test_minimal_handshake()

  io.println(
    "\n═══════════════════════════════════════════════════════════════",
  )
  io.println("Diagnostic tests complete")
  io.println("═══════════════════════════════════════════════════════════════")
}

fn test_handshake_bytes() {
  // Create handshake message
  let version_string = "v100.200"
  let version_length = string.length(version_string)
  let api_version = 200

  io.println("Version string: " <> version_string)
  io.println("Version length: " <> int.to_string(version_length))
  io.println("API version: " <> int.to_string(api_version))

  // Build message manually using bit array syntax
  let message = <<version_length:32, version_string:utf8, api_version:32>>

  io.println(
    "Message size: " <> int.to_string(bit_array.byte_size(message)) <> " bytes",
  )

  // Show hex representation
  let hex = bit_array.base16_encode(message)
  io.println("Message hex: " <> hex)
}

fn test_minimal_handshake() {
  let config = connection.config("127.0.0.1", 7497, 9999)

  io.println("Connecting to paper trading (port 7497)...")

  case connection.connect(config) {
    Ok(conn) -> {
      io.println("✅ Connected")

      // Send minimal handshake (just version string)
      let version_string = "v100"
      let version_length = string.length(version_string)
      let api_version = 100

      let handshake = <<version_length:32, version_string:utf8, api_version:32>>

      io.println("Sending minimal handshake...")
      io.println(
        "Message: "
        <> version_string
        <> " (version "
        <> int.to_string(api_version)
        <> ")",
      )

      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("✅ Handshake sent")

          // Wait for response
          io.println("⏳ Waiting 2 seconds for response...")
          connection.sleep(2000)

          // Check received data
          case connection.receive(conn) {
            Ok(data) -> {
              io.println("✅ Received response:")
              io.println("  " <> data)
            }
            Error(_) -> io.println("❌ No responses from TWS")
          }

          connection.close(conn)
        }
        Error(_) -> {
          io.println("❌ Failed to send handshake")
          connection.close(conn)
        }
      }
    }
    Error(_) -> Ok(Nil)
  }
}
