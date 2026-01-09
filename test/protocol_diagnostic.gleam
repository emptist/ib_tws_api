import connection
import gleam/int
import gleam/io
import message_encoder
import protocol

// ═══════════════════════════════════════════════════════════════
// PROTOCOL DIAGNOSTIC TEST
//
// Minimal test to debug why TWS is closing connections.
// This will help us understand the exact sequence TWS expects.
//
// Run with: gleam run --module protocol_diagnostic
// ═══════════════════════════════════════════════════════════════

pub fn main() {
  io.println(
    "\n═══════════════════════════════════════════════════════════════",
  )
  io.println("PROTOCOL DIAGNOSTIC TEST")
  io.println(
    "═══════════════════════════════════════════════════════════════\n",
  )

  let client_id = connection.generate_client_id()
  io.println("Client ID: " <> int.to_string(client_id))
  io.println("Connecting to paper trading (port 7497)...\n")

  // Connect
  let config = connection.config("127.0.0.1", 7497, client_id)
  let assert Ok(conn) = connection.connect(config)
  io.println("✅ Connected to TWS\n")

  // Wait a bit to see if connection stays open
  io.println("Waiting 1 second to check if connection stays open...")
  connection.sleep(1000)

  // Step 1: Send handshake
  io.println("Step 1: Sending handshake...")
  let handshake = protocol.start_api_message(100, 200)
  let _ = connection.send_bytes(conn, handshake)
  io.println("✅ Handshake sent")

  // Wait for server response
  io.println("Waiting 2 seconds for server handshake response...")
  connection.sleep(2000)

  // Try to receive data
  case connection.receive(conn) {
    Ok(first) -> {
      io.println("\n📥 Received data after handshake:")
      io.println("Raw: " <> first)
      io.println("Hex: " <> protocol.string_to_hex(first))
    }
    Error(_) -> {
      io.println("\n❌ No data received yet")
    }
  }
  io.println("")

  // Step 2: Send START_API message
  io.println("Step 2: Sending START_API message...")
  let start_api_msg = message_encoder.start_api_message_with_length(client_id)
  let _ = connection.send_bytes(conn, start_api_msg)
  io.println("✅ START_API message sent")

  // Wait for TWS to process
  io.println("Waiting 3 seconds for TWS to process...")
  connection.sleep(3000)

  // Try to receive more data
  case connection.receive(conn) {
    Ok(data) -> {
      io.println("\n📥 Received data after START_API:")
      io.println("Raw: " <> data)
      io.println("Hex: " <> protocol.string_to_hex(data))
    }
    Error(_) -> {
      io.println("\n❌ No data received")
    }
  }

  io.println("\n")
  io.println("═══════════════════════════════════════════════════════════════")
  io.println("Diagnostic complete")
  io.println(
    "═══════════════════════════════════════════════════════════════\n",
  )

  // Close connection
  let _ = connection.close(conn)
}
