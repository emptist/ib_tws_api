import account_data
import connection
import gleam/int
import gleam/io
import gleam/string
import message_encoder
import protocol

// ═══════════════════════════════════════════════════════════════
// RECEIVE AND DECODE TEST
//
// This test connects to TWS, sends requests, and receives raw responses
// to discover the actual format of TWS messages.
//
// Run with: gleam run --module test/receive_and_decode_test
// ═══════════════════════════════════════════════════════════════

pub fn main() {
  io.println(
    "\n═══════════════════════════════════════════════════════════════",
  )
  io.println("RECEIVE AND DECODE TEST - Discover TWS Response Format")
  io.println(
    "═══════════════════════════════════════════════════════════════\n",
  )

  let client_id = connection.generate_client_id()
  let config = connection.config("127.0.0.1", 7497, client_id)

  io.println("Client ID: " <> int.to_string(client_id))
  io.println("Connecting to paper trading (port 7497)...\n")

  let assert Ok(conn) = connection.connect(config)
  io.println("✅ Connected to TWS\n")

  // Step 1: Send handshake
  io.println("Step 1: Sending handshake...")
  let handshake = protocol.start_api_message(100, 200)
  let _ = connection.send_bytes(conn, handshake)
  io.println("✅ Handshake sent")

  // Wait for server handshake response
  io.println("⏳ Waiting for server handshake response...")
  connection.sleep(1000)

  // Try to receive server handshake
  case connection.receive(conn) {
    Ok(response) -> {
      io.println("\n📥 SERVER HANDSHAKE RESPONSE:")
      io.println("Raw: " <> response)
      io.println("Hex: " <> protocol.string_to_hex(response))
      io.println("")

      // Try to parse
      case protocol.parse_server_response(response) {
        Ok(#(version, timestamp)) -> {
          io.println("✅ Parsed handshake:")
          io.println("   Version: " <> int.to_string(version))
          io.println("   Timestamp: " <> timestamp)
          io.println("")
        }
        Error(msg) -> {
          io.println("❌ Failed to parse handshake: " <> msg)
          io.println("")
        }
      }
    }
    Error(_) -> {
      io.println("❌ Failed to receive handshake")
      io.println("")
    }
  }

  // Step 2: Send client ID
  io.println("Step 2: Sending client ID...")
  let client_id_msg = protocol.client_id_message(client_id)
  let _ = connection.send_bytes(conn, client_id_msg)
  io.println("✅ Client ID sent\n")

  // Wait for any response
  io.println("⏳ Waiting 2 seconds...")
  connection.sleep(2000)

  // Step 3: Request account summary
  io.println("Step 3: Requesting account summary...")
  let account_summary_msg =
    account_data.request_account_summary(
      1,
      "All",
      account_data.common_account_tags(),
    )
  let account_summary_bytes =
    message_encoder.add_length_prefix_to_string(account_summary_msg)
  let _ = connection.send_bytes(conn, account_summary_bytes)
  io.println("✅ Account summary request sent")

  // Step 4: Request positions
  io.println("Step 4: Requesting positions...")
  let positions_msg = account_data.request_positions(1)
  let positions_bytes =
    message_encoder.add_length_prefix_to_string(positions_msg)
  let _ = connection.send_bytes(conn, positions_bytes)
  io.println("✅ Positions request sent\n")

  // Step 5: Receive and display all responses
  io.println("Step 5: Receiving responses (10 seconds)...")
  io.println(
    "═══════════════════════════════════════════════════════════════\n",
  )

  let max_responses = 20
  let response_count = receive_and_display_responses(conn, max_responses, 0)

  io.println(
    "\n═══════════════════════════════════════════════════════════════",
  )
  io.println("✅ Received " <> int.to_string(response_count) <> " responses")
  io.println(
    "═══════════════════════════════════════════════════════════════\n",
  )

  // Close connection
  let _ = connection.close(conn)
}

fn receive_and_display_responses(
  conn: connection.Connection,
  max_remaining: Int,
  count: Int,
) -> Int {
  case max_remaining <= 0 {
    True -> count
    False -> {
      // Try to receive with timeout
      case connection.receive(conn) {
        Ok(response) -> {
          let response_size = string.length(response)
          io.println("\n📥 RESPONSE #" <> int.to_string(count + 1))
          io.println("   Size: " <> int.to_string(response_size) <> " bytes")
          io.println("   Raw: " <> response)
          io.println("   Hex: " <> protocol.string_to_hex(response))

          // Try to detect message type
          let message_type = detect_message_type(response)
          io.println("   Type: " <> message_type)

          io.println("")
          receive_and_display_responses(conn, max_remaining - 1, count + 1)
        }
        Error(_) -> {
          // Timeout or error, stop receiving
          count
        }
      }
    }
  }
}

fn detect_message_type(data: String) -> String {
  // Try to detect message type based on content
  let trimmed = string.trim(data)

  case trimmed {
    "" -> "EMPTY"
    _ -> {
      // Check for known patterns
      let contains_ledger =
        string.contains(trimmed, "AccountSummary")
        || string.contains(trimmed, "LEDGER")
      let contains_position =
        string.contains(trimmed, "Position")
        || string.contains(trimmed, "POSITION")
      let contains_order =
        string.contains(trimmed, "Order") || string.contains(trimmed, "ORDER")
      let contains_error =
        string.contains(trimmed, "Error") || string.contains(trimmed, "ERROR")

      case contains_ledger {
        True -> "ACCOUNT_SUMMARY"
        False ->
          case contains_position {
            True -> "POSITION"
            False ->
              case contains_order {
                True -> "ORDER"
                False ->
                  case contains_error {
                    True -> "ERROR"
                    False -> "UNKNOWN"
                  }
              }
          }
      }
    }
  }
}
