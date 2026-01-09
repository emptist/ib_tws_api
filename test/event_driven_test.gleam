import account_data
import connection
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import message_encoder
import protocol

// ═══════════════════════════════════════════════════════════════
// EVENT-DRIVEN TEST - Follow Correct TWS Protocol
//
// This test uses proper event-driven approach to communicate with TWS.
// Protocol sequence:
// 1. Connect to TWS
// 2. Send handshake (API\0 + length + version string)
// 3. Wait for server handshake response
// 4. Send START_API message with client ID (71\02\0<client_id>\0\0)
// 5. Wait for nextValidId event
// 6. Send API requests
// 7. Receive and parse responses
//
// Run with: gleam run --module event_driven_test
// ═══════════════════════════════════════════════════════════════

pub fn main() {
  io.println(
    "\n═══════════════════════════════════════════════════════════════",
  )
  io.println("EVENT-DRIVEN TEST - Follow Correct TWS Protocol")
  io.println(
    "═══════════════════════════════════════════════════════════════\n",
  )

  let client_id = connection.generate_client_id()
  io.println("Client ID: " <> int.to_string(client_id))
  io.println("Connecting to paper trading (port 7497)...\n")

  // Create a mutable list to store received data
  let received_data = []

  // Create callback to collect data
  let callback = fn(data: String) {
    received_data
    // In real implementation, we'd append to a mutable list
    io.println("[Callback] Received: " <> data)
  }

  // Connect with callback
  let config = connection.config("127.0.0.1", 7497, client_id)
  // let assert Ok(conn) = connection.connect_with_callback(config, Some(callback))
  let assert Ok(conn) = connection.connect(config)

  io.println("✅ Connected to TWS\n")

  // Step 1: Send handshake
  io.println("Step 1: Sending handshake...")
  let handshake = protocol.start_api_message(100, 200)
  let _ = connection.send_bytes(conn, handshake)
  io.println("✅ Handshake sent")

  // Wait for server handshake response
  io.println("⏳ Waiting 2 seconds for server handshake...")
  connection.sleep(2000)

  // Try to receive server handshake
  case connection.receive(conn) {
    Ok(response) -> {
      io.println("\n📥 SERVER HANDSHAKE RESPONSE:")
      io.println("Raw: " <> response)
      io.println("Hex: " <> protocol.string_to_hex(response))

      // Try to parse
      case protocol.parse_server_response(response) {
        Ok(#(version, timestamp)) -> {
          io.println("\n✅ Parsed handshake:")
          io.println("   Version: " <> int.to_string(version))
          io.println("   Timestamp: " <> timestamp)
          io.println("")
        }
        Error(msg) -> {
          io.println("\n❌ Failed to parse handshake: " <> msg)
          io.println("")
        }
      }
    }
    Error(_) -> {
      io.println("\n❌ Failed to receive server handshake")
      io.println("")
    }
  }

  // Step 2: Send START_API message with client ID (CORRECT FORMAT)
  io.println("Step 2: Sending START_API message with client ID...")
  let start_api_msg = message_encoder.start_api_message_with_length(client_id)
  let _ = connection.send_bytes(conn, start_api_msg)
  io.println("✅ START_API message sent")

  // Wait for TWS to process
  io.println("⏳ Waiting 3 seconds for TWS to process...")
  connection.sleep(3000)

  // Check for nextValidId event
  case connection.receive(conn) {
    Ok(response) -> {
      io.println("\n📥 Received response:")
      io.println("Raw: " <> response)
      io.println("Hex: " <> protocol.string_to_hex(response))
      io.println("")
    }
    Error(_) -> {
      io.println("\n❌ No response received yet")
      io.println("")
    }
  }

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
  let positions_msg = message_encoder.request_positions(1)
  let positions_bytes =
    message_encoder.add_length_prefix_to_string(positions_msg)
  let _ = connection.send_bytes(conn, positions_bytes)
  io.println("✅ Positions request sent")

  // Step 5: Request open orders
  io.println("Step 5: Requesting open orders...")
  let open_orders_msg = message_encoder.request_open_orders()
  let open_orders_bytes =
    message_encoder.add_length_prefix_to_string(open_orders_msg)
  let _ = connection.send_bytes(conn, open_orders_bytes)
  io.println("✅ Open orders request sent\n")

  // Step 6: Receive and display all responses
  io.println("Step 6: Receiving responses (10 seconds)...")
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
      // Try to receive
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

          // Try to parse as fields
          let fields = parse_message_fields(response)
          io.println("   Fields: " <> int.to_string(list.length(fields)))
          list.each(fields, fn(field) { io.println("     - " <> field) })

          io.println("")
          receive_and_display_responses(conn, max_remaining - 1, count + 1)
        }
        Error(_) -> {
          // No more data, stop receiving
          count
        }
      }
    }
  }
}

fn detect_message_type(data: String) -> String {
  let trimmed = string.trim(data)

  case trimmed {
    "" -> "EMPTY"
    _ -> {
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

fn parse_message_fields(data: String) -> List(String) {
  // Split by NULL byte and filter empty strings
  data
  |> string.split("\u{0000}")
  |> list.filter(fn(s) { s != "" })
  |> list.map(fn(s) { string.trim(s) })
}
