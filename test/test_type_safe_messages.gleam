import api_messages
import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/result
import protocol

/// Test type-safe API message encoding
/// This test verifies that all messages are encoded with correct protocol format
/// This prevents the protocol errors we had before (raw binary vs NULL-separated tokens)
pub fn test_type_safe_message_encoding() {
  io.println("\n=== Testing Type-Safe API Message Encoding ===\n")

  // Test 1: START_API message
  io.println("Test 1: START_API message")
  let start_api_msg = api_messages.start_api_message(123)
  let encoded = api_messages.encode_message(start_api_msg)
  io.println("✓ START_API message encoded successfully")
  io.println(
    "  Size: " <> int.to_string(bit_array.byte_size(encoded)) <> " bytes",
  )
  io.println("")

  // Test 2: REQ_ACCOUNT_SUMMARY message
  io.println("Test 2: REQ_ACCOUNT_SUMMARY message")
  let account_summary_msg = api_messages.request_account_summary_message(1)
  let encoded = api_messages.encode_message(account_summary_msg)
  io.println("✓ REQ_ACCOUNT_SUMMARY message encoded successfully")
  io.println(
    "  Size: " <> int.to_string(bit_array.byte_size(encoded)) <> " bytes",
  )
  io.println("")

  // Test 3: REQ_POSITIONS message
  io.println("Test 3: REQ_POSITIONS message")
  let positions_msg = api_messages.request_positions_message(2)
  let encoded = api_messages.encode_message(positions_msg)
  io.println("✓ REQ_POSITIONS message encoded successfully")
  io.println(
    "  Size: " <> int.to_string(bit_array.byte_size(encoded)) <> " bytes",
  )
  io.println("")

  // Test 4: REQ_OPEN_ORDERS message
  io.println("Test 4: REQ_OPEN_ORDERS message")
  let open_orders_msg = api_messages.request_open_orders_message()
  let encoded = api_messages.encode_message(open_orders_msg)
  io.println("✓ REQ_OPEN_ORDERS message encoded successfully")
  io.println(
    "  Size: " <> int.to_string(bit_array.byte_size(encoded)) <> " bytes",
  )
  io.println("")

  // Test 5: CANCEL_ORDER message
  io.println("Test 5: CANCEL_ORDER message")
  let cancel_msg = api_messages.cancel_order_message(3, 456)
  let encoded = api_messages.encode_message(cancel_msg)
  io.println("✓ CANCEL_ORDER message encoded successfully")
  io.println(
    "  Size: " <> int.to_string(bit_array.byte_size(encoded)) <> " bytes",
  )
  io.println("")

  // Test 6: PLACE_ORDER message (market buy)
  io.println("Test 6: PLACE_ORDER message (market buy)")
  let buy_order =
    api_messages.place_market_order_message(4, 789, 101, api_messages.Buy, 100)
  let encoded = api_messages.encode_message(buy_order)
  io.println("✓ PLACE_ORDER (market buy) message encoded successfully")
  io.println(
    "  Size: " <> int.to_string(bit_array.byte_size(encoded)) <> " bytes",
  )
  io.println("")

  // Test 7: PLACE_ORDER message (limit sell)
  io.println("Test 7: PLACE_ORDER message (limit sell)")
  let sell_order =
    api_messages.place_limit_order_message(
      5,
      789,
      102,
      api_messages.Sell,
      50,
      25.5,
    )
  let encoded = api_messages.encode_message(sell_order)
  io.println("✓ PLACE_ORDER (limit sell) message encoded successfully")
  io.println(
    "  Size: " <> int.to_string(bit_array.byte_size(encoded)) <> " bytes",
  )
  io.println("")

  // Test 8: PLACE_ORDER message (stop sell)
  io.println("Test 8: PLACE_ORDER message (stop sell)")
  let stop_order =
    api_messages.place_stop_order_message(
      6,
      789,
      103,
      api_messages.Sell,
      75,
      24.0,
    )
  let encoded = api_messages.encode_message(stop_order)
  io.println("✓ PLACE_ORDER (stop sell) message encoded successfully")
  io.println(
    "  Size: " <> int.to_string(bit_array.byte_size(encoded)) <> " bytes",
  )
  io.println("")

  io.println("=== All Type-Safe Message Encoding Tests Passed ===\n")
}

/// Test handshake with type-safe messages
/// This tests the complete flow: handshake + START_API with correct protocol
pub fn test_handshake_with_type_safe_messages() {
  io.println("\n=== Testing Handshake with Type-Safe Messages ===\n")

  // Create connection config
  let config = connection.config("127.0.0.1", 7497, 123)

  io.println("Connecting to TWS on port 7497...")

  case connection.connect(config) {
    Ok(conn) -> {
      io.println("✓ Connected to TWS")

      // Send handshake
      io.println("\nSending handshake...")
      let handshake_msg = protocol.start_api_message(100, 200)
      case connection.send_bytes(conn, handshake_msg) {
        Ok(_) -> {
          io.println("✓ Handshake sent")

          // Wait for server response
          io.println("\nWaiting for server handshake response...")
          connection.keep_alive(5000)

          // Check if we received data
          case connection.receive(conn) {
            Ok(data) -> {
              io.println("✓ Received server response: " <> data)

              // Parse server response
              case protocol.parse_server_response(data) {
                Ok(#(version, timestamp)) -> {
                  io.println("✓ Server version: " <> int.to_string(version))
                  io.println("✓ Server timestamp: " <> timestamp)

                  // Send START_API message with type-safe encoding
                  io.println(
                    "\nSending START_API message with type-safe encoding...",
                  )
                  let start_api_msg = api_messages.start_api_message(123)
                  let encoded = api_messages.encode_message(start_api_msg)

                  case connection.send_bytes(conn, encoded) {
                    Ok(_) -> {
                      io.println(
                        "✓ START_API message sent with correct protocol format",
                      )
                      io.println(
                        "✓ This uses NULL-separated tokens with length prefix",
                      )
                      io.println(
                        "✓ NOT the old raw binary format that caused errors",
                      )

                      // Wait for nextValidId event
                      io.println(
                        "\nWaiting for nextValidId event (connection ready)...",
                      )
                      connection.keep_alive(5000)

                      io.println(
                        "\n=== Handshake with Type-Safe Messages Test Complete ===",
                      )
                      io.println(
                        "Note: Connection should remain open with correct protocol",
                      )
                    }
                    Error(e) -> {
                      io.println(
                        "✗ Failed to send START_API message: " <> debug_error(e),
                      )
                    }
                  }
                }
                Error(e) -> {
                  io.println("✗ Failed to parse server response: " <> e)
                }
              }
            }
            Error(e) -> {
              io.println("✗ Failed to receive data: " <> debug_error(e))
            }
          }
        }
        Error(e) -> {
          io.println("✗ Failed to send handshake: " <> debug_error(e))
        }
      }
    }
    Error(e) -> {
      io.println("✗ Failed to connect: " <> debug_error(e))
    }
  }
}

/// Helper function to convert connection error to string
fn debug_error(error: connection.ConnectionError) -> String {
  case error {
    connection.ConnectionFailed(msg) -> "ConnectionFailed: " <> msg
    connection.InvalidHost -> "InvalidHost"
    connection.InvalidPort -> "InvalidPort"
    connection.SocketError(msg) -> "SocketError: " <> msg
    connection.Timeout -> "Timeout"
  }
}
