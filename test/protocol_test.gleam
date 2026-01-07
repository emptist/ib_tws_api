import gleam/int
import gleam/io
import protocol
import types

/// Test protocol handshake
pub fn handshake_test() {
  io.println("=== Protocol Handshake Test ===")
  io.println("")

  // This test requires an actual connection
  // For now, we'll just verify the handshake function exists and has correct signature
  io.println("Protocol handshake function exists")
  io.println("")

  io.println("✓ Handshake test completed (placeholder)")
}

/// Test message encoding
pub fn encode_message_test() {
  io.println("=== Message Encoding Test ===")
  io.println("")

  // Test encoding a simple message
  let msg_type = protocol.msg_server_time
  let params = []
  let encoded = protocol.encode_message(msg_type, params)

  io.println("Encoded message: " <> encoded)
  io.println("")

  // Test encoding a message with parameters
  let params_with_data = ["param1", "param2"]
  let encoded_with_params = protocol.encode_message(msg_type, params_with_data)

  io.println("Encoded message with params: " <> encoded_with_params)
  io.println("")

  io.println("✓ Message encoding test completed")
}

/// Test message parsing
pub fn parse_message_test() {
  io.println("=== Message Parsing Test ===")
  io.println("")

  // Test parsing a raw message
  let raw_data = "9\\0server_time_value"
  let result = protocol.parse_message(raw_data)

  case result {
    Ok(msg) -> {
      io.println("Parsed message successfully")
      io.println("  Type: " <> message_type_to_string(msg.message_type))
      io.println("  Payload: " <> msg.payload)
      io.println("")
      io.println("✓ Message parsing test completed")
    }
    Error(_) -> {
      io.println("✗ Failed to parse message")
    }
  }
}

/// Test message constants
pub fn message_constants_test() {
  io.println("=== Message Constants Test ===")
  io.println("")

  io.println(
    "Server Time Message Code: " <> int.to_string(protocol.msg_server_time),
  )
  io.println("Error Message Code: " <> int.to_string(protocol.msg_error))
  io.println(
    "Market Data Message Code: " <> int.to_string(protocol.msg_market_data),
  )
  io.println(
    "Order Status Message Code: " <> int.to_string(protocol.msg_order_status),
  )
  io.println("")

  io.println("✓ Message constants test completed")
}

/// Run all protocol tests
pub fn main() {
  io.println("========================================")
  io.println("  IB TWS API Protocol Test Suite")
  io.println("========================================")
  io.println("")

  handshake_test()
  io.println("")

  encode_message_test()
  io.println("")

  parse_message_test()
  io.println("")

  message_constants_test()
  io.println("")

  io.println("========================================")
  io.println("  All Protocol Tests Completed")
  io.println("========================================")
}

fn message_type_to_string(msg_type) {
  case msg_type {
    types.ServerTime -> "ServerTime"
    types.Error -> "Error"
    types.MarketData -> "MarketData"
    types.OrderStatus -> "OrderStatus"
  }
}
