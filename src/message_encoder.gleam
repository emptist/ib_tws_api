import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/string

/// Message types for IB TWS API requests
pub type RequestType {
  RequestAccountSummary
  RequestPositions
  RequestOpenOrders
}

/// Create REQ_ACCOUNT_SUMMARY message with length prefix
/// Request ID: 6
/// Parameters:
/// - request_id: Request ID to identify this request
/// - group_code: Account group code (e.g., "All")
/// - tags: Tags for data requested (e.g., "$LEDGER:ALL")
///
/// Returns: BitArray with [4-byte length][6\0<request_id>\0<group_code>\0<tags>\0]
pub fn request_account_summary_with_length(
  request_id: Int,
  group_code: String,
  tags: String,
) -> BitArray {
  // Message format: "6\u{0000}request_id\u{0000}group_code\u{0000}tags\u{0000}"
  // All fields are NULL-separated, ends with NULL

  let tokens = [
    int_to_string(6),
    // REQ_ACCOUNT_SUMMARY message ID
    int_to_string(request_id),
    group_code,
    tags,
  ]

  // Join with NULL and add final NULL terminator
  let message_string = string.join(tokens, "\u{0000}") <> "\u{0000}"

  io.println("[Encoder] REQ_ACCOUNT_SUMMARY string: " <> message_string)

  // Add 4-byte big-endian length prefix
  let result = add_length_prefix_to_string(message_string)

  io.println(
    "[Encoder] REQ_ACCOUNT_SUMMARY with length: "
    <> int.to_string(bit_array.byte_size(result))
    <> " bytes",
  )
  result
}

/// Create REQ_ACCOUNT_SUMMARY message without length prefix
/// Request ID: 6
/// Parameters:
/// - request_id: Request ID to identify this request
/// - group_code: Account group code (e.g., "All")
/// - tags: Tags for data requested (e.g., "$LEDGER:ALL")
///
/// Returns: String with NULL-separated tokens (length prefix added by socket layer)
pub fn request_account_summary(
  request_id: Int,
  group_code: String,
  tags: String,
) -> String {
  // Message format: "6\u{0000}request_id\u{0000}group_code\u{0000}tags\u{0000}"
  // All fields are NULL-separated, ends with NULL
  // Length prefix is added by socket layer, NOT here

  let tokens = [
    int_to_string(6),
    // REQ_ACCOUNT_SUMMARY message ID
    int_to_string(request_id),
    group_code,
    tags,
  ]

  // Join with NULL and add final NULL terminator
  let result = string.join(tokens, "\u{0000}") <> "\u{0000}"

  io.println("[Encoder] REQ_ACCOUNT_SUMMARY (no length): " <> result)
  result
}

/// Create REQ_POSITIONS message with length prefix
/// Request ID: 7
/// Parameters:
/// - request_id: Request ID to identify this request
///
/// Returns: BitArray with [4-byte length][7\0<request_id>\0]
pub fn request_positions_with_length(request_id: Int) -> BitArray {
  // Message format: "7\u{0000}request_id\u{0000}"

  let tokens = [
    int_to_string(7),
    // REQ_POSITIONS message ID
    int_to_string(request_id),
  ]

  // Join with NULL and add final NULL terminator
  let message_string = string.join(tokens, "\u{0000}") <> "\u{0000}"

  io.println("[Encoder] REQ_POSITIONS string: " <> message_string)

  // Add 4-byte big-endian length prefix
  let result = add_length_prefix_to_string(message_string)

  io.println(
    "[Encoder] REQ_POSITIONS with length: "
    <> int.to_string(bit_array.byte_size(result))
    <> " bytes",
  )
  result
}

/// Create REQ_POSITIONS message without length prefix
/// Request ID: 7
/// Parameters:
/// - request_id: Request ID to identify this request
///
/// Returns: String with NULL-separated tokens (length prefix added by socket layer)
pub fn request_positions(request_id: Int) -> String {
  // Message format: "7\u{0000}request_id\u{0000}"
  // Length prefix is added by socket layer, NOT here

  let tokens = [
    int_to_string(7),
    // REQ_POSITIONS message ID
    int_to_string(request_id),
  ]

  // Join with NULL and add final NULL terminator
  let result = string.join(tokens, "\u{0000}") <> "\u{0000}"

  io.println("[Encoder] REQ_POSITIONS (no length): " <> result)
  result
}

/// Create REQ_OPEN_ORDERS message with length prefix
/// Request ID: 9
///
/// Returns: BitArray with [4-byte length][9\0]
pub fn request_open_orders_with_length() -> BitArray {
  // Message format: "9\u{0000}"

  let tokens = [
    int_to_string(9),
    // REQ_OPEN_ORDERS message ID
  ]

  // Join with NULL and add final NULL terminator
  let message_string = string.join(tokens, "\u{0000}") <> "\u{0000}"

  io.println("[Encoder] REQ_OPEN_ORDERS string: " <> message_string)

  // Add 4-byte big-endian length prefix
  let result = add_length_prefix_to_string(message_string)

  io.println(
    "[Encoder] REQ_OPEN_ORDERS with length: "
    <> int.to_string(bit_array.byte_size(result))
    <> " bytes",
  )
  result
}

/// Create REQ_OPEN_ORDERS message without length prefix
/// Request ID: 9
///
/// Returns: String with NULL-separated tokens (length prefix added by socket layer)
pub fn request_open_orders() -> String {
  // Message format: "9\u{0000}"
  // Length prefix is added by socket layer, NOT here

  let tokens = [
    int_to_string(9),
    // REQ_OPEN_ORDERS message ID
  ]

  // Join with NULL and add final NULL terminator
  let result = string.join(tokens, "\u{0000}") <> "\u{0000}"

  io.println("[Encoder] REQ_OPEN_ORDERS (no length): " <> result)
  result
}

/// Encode a message with message ID and tokens
/// This is a helper function to create properly formatted IB TWS messages
/// Parameters:
/// - message_id: The message ID (e.g., 6 for REQ_ACCOUNT_SUMMARY)
/// - tokens: List of string tokens for the message
///
/// Returns: String with NULL-separated tokens (length prefix added by socket layer)
pub fn encode_message_tokens(message_id: Int, tokens: List(String)) -> String {
  // Convert message ID to string and prepend to tokens
  let message_id_str = int_to_string(message_id)
  let all_tokens = [message_id_str, ..tokens]

  // Join with NULL and add final NULL terminator
  let result = string.join(all_tokens, "\u{0000}") <> "\u{0000}"

  io.println(
    "[Encoder] Message " <> int_to_string(message_id) <> ": " <> result,
  )
  result
}

/// Convert integer to string
/// IB TWS encodes integers as ASCII strings in messages
fn int_to_string(value: Int) -> String {
  int.to_string(value)
}

/// Create START_API message (client ID message) with length prefix
/// This is sent after receiving the server handshake response
/// This is the CORRECT format that TWS expects
///
/// Parameters:
/// - client_id: Client ID to identify this connection
///
/// Returns: BitArray with [4-byte length][71\02\0<client_id>\0\0]
/// Format: 4-byte big-endian length + NULL-separated tokens
pub fn start_api_message_with_length(client_id: Int) -> BitArray {
  // Message format: "71\u{0000}2\u{0000}client_id\u{0000}\u{0000}"
  // 71 = START_API message ID
  // 2 = version (fixed)
  // client_id = client identifier
  // "" = optional capabilities (empty)

  let tokens = [
    int_to_string(71),
    // START_API message ID
    int_to_string(2),
    // Version (fixed at 2)
    int_to_string(client_id),
    "",
    // Optional capabilities (empty string)
  ]

  // Join with NULL and add final NULL terminator
  let message_string = string.join(tokens, "\u{0000}") <> "\u{0000}"

  io.println("[Encoder] START_API message string: " <> message_string)

  // Add 4-byte big-endian length prefix
  let result = add_length_prefix_to_string(message_string)

  io.println(
    "[Encoder] START_API message with length: "
    <> int.to_string(bit_array.byte_size(result))
    <> " bytes",
  )
  result
}

/// Create START_API message (client ID message) without length prefix
/// This is sent after receiving the server handshake response
/// DEPRECATED: Use start_api_message_with_length() instead for correct protocol
///
/// Parameters:
/// - client_id: Client ID to identify this connection
///
/// Returns: String with NULL-separated tokens (length prefix added by socket layer)
pub fn start_api_message(client_id: Int) -> String {
  // Message format: "71\u{0000}2\u{0000}client_id\u{0000}\u{0000}"
  // 71 = START_API message ID
  // 2 = version (fixed)
  // client_id = client identifier
  // "" = optional capabilities (empty)

  let tokens = [
    int_to_string(71),
    // START_API message ID
    int_to_string(2),
    // Version (fixed at 2)
    int_to_string(client_id),
    "",
    // Optional capabilities (empty string)
  ]

  // Join with NULL and add final NULL terminator
  let result = string.join(tokens, "\u{0000}") <> "\u{0000}"

  io.println("[Encoder] START_API (no length): " <> result)
  result
}

/// Add 4-byte big-endian length prefix to a string message
/// This is called by the socket layer when sending messages
/// Parameters:
/// - message: The NULL-separated message string
///
/// Returns: BitArray with length prefix + message data
pub fn add_length_prefix_to_string(message: String) -> BitArray {
  let size = string.length(message)
  let length_bytes = int_to_four_bytes_big_endian(size)
  let message_bytes = bit_array.from_string(message)
  bit_array.concat([length_bytes, message_bytes])
}

/// Convert integer to 4 bytes (big-endian)
fn int_to_four_bytes_big_endian(value: Int) -> BitArray {
  let b3 = value / 16_777_216
  let temp = value / 65_536
  let b2 = temp % 256
  let temp2 = value / 256
  let b1 = temp2 % 256
  let b0 = value % 256
  <<b3:8, b2:8, b1:8, b0:8>>
}
