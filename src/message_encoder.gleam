import gleam/bit_array
import gleam/int
import gleam/string

/// Message types for IB TWS API requests
pub type RequestType {
  RequestAccountSummary
  RequestPositions
  RequestOpenOrders
}

/// Create REQ_ACCOUNT_SUMMARY message
/// Request ID: 6
/// Parameters:
/// - request_id: Request ID to identify this request
/// - group_code: Account group code (e.g., "All")
/// - tags: Tags for data requested (e.g., "$LEDGER:ALL")
pub fn request_account_summary(
  request_id: Int,
  group_code: String,
  tags: String,
) -> BitArray {
  // Message format: 6<NULL>request_id<NULL>group_code<NULL>tags
  // All fields are NULL-separated

  let request_id_bytes = int_to_bytes(request_id)
  let group_bytes = bit_array.from_string(group_code)
  let tags_bytes = bit_array.from_string(tags)

  // Combine all parts with NULL separators
  let payload =
    bit_array.concat([
      <<6:16>>,
      // 2-byte big-endian message code
      request_id_bytes,
      <<0:8>>,
      group_bytes,
      <<0:8>>,
      tags_bytes,
    ])

  // Add 4-byte length prefix (required by V100+ protocol)
  add_length_prefix(payload)
}

/// Create REQ_POSITIONS message
/// Request ID: 7
/// Parameters:
/// - request_id: Request ID to identify this request
pub fn request_positions(request_id: Int) -> BitArray {
  // Message format: 7<NULL>request_id

  let request_id_bytes = int_to_bytes(request_id)

  let payload = bit_array.concat([<<7:16>>, request_id_bytes])
  // 2-byte big-endian message code

  // Add 4-byte length prefix (required by V100+ protocol)
  add_length_prefix(payload)
}

/// Create REQ_OPEN_ORDERS message
/// Request ID: 9
pub fn request_open_orders() -> BitArray {
  // Message format: [4-byte length][2-byte code]
  let payload = <<9:16>>
  // 2-byte big-endian message code

  // Add 4-byte length prefix (required by V100+ protocol)
  add_length_prefix(payload)
}

/// Encode a message with message ID and payload
/// This is a helper function to create properly formatted IB TWS messages
/// Parameters:
/// - message_id: The message ID (e.g., 6 for REQ_ACCOUNT_SUMMARY)
/// - payload: The message payload as a BitArray
pub fn encode_message(message_id: Int, payload: BitArray) -> BitArray {
  // Create message ID byte followed by payload
  let message = bit_array.concat([<<message_id:8>>, payload])
  // Add length prefix (4 bytes big-endian)
  add_length_prefix(message)
}

/// Convert integer to variable-length bytes
/// IB TWS uses variable-length encoding for integers
fn int_to_bytes(value: Int) -> BitArray {
  // Simple encoding: just convert to bytes
  // In reality, IB TWS uses more complex variable-length encoding
  // For now, use simple ASCII encoding
  let str = int.to_string(value)
  bit_array.from_string(str)
}

/// Create a message with length prefix
/// IB TWS messages start with 4-byte length prefix (big-endian)
pub fn add_length_prefix(message: BitArray) -> BitArray {
  let size = bit_array.byte_size(message)
  let length_bytes = int_to_four_bytes_big_endian(size)
  bit_array.concat([length_bytes, message])
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
