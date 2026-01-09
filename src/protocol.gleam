import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/string

/// Convert string to hex representation for debugging
/// Uses JavaScript FFI for efficient conversion
@external(javascript, "./connection_ffi.mjs", "string_to_hex")
pub fn string_to_hex(str: String) -> String

/// Filter control characters from a string using JavaScript FFI
/// Removes ASCII control characters (0x00-0x1e) except space (0x20)
@external(javascript, "./connection_ffi.mjs", "strip_leading_control_characters")
pub fn strip_leading_control_characters(str: String) -> String

/// Clean server handshake response using JavaScript FFI
/// Removes leading length bytes and control characters, preserves NULL byte
/// Returns: Cleaned string ready for parsing
@external(javascript, "./connection_ffi.mjs", "clean_server_response")
pub fn clean_server_response(data: String) -> String

/// IB TWS API Message Types
pub type MessageCode {
  /// Start API message (initial handshake)
  StartApi
  /// Server time response
  ServerTime
  /// Market data request
  ReqMktData
  /// Market data snapshot
  TickPrice
  /// Contract data
  ContractData
  /// Order status
  OrderStatus
  /// Error message
  ErrorMessage
  /// Connection error
  ConnectError
}

/// IB TWS API Message structure
/// Messages are length-prefixed: 4 bytes for length, then message payload
pub type Message {
  Message(code: MessageCode, version: Int, data: BitArray)
}

/// Create START_API handshake message using V100+ protocol
/// This is first message sent to IB TWS API
///
/// Protocol format:
/// - "API\0" (4 bytes: 'A', 'P', 'I', null byte)
/// - 4-byte big-endian length of version string
/// - Version string (e.g., "v176.38" or just "v100")
///
/// Parameters:
/// - min_version: Minimum API version supported (typically 100)
/// - max_version: Maximum API version supported (from API_VersionNum.txt)
///
/// Returns: BitArray containing the complete handshake message
pub fn start_api_message(min_version: Int, max_version: Int) -> BitArray {
  // Build version string: "v100" or "v176.38"
  // Note: Use dot notation (.) not double dot (..) for version range
  let version_string = case min_version == max_version {
    True -> "v" <> int.to_string(min_version)
    False ->
      "v" <> int.to_string(min_version) <> "." <> int.to_string(max_version)
  }

  // Calculate version string length in bytes
  let version_length = string.length(version_string)

  // Convert version string to bit array (UTF-8 bytes)
  let version_bytes = bit_array.from_string(version_string)

  // Create 4-byte big-endian length
  let length_bytes = int_to_four_bytes_big_endian(version_length)

  // Create "API\0" bytes using bit array syntax
  // 'A'=65, 'P'=80, 'I'=73, null=0
  let api_bytes = <<65:8, 80:8, 73:8, 0:8>>

  // Combine: API\0 + length_bytes + version_string_bytes
  // Note: Must be sent immediately after connection with no delay
  bit_array.concat([api_bytes, length_bytes, version_bytes])
}

/// Create START_API message with proper length prefix for sending
/// This wraps the handshake in the standard IB message format:
/// 4-byte length + message payload
pub fn start_api_message_with_length(
  min_version: Int,
  max_version: Int,
) -> BitArray {
  let message = start_api_message(min_version, max_version)
  let length = bit_array.byte_size(message)
  bit_array.concat([int_to_four_bytes_big_endian(length), message])
}

/// DEPRECATED: Use message_encoder.start_api_message_with_length() instead
/// This function is kept for backward compatibility but should not be used
/// It sends raw binary instead of the correct NULL-separated format
pub fn client_id_message(client_id: Int) -> BitArray {
  int_to_four_bytes_big_endian(client_id)
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

/// Parse server handshake response
/// Expected format: "VERSION<NULL separator>timestamp EST"
/// Example: "200\020260107 08:02:02 EST"
/// Note: The response may have leading null bytes or control characters
/// The IB TWS protocol includes a 4-byte length prefix before actual data
/// Version and timestamp are separated by NULL byte (0x00)
pub fn parse_server_response(data: String) -> Result(#(Int, String), String) {
  // Clean the data first using FFI (removes control chars, preserves NULL)
  let cleaned = clean_server_response(data)

  // Split on NULL byte (0x00)
  let parts = string.split(cleaned, "\u{0000}")

  // Filter out empty strings from leading/trailing NULLs
  let non_empty_parts = list.filter(parts, fn(s) { s != "" })

  case non_empty_parts {
    [version_str, ..rest] -> {
      // Parse version number
      case int.parse(string.trim(version_str)) {
        Ok(version) -> {
          // Join remaining parts for timestamp (in case timestamp contains NULL)
          let timestamp_str = string.join(rest, "\u{0000}")
          Ok(#(version, string.trim(timestamp_str)))
        }
        Error(_) -> Error("Invalid version number: " <> version_str)
      }
    }
    _ -> {
      // No NULL byte found, try to parse entire string as version
      case int.parse(string.trim(cleaned)) {
        Ok(version) -> Ok(#(version, "No timestamp"))
        Error(_) -> Error("Invalid server response format: " <> cleaned)
      }
    }
  }
}

/// Parse a message from received bytes
/// Returns message code and version if valid
pub fn parse_message(data: BitArray) -> Result(MessageCode, String) {
  // Simple parsing - check message type based on first few bytes
  // Full protocol parsing will be implemented as needed

  let size = bit_array.byte_size(data)
  case size {
    0 -> Error("Empty message")
    _ -> {
      // Check for known message patterns
      // This is a simplified parser - will be expanded as needed
      Ok(StartApi)
    }
  }
}
