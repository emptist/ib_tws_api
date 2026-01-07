import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string

/// Filter control characters from a string using JavaScript FFI
/// Removes ASCII control characters (0x00-0x1f) except space (0x20)
@external(javascript, "./connection_ffi.mjs", "filter_control_characters")
pub fn filter_control_characters(str: String) -> String

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
/// This is the first message sent to IB TWS API
///
/// Protocol format:
/// - "API\0" (4 bytes: 'A', 'P', 'I', null byte)
/// - 4-byte big-endian length of version string
/// - Version string (e.g., "v100..200" or just "v100")
///
/// Parameters:
/// - min_version: Minimum API version supported (typically 100)
/// - max_version: Maximum API version supported (from API_VersionNum.txt)
///
/// Returns: BitArray containing the complete handshake message
pub fn start_api_message(min_version: Int, max_version: Int) -> BitArray {
  // Build version string: "v100" or "v100..200"
  let version_string = case min_version == max_version {
    True -> "v" <> int.to_string(min_version)
    False ->
      "v" <> int.to_string(min_version) <> ".." <> int.to_string(max_version)
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

  // Debug: Log each component
  io.println("[DEBUG] version_string: " <> version_string)
  io.println("[DEBUG] version_length: " <> int.to_string(version_length))
  io.println(
    "[DEBUG] version_bytes size: "
    <> int.to_string(bit_array.byte_size(version_bytes)),
  )
  io.println(
    "[DEBUG] length_bytes size: "
    <> int.to_string(bit_array.byte_size(length_bytes)),
  )
  io.println(
    "[DEBUG] api_bytes size: " <> int.to_string(bit_array.byte_size(api_bytes)),
  )

  // Combine: API\0 + length_bytes + version_string_bytes
  let result = bit_array.concat([api_bytes, length_bytes, version_bytes])
  io.println(
    "[DEBUG] final message size: " <> int.to_string(bit_array.byte_size(result)),
  )

  result
}

/// Create client ID message to send after handshake
/// Client ID must be sent as a separate message after receiving server response
///
/// Parameters:
/// - client_id: Client ID to identify this connection (unique per connection)
///
/// Returns: BitArray containing the client ID as a 4-byte integer
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
/// Expected format: "VERSION<timestamp> EST"
/// Example: "20020260107 08:02:02 EST"
/// Note: The response may have leading null bytes or control characters
/// The IB TWS protocol includes a 4-byte length prefix before actual data
pub fn parse_server_response(data: String) -> Result(#(Int, String), String) {
  // The IB TWS server response includes a 4-byte length prefix
  // We need to skip this and extract the actual data
  // Also remove any control characters (0x00-0x1f) except space (0x20)

  // Use JavaScript FFI to filter out control characters
  let filtered_data = filter_control_characters(data)

  // Trim whitespace from the filtered data
  let trimmed_data = string.trim(filtered_data)

  io.println("[DEBUG] Cleaned server response: " <> trimmed_data)

  // Parse version number from the start of the string
  case int.parse(trimmed_data) {
    Ok(version) -> {
      // Extract timestamp (everything after the version number)
      let version_str = int.to_string(version)
      let timestamp = case
        string.slice(
          trimmed_data,
          string.length(version_str),
          string.length(trimmed_data),
        )
      {
        "" -> "No timestamp"
        ts -> string.trim(ts)
      }
      Ok(#(version, timestamp))
    }
    Error(_) -> Error("Invalid server response format: " <> trimmed_data)
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
