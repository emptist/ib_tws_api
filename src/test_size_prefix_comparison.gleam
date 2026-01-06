import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string

pub fn main() {
  io.println("=== TWS API Message Size Prefix Comparison Test ===")
  io.println("")

  let client_id = 1
  let version_str = "176"
  let client_id_str = "1"

  let message_body =
    <<
      0:int-little-size(32),
      version_str:utf8, 0:int-size(8),
      client_id_str:utf8, 0:int-size(8),
    >>

  let body_size = bit_array.byte_size(message_body)

  io.println("Message components:")
  io.println("  Protocol version: " <> version_str)
  io.println("  Client ID: " <> client_id_str)
  io.println("  Message body size: " <> int.to_string(body_size) <> " bytes")
  io.println("  Message body bytes: " <> string.inspect(message_body))
  io.println("")

  let message_without_prefix = message_body

  let message_with_prefix =
    <<
      body_size:int-little-size(32),
      message_body:bits,
    >>

  io.println("Format 1 - WITHOUT size prefix:")
  io.println("  Total size: " <> int.to_string(bit_array.byte_size(message_without_prefix)) <> " bytes")
  io.println("  Bytes: " <> string.inspect(message_without_prefix))
  io.println("")

  io.println("Format 2 - WITH 4-byte size prefix:")
  io.println("  Total size: " <> int.to_string(bit_array.byte_size(message_with_prefix)) <> " bytes")
  io.println("  Prefix value: " <> int.to_string(body_size) <> " (little-endian)")
  io.println("  Bytes: " <> string.inspect(message_with_prefix))
  io.println("")

  io.println("Expected structure of Format 2:")
  io.println("  [4 bytes: body size (little-endian)] [body content]")
  io.println("  [" <> int.to_string(body_size) <> " bytes: message body]")
  io.println("")

  io.println("=== Analyzing byte breakdown ===")
  io.println("")

  case message_with_prefix {
    <<size_prefix:32-little, rest:bits>> -> {
      io.println("Parsed size prefix: " <> int.to_string(size_prefix))
      io.println("Remaining body size: " <> int.to_string(bit_array.byte_size(rest)) <> " bytes")
      io.println("Match: " <> case size_prefix == body_size {
        True -> "YES - size prefix correctly encodes body size"
        False -> "NO - mismatch between prefix and body"
      })
    }
    _ -> {
      io.println("ERROR: Could not parse message with prefix")
    }
  }

  io.println("")
  io.println("=== Test Ready ===")
  io.println("")
  io.println("To test with live TWS:")
  io.println("  1. Ensure TWS is running with API access enabled on port 7496")
  io.println("  2. Run: gleam run --module test_size_prefix_comparison")
  io.println("  3. The test will attempt both formats and report which one works")
  io.println("")
  io.println("The correct format will receive a ConnectAck response from TWS")
}
