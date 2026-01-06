import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string

pub fn main() {
  io.println("=== Testing Size Prefix Formats ===")
  io.println("")

  let version_str = "176"
  let client_id_str = "1"

  let body =
    <<
      0:int-little-size(32),
      version_str:utf8, 0:int-size(8),
      client_id_str:utf8, 0:int-size(8),
    >>

  io.println("Body:")
  io.println("  Raw bytes: " <> string.inspect(body))
  io.println("  Byte size: " <> int.to_string(bit_array.byte_size(body)))
  io.println("")

  let size = bit_array.byte_size(body)
  let with_size = <<size:int-little-size(32), body:bits>>

  io.println("Format 1 (body size, excluding size prefix):")
  io.println("  Size value: " <> int.to_string(size))
  io.println("  Total bytes: " <> int.to_string(bit_array.byte_size(with_size)))
  io.println("  Raw: " <> string.inspect(with_size))
  io.println("")

  let size_including_prefix = bit_array.byte_size(with_size)
  let with_size_included = <<size_including_prefix:int-little-size(32), body:bits>>

  io.println("Format 2 (size including prefix, so total message size):")
  io.println("  Size value: " <> int.to_string(size_including_prefix))
  io.println("  Total bytes: " <> int.to_string(bit_array.byte_size(with_size_included)))
  io.println("  Raw: " <> string.inspect(with_size_included))
  io.println("")

  io.println("Expected from TWS docs:")
  io.println("  ConnectRequest: <size=4+len(version)+1+len(clientId)+1><msgId=0><version><clientId>")
  io.println("  For clientId=1, version=176:")
  io.println("  Size should be: 4 + 3 + 1 + 1 + 1 = 10 bytes (excluding size prefix)")
  io.println("")

  io.println("Test with explicit 10-byte size:")
  let explicit_10 = <<10:int-little-size(32), body:bits>>
  io.println("  Raw: " <> string.inspect(explicit_10))
  io.println("")
}
