import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string

// Test to verify BitArray to Buffer conversion works correctly
pub fn main() {
  io.println("Testing BitArray conversion...")

  // Test 1: Simple integer to bytes
  let test_value = 8
  let bytes = int_to_four_bytes_big_endian(test_value)
  let size = bit_array.byte_size(bytes)
  io.println("Test 1: int_to_four_bytes_big_endian(8)")
  io.println("  Expected size: 4 bytes")
  io.println("  Actual size: " <> int.to_string(size))
  io.println("  Expected hex: 00 00 00 08")

  // Test 2: Version string "v100"
  let version_string = "v100"
  let version_bytes = bit_array.from_string(version_string)
  let version_size = bit_array.byte_size(version_bytes)
  io.println("\nTest 2: version string 'v100'")
  io.println("  Expected size: 4 bytes")
  io.println("  Actual size: " <> int.to_string(version_size))

  // Test 3: Complete handshake message
  let version_length = string.length(version_string)
  let length_bytes = int_to_four_bytes_big_endian(version_length)
  let api_bytes = <<65:8, 80:8, 73:8, 0:8>>
  let handshake = bit_array.concat([api_bytes, length_bytes, version_bytes])
  let handshake_size = bit_array.byte_size(handshake)
  io.println("\nTest 3: Complete handshake message")
  io.println("  Expected size: 12 bytes (4 + 4 + 4)")
  io.println("  Actual size: " <> int.to_string(handshake_size))
  io.println("  Expected hex: 41 50 49 00 00 00 00 04 76 31 30 30")

  io.println("\n✅ All tests completed")
}

fn int_to_four_bytes_big_endian(value: Int) -> BitArray {
  let b3 = value / 16_777_216
  let temp = value / 65_536
  let b2 = temp % 256
  let temp2 = value / 256
  let b1 = temp2 % 256
  let b0 = value % 256
  <<b3:8, b2:8, b1:8, b0:8>>
}
