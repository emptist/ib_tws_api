import connection
import gleam/bit_array
import gleam/io

// Test to verify what Gleam passes to FFI layer
pub fn main() {
  io.println("Testing BitArray FFI conversion...")

  // Create a simple BitArray
  let test_data = <<65:8, 80:8, 73:8, 0:8>>
  io.println("Created BitArray: <<65:8, 80:8, 73:8, 0:8>>")

  // Try to send it (this will fail because we don't have a real connection)
  // But we can see what gets logged
  io.println("\nAttempting to send via FFI...")

  // We can't actually send without a connection, but let's test the conversion
  // by calling the FFI function directly
  let result = send_bytes_test(test_data)
  io.println("FFI conversion result: " <> result)
}

@external(javascript, "./test_ffi_helper.mjs", "send_bytes_test")
fn send_bytes_test(data: BitArray) -> String
