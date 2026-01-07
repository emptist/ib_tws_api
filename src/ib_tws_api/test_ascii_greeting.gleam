import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api/socket

// Create greeting using explicit ASCII encoding like IBKit
fn encode_greeting_ascii() -> BitArray {
  let prefix = "API"
  let version = "v142..177"

  // Manually construct: "API\0" + 4-byte length + version
  // Using ASCII (1 byte per character)
  let prefix_bytes = <<65, 80, 73, 0>>
  // "API\0" in ASCII
  let version_bytes = bit_array.from_string(version)
  let version_len = bit_array.byte_size(version_bytes)

  // 4-byte big-endian length
  let len_bytes = <<version_len:int-big-size(32)>>

  <<prefix_bytes:bits, len_bytes:bits, version_bytes:bits>>
}

pub fn main() {
  io.println("=== ASCII Greeting Test ===")

  let port = 7497
  let host = "127.0.0.1"

  io.println("Port: " <> int.to_string(port))
  io.println("")

  io.println("Connecting to TWS...")

  let _ = case socket.connect_socket(host, port) {
    Ok(socket) -> {
      io.println("✓ Connected")
      io.println("")

      // Send ASCII greeting
      let greeting = encode_greeting_ascii()
      io.println("Sending ASCII greeting:")
      io.println("  Raw bytes: " <> string.inspect(greeting))
      io.println("  Length: " <> int.to_string(bit_array.byte_size(greeting)))

      case socket.send_raw_bytes(socket, greeting) {
        Ok(_) -> {
          io.println("✓ Greeting sent")
          io.println("")

          // Wait for response
          io.println("Waiting for server response...")

          case socket.tcp_recv(socket, 4096, 5000) {
            Ok(data) -> {
              io.println("✓ Received response!")
              io.println("  Raw bytes: " <> string.inspect(data))
              io.println(
                "  Length: " <> int.to_string(bit_array.byte_size(data)),
              )
              io.println("")

              // Try to parse as string
              case bit_array.to_string(data) {
                Ok(str) -> {
                  io.println("As string:")
                  io.println("  " <> str)
                }
                Error(_) -> {
                  io.println("Could not decode as string")
                }
              }
            }
            Error(err) -> {
              io.println("✗ Receive error: " <> string.inspect(err))
            }
          }
        }
        Error(err) -> {
          io.println("✗ Send error: " <> string.inspect(err))
        }
      }

      let _ = socket.close_socket(socket)
      Ok(Nil)
    }
    Error(_err) -> {
      io.println("✗ Connection failed")
      Ok(Nil)
    }
  }

  io.println("")
  io.println("=== Test Complete ===")
}
