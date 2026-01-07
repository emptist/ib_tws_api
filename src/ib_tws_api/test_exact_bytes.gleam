import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api/socket

// Construct greeting exactly as IBKit does
fn create_greeting_like_ibkit() -> BitArray {
  // "API\0" as ASCII bytes
  let prefix_bytes = <<65, 80, 73, 0>>

  // Version string
  let version = "v142..177"
  let version_bytes = bit_array.from_string(version)

  // Length as little-endian: 9 -> [9, 0, 0, 0]
  let len_bytes = <<9, 0, 0, 0>>

  // Combine: prefix + length + version
  bit_array.append(bit_array.append(prefix_bytes, len_bytes), version_bytes)
}

pub fn main() {
  io.println("=== Exact Bytes Test ===")

  let port = 7497
  let host = "127.0.0.1"

  io.println("Port: " <> int.to_string(port))
  io.println("")

  let greeting = create_greeting_like_ibkit()

  io.println("Greeting bytes (as constructed):")
  io.println("  " <> string.inspect(greeting))
  io.println("  Length: " <> int.to_string(bit_array.byte_size(greeting)))
  io.println("")

  io.println("Expected bytes (from IBKit):")
  io.println(
    "  <<65, 80, 73, 0, 9, 0, 0, 0, 118, 49, 52, 50, 46, 46, 49, 55, 55>>",
  )
  io.println("")

  io.println("Connecting to TWS...")

  let _ = case socket.connect_socket(host, port) {
    Ok(socket) -> {
      io.println("✓ Connected")
      io.println("")

      case socket.send_raw_bytes(socket, greeting) {
        Ok(_) -> {
          io.println("✓ Greeting sent")
          io.println("")

          case socket.tcp_recv(socket, 4096, 5000) {
            Ok(data) -> {
              io.println("✓ Received response!")
              io.println("  Raw bytes: " <> string.inspect(data))
              io.println(
                "  Length: " <> int.to_string(bit_array.byte_size(data)),
              )
              io.println("")

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
