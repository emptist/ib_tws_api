import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api/client.{Client}
import ib_tws_api/protocol
import ib_tws_api/socket

pub fn main() {
  io.println("=== Raw Read Test ===")

  // Configuration - easily change port here
  let port = 7497
  // Paper trading port
  let host = "127.0.0.1"
  let client_id = 702_820

  io.println("Port: " <> int.to_string(port))
  io.println("")

  // Test 1: Connect and send greeting
  io.println("Test 1: Connect and send greeting message")
  io.println("----------------------------------------")

  let _ = case socket.connect_socket(host, port) {
    Ok(socket) -> {
      io.println("✓ Socket connected")

      // Send greeting message
      let greeting = protocol.encode_greeting_message()
      case socket.send_raw_bytes(socket, greeting) {
        Ok(_) -> {
          io.println("✓ Greeting sent")
          io.println("")

          // Try to read raw bytes from server
          io.println("Attempting to read raw bytes from server...")

          case socket.tcp_recv(socket, 4096, 5000) {
            Ok(data) -> {
              io.println("✓ Received data!")
              io.println(
                "Data length: " <> int.to_string(bit_array.byte_size(data)),
              )
              io.println("Raw bytes: " <> string.inspect(data))

              // Try to parse as UTF-8 string
              case bit_array.to_string(data) {
                Ok(str) -> {
                  io.println("As UTF-8: " <> str)
                }
                Error(_) -> {
                  io.println("Could not decode as UTF-8 (binary data)")
                }
              }
            }
            Error(err) -> {
              io.println("✗ Receive error: " <> string.inspect(err))
            }
          }
        }
        Error(err) -> {
          io.println("✗ Failed to send greeting: " <> string.inspect(err))
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
  Nil
}
