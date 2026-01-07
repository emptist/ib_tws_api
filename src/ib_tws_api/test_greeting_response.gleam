import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import ib_tws_api/protocol
import ib_tws_api/socket

pub fn main() {
  io.println("=== Greeting Response Test ===")

  let port = 7497
  // Paper trading port
  let host = "127.0.0.1"
  let client_id = 778
  // Must match Master API client ID set in TWS

  io.println("Port: " <> int.to_string(port))
  io.println("")

  io.println("Connecting to TWS...")

  let _ = case socket.connect_socket(host, port) {
    Ok(socket) -> {
      io.println("✓ Connected")
      io.println("")

      // Send greeting message
      let greeting = protocol.encode_greeting_message()
      io.println("Sending greeting:")
      io.println("  Bytes: " <> string.inspect(greeting))
      io.println("  Length: " <> int.to_string(bit_array.byte_size(greeting)))
      io.println("")

      case socket.send_raw_bytes(socket, greeting) {
        Ok(_) -> {
          io.println("✓ Greeting sent")
          io.println("")

          // Read raw response (no length prefix expected)
          io.println("Reading server response (expecting version\\0time\\0)...")

          case socket.tcp_recv(socket, 4096, 5000) {
            Ok(data) -> {
              io.println("✓ Received data!")
              io.println("  Raw bytes: " <> string.inspect(data))
              io.println(
                "  Length: " <> int.to_string(bit_array.byte_size(data)),
              )
              io.println("")

              // Try to parse as UTF-8 string
              case bit_array.to_string(data) {
                Ok(str) -> {
                  io.println("As UTF-8 string:")
                  io.println("  " <> str)
                  io.println("")

                  // Parse version and time (format: version\0time\0)
                  case string.split(str, "\\0") {
                    [version, time, ""] -> {
                      io.println("Parsed response:")
                      io.println("  Server version: " <> version)
                      io.println("  Connection time: " <> time)
                    }
                    [version, time] -> {
                      io.println("Parsed response:")
                      io.println("  Server version: " <> version)
                      io.println("  Connection time: " <> time)
                    }
                    parts -> {
                      io.println(
                        "Unexpected format, got "
                        <> int.to_string(list.length(parts))
                        <> " parts",
                      )
                    }
                  }
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
}
