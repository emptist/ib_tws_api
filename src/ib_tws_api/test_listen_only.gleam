import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("=== Listen Only Test ===")

  let port = 7497
  let host = "127.0.0.1"

  io.println("Port: " <> int.to_string(port))
  io.println("")

  io.println("Connecting to TWS...")

  let _ = case socket.connect_socket(host, port) {
    Ok(socket) -> {
      io.println("✓ Connected")
      io.println("")
      io.println("Waiting for server to send data (5 seconds)...")
      io.println("(Not sending any data ourselves)")
      io.println("")

      case socket.tcp_recv(socket, 4096, 5000) {
        Ok(data) -> {
          io.println("✓ Received data from server!")
          io.println("  Raw bytes: " <> string.inspect(data))
          io.println("  Length: " <> int.to_string(bit_array.byte_size(data)))
        }
        Error(err) -> {
          io.println("✗ Receive error: " <> string.inspect(err))
          io.println("")
          io.println("This means TWS is not sending any data on its own.")
          io.println("We need to send a greeting message first.")
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
