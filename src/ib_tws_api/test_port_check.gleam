import gleam/int
import gleam/io
import gleam/list
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("=== Port Check Test ===")

  let ports = [7496, 7497]
  // Live trading and paper trading ports
  let host = "127.0.0.1"

  io.println("Checking TWS ports...")
  io.println("")

  list.each(ports, fn(port) {
    io.println("Testing port " <> int.to_string(port) <> ":")
    io.println(
      "  Connecting to " <> host <> ":" <> int.to_string(port) <> "...",
    )

    let _ = case socket.connect_socket(host, port) {
      Ok(socket) -> {
        io.println(
          "  ✓ Port " <> int.to_string(port) <> " is accepting connections",
        )
        let _ = socket.close_socket(socket)
        Ok(Nil)
      }
      Error(_err) -> {
        io.println(
          "  ✗ Port " <> int.to_string(port) <> " is NOT accepting connections",
        )
        Ok(Nil)
      }
    }
    io.println("")
  })

  io.println("=== Test Complete ===")
}
