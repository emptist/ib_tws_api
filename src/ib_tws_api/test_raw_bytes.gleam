import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("=== Raw Byte Test ===")
  io.println("")

  // Manually construct the ConnectRequest message
  // Format: Message ID (0, little-endian 32-bit) + "76" + null + "123456" + null
  let message = <<
    0:int-little-size(32),
    "76":utf8,
    0:size(8),
    "123456":utf8,
    0:size(8),
  >>

  io.println("Message bytes:")
  io.println(bit_array.inspect(message))
  io.println("")
  io.println("Expected: <<0, 0, 0, 0, 55, 54, 0, 49, 50, 51, 52, 53, 54, 0>>")
  io.println("")

  io.println("Testing socket connection to 127.0.0.1:7496...")
  io.println("")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(sock) -> {
      io.println("✓ Socket connected successfully!")
      io.println("")

      io.println("Sending message to TWS...")
      let _ = socket.tcp_send(sock, message)
      io.println("✓ Message sent")
      io.println("")

      io.println("Waiting for response (5 seconds)...")
      case socket.tcp_recv(sock, 4096, 5000) {
        Ok(data) -> {
          io.println("✓ Received data from TWS:")
          io.println("  Bytes: " <> int.to_string(bit_array.byte_size(data)))
          io.println("  Content: " <> bit_array.inspect(data))
          io.println("")
          io.println("SUCCESS! TWS responded to the message!")
        }
        Error(err) -> {
          io.println("✗ No data received from TWS")
          io.println("  Error: " <> string.inspect(err))
        }
      }

      io.println("")
      io.println("Closing socket...")
      let _ = socket.tcp_close(sock)
      io.println("✓ Socket closed")
    }
    Error(err) -> {
      io.println("✗ Failed to connect to TWS")
    }
  }

  io.println("")
  io.println("=== Test Complete ===")
}
