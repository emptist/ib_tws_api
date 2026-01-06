import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("Testing simple connection to TWS...")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(socket) -> {
      io.println("Socket connected successfully")
      io.println("Waiting for initial data (5 seconds)...")

      case socket.tcp_recv(socket, 4096, 5000) {
        Ok(data) -> {
          io.println("Received data, length: " <> int.to_string(bit_array.byte_size(data)))
          io.println("Data: " <> string.inspect(data))
        }
        Error(err) -> {
          io.println("Receive error: " <> string.inspect(err))
        }
      }

      let _ = socket.close_socket(socket)
      io.println("Socket closed")
    }
    Error(err) -> {
      io.println("Failed to connect: " <> string.inspect(err))
    }
  }
}
