import gleam/io
import ib_tws_api/socket

pub fn main() {
  io.println("Simple connection test to 127.0.0.1:7496")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(socket) -> {
      io.println("✓ Connection successful!")
      case socket.close_socket(socket) {
        Ok(_) -> io.println("✓ Socket closed")
        Error(err) -> {
          case err {
            socket.SocketError(msg) -> {
              io.println("✗ Error closing socket: " <> msg)
            }
          }
        }
      }
    }
    Error(err) -> {
      case err {
        socket.SocketError(msg) -> {
          io.println("✗ Connection failed: " <> msg)
        }
      }
    }
  }
}
