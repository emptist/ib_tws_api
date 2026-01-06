import gleam/bit_array
import gleam/io
import gleam/int
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("Testing TWS connection without sending any message...")
  io.println("")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(s) -> {
      io.println("Connected successfully!")
      io.println("")

      io.println("Attempting to receive data with timeout 10000ms...")
      case socket.receive_message(s, <<>>, 10000) {
        Ok(#(msg, buffer)) -> {
          io.println("Received message:")
          io.println(string.inspect(msg))
          io.println("Buffer size: " <> int.to_string(bit_array.byte_size(buffer)))
        }
        Error(err) -> {
          io.println("Receive error:")
          io.println(string.inspect(err))
        }
      }

      io.println("")
      io.println("Closing socket...")
      let _ = socket.tcp_close(s)
      io.println("Done")
    }
    Error(err) -> {
      io.println("Connection error:")
      io.println(string.inspect(err))
    }
  }
}