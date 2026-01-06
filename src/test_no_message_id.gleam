import gleam/bit_array
import gleam/io
import gleam/int
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("Testing ConnectRequest without message ID...")
  io.println("")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(s) -> {
      io.println("Connected successfully!")
      io.println("")

      let version_str = "176"
      let client_id_str = "1"

      let data = <<
        version_str:utf8, 0:size(8),
        client_id_str:utf8, 0:size(8),
      >>

      io.println("Sending data without message ID:")
      io.println("  Version: " <> version_str)
      io.println("  Client ID: " <> client_id_str)
      io.println("  Data length: " <> int.to_string(bit_array.byte_size(data)))
      io.println("  Data bytes: " <> string.inspect(data))
      io.println("")

      let send_result = socket.tcp_send(s, data)
      io.println("Send result: " <> string.inspect(send_result))
      io.println("")

      io.println("Attempting to receive response with timeout 10000ms...")
  case socket.receive_message(s, <<>>, 10000) {
        Ok(#(msg, buffer)) -> {
          io.println("SUCCESS! Received message:")
          io.println("  " <> string.inspect(msg))
          io.println("  Buffer size: " <> int.to_string(bit_array.byte_size(buffer)))
        }
        Error(err) -> {
          io.println("Receive error:")
          io.println("  " <> string.inspect(err))
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
