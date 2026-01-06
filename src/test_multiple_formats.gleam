import gleam/bit_array
import gleam/io
import gleam/int
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("Testing multiple ConnectRequest formats...")
  io.println("")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(s) -> {
      io.println("Connected successfully!")
      io.println("")

      io.println("Testing Format 1: Message ID (0) + Version (int32) + Client ID (int32)")
      let format1 = <<
        0:int-little-size(32),
        176:int-little-size(32),
        1:int-little-size(32),
      >>
      test_format(s, format1, "Format 1")

      io.println("")
      io.println("Testing Format 2: Message ID (0) + Version (null-terminated string) + Client ID (null-terminated string)")
      let format2 = <<
        0:int-little-size(32),
        "176":utf8, 0:int-size(8),
        "1":utf8, 0:int-size(8),
      >>
      test_format(s, format2, "Format 2")

      io.println("")
      io.println("Testing Format 3: Version (int32) + Client ID (int32) [no message ID]")
      let format3 = <<
        176:int-little-size(32),
        1:int-little-size(32),
      >>
      test_format(s, format3, "Format 3")

      io.println("")
      io.println("Testing Format 4: Version (null-terminated string) + Client ID (null-terminated string) [no message ID]")
      let format4 = <<
        "176":utf8, 0:int-size(8),
        "1":utf8, 0:int-size(8),
      >>
      test_format(s, format4, "Format 4")

      io.println("")
      io.println("Testing Format 5: Message ID (0) + Version (int32) + Client ID (null-terminated string)")
      let format5 = <<
        0:int-little-size(32),
        176:int-little-size(32),
        "1":utf8, 0:int-size(8),
      >>
      test_format(s, format5, "Format 5")

      io.println("")
      io.println("Testing Format 6: Message ID (0) + Version (null-terminated string) + Client ID (int32)")
      let format6 = <<
        0:int-little-size(32),
        "176":utf8, 0:int-size(8),
        1:int-little-size(32),
      >>
      test_format(s, format6, "Format 6")

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

fn test_format(socket: socket.Socket, data: BitArray, format_name: String) {
  io.println(format_name)
  io.println("  Data length: " <> int.to_string(bit_array.byte_size(data)))
  io.println("  Data bytes: " <> string.inspect(data))
  io.println("")

  let send_result = socket.tcp_send(socket, data)
  io.println("  Send result: " <> string.inspect(send_result))
  io.println("")

  io.println("  Attempting to receive response with timeout 10000ms...")
  case socket.receive_message(socket, <<>>, 10000) {
    Ok(#(msg, buffer)) -> {
      io.println("  SUCCESS! Received message:")
      io.println("    " <> string.inspect(msg))
      io.println("    Buffer size: " <> int.to_string(bit_array.byte_size(buffer)))
    }
    Error(err) -> {
      io.println("  Receive error:")
      io.println("    " <> string.inspect(err))
    }
  }
}
