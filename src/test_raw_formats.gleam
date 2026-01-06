import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("=== Testing Raw Socket Approaches ===")
  io.println("")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(s) -> {
      io.println("Connected to TWS")
      io.println("")

      let version_str = "176"
      let client_id_str = "1"

      io.println("Test 1: Raw strings without size prefix")
      let raw1 = <<
        version_str:utf8,
        0:int-size(8),
        client_id_str:utf8,
        0:int-size(8),
      >>
      io.println("  Data: " <> string.inspect(raw1))
      io.println("  Length: " <> int.to_string(bit_array.byte_size(raw1)))
      let _ = socket.tcp_send(s, raw1)
      let _ = socket.receive_message(s, <<>>, 3000)
      io.println("")

      io.println(
        "Test 2: Message ID (0) + version + clientId WITHOUT size prefix",
      )
      let raw2 = <<
        0:int-little-size(32),
        version_str:utf8,
        0:int-size(8),
        client_id_str:utf8,
        0:int-size(8),
      >>
      io.println("  Data: " <> string.inspect(raw2))
      io.println("  Length: " <> int.to_string(bit_array.byte_size(raw2)))
      let _ = socket.tcp_send(s, raw2)
      let _ = socket.receive_message(s, <<>>, 3000)
      io.println("")

      io.println("Test 3: With size prefix (10 bytes body)")
      let body = <<
        0:int-little-size(32),
        version_str:utf8,
        0:int-size(8),
        client_id_str:utf8,
        0:int-size(8),
      >>
      let raw3 = <<10:int-little-size(32), body:bits>>
      io.println("  Data: " <> string.inspect(raw3))
      io.println("  Length: " <> int.to_string(bit_array.byte_size(raw3)))
      let _ = socket.tcp_send(s, raw3)
      let _ = socket.receive_message(s, <<>>, 3000)
      io.println("")

      io.println("Test 4: Just send <version><null><clientId> (no message ID)")
      let raw4 = <<
        version_str:utf8,
        0:int-size(8),
        client_id_str:utf8,
        0:int-size(8),
      >>
      io.println("  Data: " <> string.inspect(raw4))
      io.println("  Length: " <> int.to_string(bit_array.byte_size(raw4)))
      let _ = socket.tcp_send(s, raw4)
      let _ = socket.receive_message(s, <<>>, 3000)
      io.println("")

      io.println("Test 5: Try with integer version (4 bytes) instead of string")
      let raw5 = <<
        0:int-little-size(32),
        176:int-little-size(32),
        1:int-little-size(32),
      >>
      io.println("  Data: " <> string.inspect(raw5))
      io.println("  Length: " <> int.to_string(bit_array.byte_size(raw5)))
      let _ = socket.tcp_send(s, raw5)
      let _ = socket.receive_message(s, <<>>, 3000)
      io.println("")

      io.println(
        "Test 6: ConnectRequest as per older API (just version and clientId as strings)",
      )
      let raw6 = <<
        "APIv",
        version_str:utf8,
        0:int-size(8),
        client_id_str:utf8,
        0:int-size(8),
      >>
      io.println("  Data: " <> string.inspect(raw6))
      io.println("  Length: " <> int.to_string(bit_array.byte_size(raw6)))
      let _ = socket.tcp_send(s, raw6)
      let _ = socket.receive_message(s, <<>>, 3000)
      io.println("")

      let _ = socket.tcp_close(s)
      io.println("Tests complete")
    }
    Error(err) -> {
      io.println("Connection failed: " <> string.inspect(err))
    }
  }
}
