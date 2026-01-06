import gleam/io
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("=== Testing with nc-like raw bytes ===")
  io.println("")

  let connection_result = socket.connect_socket("127.0.0.1", 7496)

  case connection_result {
    Ok(s) -> {
      io.println("Connected")
      io.println("")

      io.println("Test 1: Send <size=8><APIv><null><version><null><clientId>")
      let data1 = <<8:int-little-size(32), "APIv", 0:int-size(8), 176:int-little-size(32), 1:int-little-size(32)>>
      io.println("  Raw: " <> string.inspect(data1))
      let _ = socket.tcp_send(s, data1)
      let _ = socket.receive_message(s, <<>>, 3000)
      io.println("")

      io.println("Test 2: Send just the APIv prefix (older style)")
      let data2 = <<"APIv", 0:int-size(8)>>
      io.println("  Raw: " <> string.inspect(data2))
      let _ = socket.tcp_send(s, data2)
      let _ = socket.receive_message(s, <<>>, 3000)
      io.println("")

      io.println("Test 3: Send size=4<version=176><clientId=1>")
      let data3 = <<4:int-little-size(32), 176:int-little-size(32), 1:int-little-size(32)>>
      io.println("  Raw: " <> string.inspect(data3))
      let _ = socket.tcp_send(s, data3)
      let _ = socket.receive_message(s, <<>>, 3000)
      io.println("")

      io.println("Test 4: Send binary <0x00, 0xb0, 0x00, 0x00> (176 little endian) + clientId")
      let data4 = <<0x00, 0xb0, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00>>
      io.println("  Raw: " <> string.inspect(data4))
      let _ = socket.tcp_send(s, data4)
      let _ = socket.receive_message(s, <<>>, 3000)
      io.println("")

      let _ = socket.tcp_close(s)
      io.println("All tests complete")
    }
    Error(_) -> {
      io.println("Failed to connect to TWS")
    }
  }
}
