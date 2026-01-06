import gleam/bit_array
import gleam/int
import gleam/io
import gleam/result
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("=== TWS API Live Connection Test ===")
  io.println("Testing message formats with live TWS connection")
  io.println("")

  let version_str = "176"
  let client_id_str = "1"

  let message_body =
    <<
      0:int-little-size(32),
      version_str:utf8, 0:int-size(8),
      client_id_str:utf8, 0:int-size(8),
    >>

  let body_size = bit_array.byte_size(message_body)

  let message_without_prefix = message_body

  let message_with_prefix =
    <<
      body_size:int-little-size(32),
      message_body:bits,
    >>

  io.println("Format 1 - WITHOUT size prefix:")
  io.println("  Bytes: " <> string.inspect(message_without_prefix))
  io.println("  Size: " <> int.to_string(bit_array.byte_size(message_without_prefix)) <> " bytes")
  io.println("")

  io.println("Format 2 - WITH 4-byte size prefix:")
  io.println("  Bytes: " <> string.inspect(message_with_prefix))
  io.println("  Size: " <> int.to_string(bit_array.byte_size(message_with_prefix)) <> " bytes")
  io.println("")

  io.println("=== Testing with socket connection ===")
  io.println("")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(s) -> {
      io.println("Socket connected successfully!")
      io.println("")

      io.println(">>> Testing Format 1 (WITHOUT size prefix) <<<")
      io.println("Sending message...")
      let send1_result = socket.tcp_send(s, message_without_prefix)
      io.println("Send result: " <> string.inspect(send1_result))
      io.println("Waiting for response (5 second timeout)...")
      
      let response1 = socket.receive_message(s, <<>>, 5000)
      io.println("Response: " <> string.inspect(response1))
      io.println("")

      io.println(">>> Testing Format 2 (WITH size prefix) <<<")
      io.println("Sending message...")
      let send2_result = socket.tcp_send(s, message_with_prefix)
      io.println("Send result: " <> string.inspect(send2_result))
      io.println("Waiting for response (5 second timeout)...")
      
      let response2 = socket.receive_message(s, <<>>, 5000)
      io.println("Response: " <> string.inspect(response2))
      io.println("")

      io.println("=== Summary ===")
      io.println("Format 1 (no prefix): " <> case response1 {
        Ok(_) -> "SUCCESS"
        Error(_) -> "FAILED/NO RESPONSE"
      })
      io.println("Format 2 (with prefix): " <> case response2 {
        Ok(_) -> "SUCCESS"
        Error(_) -> "FAILED/NO RESPONSE"
      })
      io.println("")

      let _ = socket.tcp_close(s)
      io.println("Connection closed")
    }
    Error(err) -> {
      io.println("Connection failed: " <> string.inspect(err))
      io.println("")
      io.println("Make sure TWS is running with:")
      io.println("  - API access enabled")
      io.println("  - Port 7496 (or change the port in the test)")
      io.println("  - 'Enable ActiveX and Socket Clients' checked")
    }
  }
}
