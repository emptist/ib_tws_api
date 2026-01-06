import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("=== TWS API Connection Test ===")
  io.println("")

  let client_id = 1
  let server_version = 76

  io.println("Handshake parameters:")
  io.println("  Server version: " <> int.to_string(server_version))
  io.println("  Client ID: " <> int.to_string(client_id))
  io.println("")
  io.println("Note: Client ID should be unique per connection")
  io.println("Recommended: Use timestamp-based ID in range 1-999999")
  io.println("")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(s) -> {
      io.println("Socket connected: " <> string.inspect(s))
      io.println("")
      io.println("Waiting 2 seconds to see if TWS sends anything...")
      io.println("")

      case socket.tcp_recv(s, 4096, 2000) {
        Ok(response) -> {
          io.println("Received UNSOLICITED data from TWS!")
          io.println(
            "  " <> int.to_string(bit_array.byte_size(response)) <> " bytes",
          )
          io.println("  Hex dump: " <> bytes_to_hex(response))
          io.println("")
        }
        Error(e) -> {
          io.println("No unsolicited data: " <> string.inspect(e))
          io.println("")
        }
      }

      let message_data = <<
        server_version:int-big-size(32),
        client_id:int-big-size(32),
      >>
      let message_length = bit_array.byte_size(message_data)

      io.println(
        "Message data (" <> int.to_string(message_length) <> " bytes):",
      )
      io.println("  Hex dump: " <> bytes_to_hex(message_data))
      io.println("")

      io.println("=== Test 1: WITHOUT length prefix (old IB API format) ===")
      io.println("Sending handshake (8 bytes)...")
      let send1 = socket.tcp_send(s, message_data)
      io.println("  Send result: " <> string.inspect(send1))
      io.println("")

      io.println("Waiting 5 seconds for response...")
      case socket.tcp_recv(s, 4096, 5000) {
        Ok(response) -> {
          io.println(
            "SUCCESS! Received "
            <> int.to_string(bit_array.byte_size(response))
            <> " bytes",
          )
          io.println("  Hex dump: " <> bytes_to_hex(response))
          io.println("")
          io.println("Response decoded:")
          io.println("  " <> string.inspect(response))
        }
        Error(e) -> {
          io.println("No response: " <> string.inspect(e))
          io.println("")

          io.println("=== Test 2: WITH length prefix (new API format) ===")
          let full_message = <<
            message_length:int-big-size(32),
            message_data:bits,
          >>

          io.println(
            "Full message ("
            <> int.to_string(bit_array.byte_size(full_message))
            <> " bytes):",
          )
          io.println("  Hex dump: " <> bytes_to_hex(full_message))
          io.println("")

          io.println("Sending handshake (12 bytes)...")
          let send2 = socket.tcp_send(s, full_message)
          io.println("  Send result: " <> string.inspect(send2))
          io.println("")

          io.println("Waiting 5 seconds for response...")
          case socket.tcp_recv(s, 4096, 5000) {
            Ok(response2) -> {
              io.println(
                "Received "
                <> int.to_string(bit_array.byte_size(response2))
                <> " bytes",
              )
              io.println("  Hex dump: " <> bytes_to_hex(response2))
            }
            Error(e2) -> {
              io.println("No response: " <> string.inspect(e2))
            }
          }
        }
      }

      let _ = socket.tcp_close(s)
      io.println("Socket closed")
    }
    Error(e) -> {
      io.println("Connection failed: " <> string.inspect(e))
      io.println("")
      io.println("Ensure TWS is running with:")
      io.println("  - API access enabled on port 7496 or 7497")
      io.println("  - 'Enable ActiveX and Socket Clients' enabled")
    }
  }
}

fn bytes_to_hex(data: BitArray) -> String {
  bytes_to_hex_loop(data, "")
}

fn bytes_to_hex_loop(data: BitArray, acc: String) -> String {
  case data {
    <<byte:size(8), rest:bits>> -> {
      let byte_str = byte_to_hex(byte)
      bytes_to_hex_loop(rest, acc <> byte_str <> " ")
    }
    _ -> acc
  }
}

fn byte_to_hex(byte: Int) -> String {
  let high = byte / 16
  let low = byte % 16
  hex_digit(high) <> hex_digit(low)
}

fn hex_digit(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    15 -> "f"
    _ -> "?"
  }
}
