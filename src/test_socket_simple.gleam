import gleam/io
import gleam/string
import gleam/int
import ib_tws_api/socket

pub fn main() {
  io.println("Testing simple socket connection...")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(s) -> {
      io.println("Socket connected successfully")
      io.println("Socket: " <> string.inspect(s))

      let client_id = 1
      let _client_id = client_id

      io.println("Waiting 2000ms for initial data from TWS...")
      case socket.tcp_recv(s, 4096, 2000) {
        Ok(response) -> {
          io.println("Received initial data: " <> string.inspect(response))
          io.println("Response bytes: " <> bit_array_to_hex(response))
        }
        Error(e) -> {
          io.println("No initial data: " <> string.inspect(e))
          io.println("Sending handshake...")
          let message_data = <<
            76:int-big-size(32),
            client_id:int-big-size(32),
          >>
          let _ = socket.tcp_send(s, <<8: int-big-size(32), message_data:bits>>)
          io.println("Handshake sent, waiting for response...")
          case socket.tcp_recv(s, 4096, 10000) {
            Ok(response) -> {
              io.println("Received: " <> string.inspect(response))
              io.println("Response bytes: " <> bit_array_to_hex(response))
            }
            Error(e2) -> {
              io.println("Receive error: " <> string.inspect(e2))
            }
          }
        }
      }

      io.println("Waiting 10000ms for response...")
      case socket.tcp_recv(s, 4096, 10000) {
        Ok(response) -> {
          io.println("Received: " <> string.inspect(response))
          io.println("Response bytes: " <> bit_array_to_hex(response))
        }
        Error(e) -> {
          io.println("Receive error: " <> string.inspect(e))
        }
      }

      let _ = socket.tcp_close(s)
      io.println("Socket closed")
    }
    Error(e) -> {
      io.println("Connection failed: " <> string.inspect(e))
    }
  }
}

fn bit_array_to_hex(data: BitArray) -> String {
  bit_array_to_hex_loop(data, "")
}

fn bit_array_to_hex_loop(data: BitArray, acc: String) -> String {
  case data {
    <<byte:size(8), rest:bits>> -> {
      let byte_str = case byte < 16 {
        True -> "0" <> int.to_string(byte)
        False -> int.to_string(byte)
      }
      bit_array_to_hex_loop(rest, acc <> byte_str <> " ")
    }
    _ -> acc
  }
}
