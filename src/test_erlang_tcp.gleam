import gleam/io
import gleam/int
import gleam/string
import gleam/erlang

pub fn main() {
  io.println("=== Testing with Erlang gen_tcp directly ===")
  io.println("")

  let options = [
    { erlang.list_to_atom("active"), False },
    { erlang.list_to_atom("binary"), True },
    { erlang.list_to_atom("send_timeout"), 3000 },
    { erlang.list_to_atom("recv_timeout"), 3000 },
  ]

  case erlang.open_port({ erlang.list_to_atom("gen_tcp"), erlang.list_to_atom("connect"), ["127.0.0.1", 7496, options, 3000] }, []) {
    port if port > 0 -> {
      io.println("Connected via Erlang TCP")
      io.println("")
      io.println("Test 1: Standard format with size prefix")
      let msg1 = <<10:32-little, 0:32-little, "176":utf8, 0:8, "1":utf8, 0:8>>
      io.println("  Sending: " <> binary_to_hex(msg1))
      io.println("")
      io.println("Tests complete!")
    }
    _ -> {
      io.println("Failed to connect")
    }
  }
}

fn binary_to_hex(b: BitArray) -> String {
  do_binary_to_hex(b, "")
}

fn do_binary_to_hex(bit_array: BitArray, acc: String) -> String {
  case bit_array {
    <<byte:8, rest:bits>> -> {
      let hex_byte = int.to_base16(byte)
      let padded_hex = case string.length(hex_byte) {
        1 -> "0" <> hex_byte
        _ -> hex_byte
      }
      do_binary_to_hex(rest, acc <> padded_hex <> " ")
    }
    <<>> -> acc
    _ -> acc
  }
}
