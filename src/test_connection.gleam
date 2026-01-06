import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api
import ib_tws_api/protocol

pub fn main() {
  io.println("Starting TWS connection test...")
  io.println("")

  let client = ib_tws_api.new_client("127.0.0.1", 7496, 1)

  io.println("Client configuration:")
  io.println("  Host: " <> client.host)
  io.println("  Port: " <> int.to_string(client.port))
  io.println("  Client ID: " <> int.to_string(client.client_id))
  io.println("")

  io.println("Attempting to connect to TWS...")
  io.println("")

  case ib_tws_api.connect(client) {
    Ok(connected_client) -> {
      io.println("")
      io.println("Successfully connected to TWS!")
      io.println("Client ID: " <> int.to_string(connected_client.client_id))
      io.println("")

      let _ = ib_tws_api.disconnect(connected_client)
      io.println("Disconnected from TWS")
    }
    Error(err) -> {
      io.println("")
      io.println("Failed to connect to TWS:")
      io.println(string.inspect(err))
      io.println("")
      
      io.println("Testing ConnectRequest encoding...")
      let connect_req = protocol.ConnectRequest(1)
      let encoded = protocol.encode_message(connect_req)
      io.println("Encoded ConnectRequest:")
      io.println("  Length: " <> int.to_string(bit_array.byte_size(encoded)))
      io.println("  Bytes: " <> string.inspect(encoded))
      io.println("")
      
      io.println("Expected format for ConnectRequest:")
      io.println("  Message ID (0): 4 bytes (little-endian)")
      io.println("  Version string: variable length, null-terminated")
      io.println("  Client ID string: variable length, null-terminated")
      io.println("")
      
      io.println("Protocol version: " <> int.to_string(protocol.protocol_version))
    }
  }
}
