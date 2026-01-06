import gleam/bit_array
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api/socket

pub fn main() {
  io.println("=== TWS Initial Data Test ===")
  io.println("Testing if TWS sends any data upon connection")
  io.println("")
  io.println("This test connects and waits WITHOUT sending anything")
  io.println("to see if TWS initiates communication.")
  io.println("")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(s) -> {
      io.println("Socket connected successfully!")
      io.println("")
      io.println("Waiting 3 seconds for any initial data from TWS...")
      io.println("(Not sending any message - just listening)")
      io.println("")

      let response = socket.receive_message(s, <<>>, 3000)
      
      io.println("Response: " <> string.inspect(response))
      
      case response {
        Ok(#(msg, buffer)) -> {
          io.println("")
          io.println("!!! TWS SENT DATA !!!")
          io.println("Message: " <> string.inspect(msg))
          io.println("Buffer size: " <> int.to_string(bit_array.byte_size(buffer)))
        }
        Error(err) -> {
          io.println("")
          io.println("No data received within 3 seconds")
          io.println("Error: " <> string.inspect(err))
          io.println("")
          io.println("This suggests TWS does NOT send initial data.")
          io.println("It waits for the client to initiate the conversation.")
        }
      }

      io.println("")
      io.println("Closing socket...")
      let _ = socket.tcp_close(s)
      io.println("Done")
    }
    Error(err) -> {
      io.println("Connection failed: " <> string.inspect(err))
      io.println("")
      io.println("Make sure TWS is running with:")
      io.println("  - API access enabled")
      io.println("  - Port 7496")
      io.println("  - 'Enable ActiveX and Socket Clients' checked")
    }
  }
}
