import gleam/bit_array
import gleam/int
import gleam/io
import ib_tws_api/protocol
import ib_tws_api/socket

pub fn main() {
  io.println("=== IB TWS Diagnostic Test ===")
  io.println("")

  // Generate random client ID
  let client_id = generate_random_client_id()
  io.println("Generated client ID: " <> int.to_string(client_id))
  io.println("")

  io.println("Testing socket connection to 127.0.0.1:7496...")
  io.println("")

  case socket.connect_socket("127.0.0.1", 7496) {
    Ok(sock) -> {
      io.println("✓ Socket connected successfully!")
      io.println("")
      io.println(
        "This means TWS is running and accepting connections on port 7496.",
      )
      io.println("")

      // Send ConnectRequest message
      io.println(
        "Sending ConnectRequest with client ID: " <> int.to_string(client_id),
      )
      let connect_msg =
        protocol.encode_message(protocol.ConnectRequest(client_id))
      io.println("Message bytes: " <> bit_array.inspect(connect_msg))
      let _ = socket.tcp_send(sock, connect_msg)
      io.println("✓ ConnectRequest sent to TWS")

      io.println("")
      io.println("Waiting for response (10 seconds)...")
      case socket.tcp_recv(sock, 4096, 10_000) {
        Ok(data) -> {
          io.println("✓ Received data from TWS:")
          io.println("  Bytes: " <> int.to_string(bit_array.byte_size(data)))
          io.println("  Content: " <> bit_array.inspect(data))
          io.println("")
          io.println("Analyzing response...")
          analyze_response(data)
        }
        Error(err) -> {
          io.println("✗ No data received from TWS (timeout or error)")
          io.println("")
          io.println("This may indicate:")
          io.println("  - TWS didn't recognize the message format")
          io.println("  - TWS rejected the connection (wrong client ID?)")
          io.println("  - TWS API version mismatch")
          io.println("")
          io.println("Try running this test again with a different client ID.")
        }
      }

      io.println("")
      io.println("Closing socket...")
      let _ = socket.tcp_close(sock)
      io.println("✓ Socket closed")

      io.println("")
      io.println("=== Test Complete ===")
    }
    Error(err) -> {
      io.println("✗ Failed to connect to TWS")
      io.println("")
      io.println("Error: Socket connection failed")
      io.println("")
      io.println("=== TWS Configuration Checklist ===")
      io.println("")
      io.println("Please verify the following in TWS:")
      io.println("")
      io.println("1. TWS is running and logged in")
      io.println("2. Configure API connections:")
      io.println("   - Go to File -> Global Configuration -> API -> Settings")
      io.println("   - Enable 'ActiveX and Socket Clients'")
      io.println("   - Set 'Socket Port' to 7496 (or 7497 for paper trading)")
      io.println("   - Uncheck 'Read-Only API' if you want to send orders")
      io.println("   - Add 127.0.0.1 to 'Trusted IPs' list")
      io.println("")
      io.println("3. Save settings and restart TWS")
      io.println("")
      io.println("After configuring, run this test again.")
    }
  }
}

fn generate_random_client_id() -> Int {
  // Use a simple pseudo-random ID
  // int.random takes an upper bound parameter
  int.random(999_999) + 1
}

fn analyze_response(data: BitArray) {
  io.println("")
  io.println("Response analysis:")
  io.println("  First byte: " <> extract_first_byte(data) |> int.to_string)
  io.println(
    "  First 8 bytes: " <> extract_first_8_bytes(data) |> bit_array.inspect,
  )

  case data {
    <<4:8, _rest:size(3), version:32>> -> {
      io.println("  ✓ Looks like ConnectAck response")
      io.println("  API Version: " <> int.to_string(version))
      io.println("")
      io.println("SUCCESS! TWS accepted the connection!")
    }
    <<5:8, error_code:32>> -> {
      io.println("  ✗ Looks like ConnectFailed response")
      io.println("  Error Code: " <> int.to_string(error_code))
      io.println("")
      io.println("TWS rejected the connection. Try a different client ID.")
    }
    _ -> {
      io.println("  ? Unknown response format")
      io.println("")
      io.println("The response doesn't match expected IB API format.")
    }
  }
}

fn extract_first_byte(data: BitArray) -> Int {
  case data {
    <<byte:8, _rest>> -> byte
    _ -> 0
  }
}

fn extract_first_8_bytes(data: BitArray) -> BitArray {
  case data {
    <<first:64, _rest>> -> <<first:64>>
    _ -> <<>>
  }
}
