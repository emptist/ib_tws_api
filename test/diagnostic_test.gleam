import connection
import gleam/bit_array
import gleam/io

// Diagnostic test to understand IB TWS API protocol
pub fn main() -> Nil {
  io.println("\n=== IB TWS API Diagnostic Test ===")
  io.println("Testing different handshake approaches on single connection")
  io.println("")

  let conn_config = connection.config("127.0.0.1", 7497, 0)

  // Connect once and keep connection open
  io.println("[Setup] Connecting to IB TWS API...")
  let _ = case connection.connect(conn_config) {
    Ok(conn) -> {
      io.println("[Setup] Connected successfully")
      io.println("")

      // Test 1: Send empty string
      io.println("[Test 1] Sending: Empty string")
      let _ = connection.send(conn, "")
      let _ = connection.receive(conn)

      // Test 2: Send "1"
      io.println("")
      io.println("[Test 2] Sending: '1'")
      let _ = connection.send(conn, "1")
      let _ = connection.receive(conn)

      // Test 3: Send "API_V1"
      io.println("")
      io.println("[Test 3] Sending: 'API_V1'")
      let _ = connection.send(conn, "API_V1")
      let _ = connection.receive(conn)

      // Test 4: Send binary data (version 1, client ID 0)
      io.println("")
      io.println("[Test 4] Sending: Binary handshake (version=1, client_id=0)")
      let binary1 = <<0, 1, 0, 0>>
      let str1 = case bit_array.to_string(binary1) {
        Ok(s) -> s
        Error(_) -> ""
      }
      let _ = connection.send(conn, str1)
      let _ = connection.receive(conn)

      // Test 5: Send another binary format with length prefix
      io.println("")
      io.println(
        "[Test 5] Sending: Binary with length prefix (4 bytes length + 2 bytes version + 2 bytes client_id)",
      )
      let binary2 = <<0, 0, 0, 4, 0, 1, 0, 0>>
      let str2 = case bit_array.to_string(binary2) {
        Ok(s) -> s
        Error(_) -> ""
      }
      let _ = connection.send(conn, str2)
      let _ = connection.receive(conn)

      // Test 6: Try just version string with newline
      io.println("")
      io.println("[Test 6] Sending: '1' followed by newline")
      let _ = connection.send(conn, "1\n")
      let _ = connection.receive(conn)

      // Test 7: Try "API" string
      io.println("")
      io.println("[Test 7] Sending: 'API'")
      let _ = connection.send(conn, "API")
      let _ = connection.receive(conn)

      // ! we will not close the connection, let the server decide when to close
      // io.println("")
      // io.println("[Setup] Closing connection...")
      // let _ = connection.close(conn)

      Nil
    }
    Error(err) -> {
      io.println("[Setup] Failed to connect: " <> err_to_string(err))
      Nil
    }
  }

  io.println("")
  io.println("=== Diagnostic Complete ===")
  io.println("Check event logs above for any received data")
  io.println("Check TWS GUI logs for any error messages")

  Nil
}

fn err_to_string(err: connection.ConnectionError) -> String {
  case err {
    connection.ConnectionFailed(msg) -> msg
    connection.InvalidHost -> "Invalid host"
    connection.InvalidPort -> "Invalid port"
    connection.SocketError(msg) -> msg
    connection.Timeout -> "Connection timeout"
  }
}
