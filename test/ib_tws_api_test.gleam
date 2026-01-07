import connection
import gleam/int
import gleam/io
import protocol

// Test IB TWS API connection with handshake
pub fn main() {
  io.println("\n=== IB TWS API Connection Test ===")
  io.println(
    "Testing TCP connection to IB TWS API on port 7497 (paper trading)",
  )
  io.println("")
  io.println("[TEST " <> connection.get_timestamp() <> "] Starting test")

  // Create connection configuration
  let conn_config = connection.config("127.0.0.1", 7497, 1)

  // Connect to IB TWS API
  case connection.connect(conn_config) {
    Ok(conn) -> {
      io.println(
        "[TEST "
        <> connection.get_timestamp()
        <> "] Successfully connected to IB TWS API",
      )
      io.println(
        "[TEST "
        <> connection.get_timestamp()
        <> "] Sending handshake message...",
      )

      // Send START_API handshake message
      // Using V100+ protocol with version range
      let handshake = protocol.start_api_message(100, 200)

      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("[Connection] Handshake sent successfully")
          io.println("[Connection] Waiting for server response...")
          io.println(
            "[Connection] Note: Data arrives asynchronously via event handlers",
          )
          io.println(
            "[Connection] Check the event log above for any received data",
          )

          // Wait for server to respond (data arrives asynchronously)
          // Server should respond with: VERSION<timestamp> EST
          io.println(
            "[TEST "
            <> connection.get_timestamp()
            <> "] Waiting 3 seconds for server response...",
          )
          connection.sleep(3000)

          // Try to receive data
          io.println(
            "[TEST "
            <> connection.get_timestamp()
            <> "] Attempting to receive data...",
          )
          case connection.receive(conn) {
            Ok(data) -> {
              io.println(
                "[TEST "
                <> connection.get_timestamp()
                <> "] Received data from server:",
              )
              io.println(data)

              // Parse the server response
              case protocol.parse_server_response(data) {
                Ok(#(version, timestamp)) -> {
                  io.println("")
                  io.println("=== Test Results ===")
                  io.println("✓ TCP connection established")
                  io.println("✓ Handshake sent successfully")
                  io.println("✓ Server responded with data")
                  io.println("")
                  io.println("Server response parsed:")
                  io.println("- Server version: " <> int.to_string(version))
                  io.println("- Server timestamp: " <> timestamp)
                  io.println("- Handshake successful!")

                  // Send client ID as separate message after server response
                  io.println("")
                  io.println("[Connection] Sending client ID message...")
                  let client_id_msg =
                    protocol.client_id_message(conn_config.client_id)
                  case connection.send_bytes(conn, client_id_msg) {
                    Ok(_) -> {
                      io.println(
                        "[Connection] Client ID sent: "
                        <> int.to_string(conn_config.client_id),
                      )
                      io.println(
                        "[Connection] Check TWS GUI for client ID "
                        <> int.to_string(conn_config.client_id),
                      )
                    }
                    Error(err) -> {
                      let error_msg = case err {
                        connection.SocketError(msg) -> msg
                        connection.ConnectionFailed(msg) -> msg
                        connection.InvalidHost -> "Invalid host"
                        connection.InvalidPort -> "Invalid port"
                        connection.Timeout -> "Connection timeout"
                      }
                      io.println(
                        "[Connection] Failed to send client ID: " <> error_msg,
                      )
                    }
                  }
                }
                Error(err) -> {
                  io.println("")
                  io.println("=== Test Results ===")
                  io.println("✓ TCP connection established")
                  io.println("✓ Handshake sent successfully")
                  io.println("✓ Server responded with data")
                  io.println("")
                  io.println("Server response parsing failed:")
                  io.println("- Error: " <> err)
                }
              }
            }
            Error(_) -> {
              io.println("[Connection] No data received yet")
              io.println("")
              io.println("=== Test Results ===")
              io.println("✓ TCP connection established")
              io.println("✓ Handshake sent successfully")
              io.println("✗ Server did not respond")
              io.println("")
              io.println(
                "Note: Check TWS GUI to see if connection was accepted",
              )
              io.println(
                "Note: Server response may have arrived in event log above",
              )
            }
          }

          // Keep connection open to allow TWS to register client ID
          io.println("")
          io.println("[Connection] Keeping connection open...")
          io.println("[Connection] Press Ctrl+C to exit")
          io.println("")
          // Keep connection open - do not close
          // This allows TWS to register the client ID
        }
        Error(err) -> {
          let error_msg = case err {
            connection.ConnectionFailed(msg) -> msg
            connection.InvalidHost -> "Invalid host"
            connection.InvalidPort -> "Invalid port"
            connection.SocketError(msg) -> msg
            connection.Timeout -> "Connection timeout"
          }
          io.println("[Connection] Failed to send handshake: " <> error_msg)
          let _ = connection.destroy(conn)
          io.println("")
          io.println("=== Test Failed ===")
          io.println("✗ Could not send handshake message")
        }
      }
    }
    Error(err) -> {
      let error_msg = case err {
        connection.ConnectionFailed(msg) -> msg
        connection.InvalidHost -> "Invalid host"
        connection.InvalidPort -> "Invalid port"
        connection.SocketError(msg) -> msg
        connection.Timeout -> "Connection timeout"
      }
      io.println("[Connection] Failed to connect: " <> error_msg)
      io.println("")
      io.println("=== Test Failed ===")
      io.println("✗ Could not establish TCP connection")
      io.println("")
      io.println("Troubleshooting:")
      io.println("1. Ensure TWS is running")
      io.println(
        "2. Enable API connections in TWS (Configure > API > Settings)",
      )
      io.println("3. Check that port 7497 is not blocked")
      io.println("4. Verify 'Allow connections from localhost' is checked")
    }
  }
}
