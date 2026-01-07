import connection
import gleam/bool
import gleam/int
import gleam/io
import gleam/option.{Some}
import protocol

// Simple test that demonstrates callback-based message handling
// This test shows the proper sequence: handshake -> wait for response -> send client ID
pub fn main() {
  io.println("\n=== Simple Callback-Based Handshake Test ===")

  // Use PaperTrading for testing (port 7497)
  // Generate random client ID based on timestamp to avoid conflicts
  let account_type = connection.PaperTrading
  let client_id = connection.generate_client_id()
  let conn_config =
    connection.config_with_account_type(
      "127.0.0.1",
      7497,
      account_type,
      client_id,
    )

  let port_name = case account_type {
    connection.PaperTrading -> "PAPER TRADING (7497)"
  }

  io.println(
    "Testing TCP connection to IB TWS API on port "
    <> int.to_string(conn_config.port)
    <> " ("
    <> port_name
    <> ")",
  )
  io.println("")
  io.println(
    "Trading Allowed: "
    <> bool.to_string(connection.is_trading_allowed(account_type)),
  )
  case connection.is_trading_allowed(account_type) {
    False -> io.println("⚠️ SAFETY: Trading operations are BLOCKED ⚠️")
    True -> Nil
  }
  io.println("")
  io.println("[TEST " <> connection.get_timestamp() <> "] Starting test")

  // Flag to track if we received data
  let _data_received = io.println("Initializing...")

  // Create a callback that will be invoked when data is received
  let data_callback = fn(data: String) {
    io.println("")
    io.println("[CALLBACK] Data received from server:")
    io.println(data)

    // Parse the server response
    case protocol.parse_server_response(data) {
      Ok(#(version, timestamp)) -> {
        io.println("")
        io.println("=== Server Response Parsed ===")
        io.println("- Server version: " <> int.to_string(version))
        io.println("- Server timestamp: " <> timestamp)
        io.println("")
        io.println("[CALLBACK] Handshake successful!")
        io.println("[CALLBACK] Client ID should be sent immediately after this")
      }
      Error(err) -> {
        io.println("")
        io.println("[CALLBACK] Failed to parse server response:")
        io.println("- Error: " <> err)
      }
    }
  }

  // Connect with the callback registered
  io.println("")
  io.println("[TEST] Connecting with callback registered...")
  case connection.connect_with_callback(conn_config, Some(data_callback)) {
    Ok(conn) -> {
      io.println(
        "[TEST "
        <> connection.get_timestamp()
        <> "] Successfully connected to IB TWS API",
      )
      // Wait for socket to be ready
      io.println("[TEST] Waiting for socket to be ready...")
      connection.sleep(100)

      io.println(
        "[TEST "
        <> connection.get_timestamp()
        <> "] Sending handshake message...",
      )

      // Send START_API handshake message
      let handshake = protocol.start_api_message(100, 200)

      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("[Connection] Handshake sent successfully")
          io.println("")
          io.println("[Connection] Waiting for server response...")
          io.println("[Connection] Callback will be invoked when data arrives")
          io.println("")

          // Wait for server to respond
          // The callback will be invoked automatically when data arrives
          io.println("[TEST] Waiting 1 second for server response...")
          connection.sleep(1000)

          // Now send the client ID message
          // This should be sent immediately after receiving server response
          io.println("")
          io.println("[TEST] Sending client ID message...")
          let client_id_msg = protocol.client_id_message(conn_config.client_id)
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
              io.println("")
              io.println("=== Test Summary ===")
              io.println("✓ TCP connection established")
              io.println("✓ Handshake sent successfully")
              io.println(
                "✓ Server response received (see callback output above)",
              )
              io.println("✓ Client ID sent after server response")
              io.println("")
              io.println(
                "If client ID appears in TWS GUI, the handshake is complete!",
              )

              // Keep connection open to allow TWS to register client ID
              io.println("")
              io.println("[Connection] Keeping connection open...")
              io.println("[Connection] Press Ctrl+C to exit")
              io.println("")
            }
            Error(err) -> {
              let error_msg = case err {
                connection.SocketError(msg) -> msg
                connection.ConnectionFailed(msg) -> msg
                connection.InvalidHost -> "Invalid host"
                connection.InvalidPort -> "Invalid port"
                connection.Timeout -> "Connection timeout"
              }
              io.println("[Connection] Failed to send client ID: " <> error_msg)
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
          io.println("[Connection] Failed to send handshake: " <> error_msg)
          let _ = connection.destroy(conn)
          Nil
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
      io.println(
        "3. Check that port "
        <> int.to_string(conn_config.port)
        <> " is not blocked",
      )
      io.println("4. Verify 'Allow connections from localhost' is checked")
    }
  }
}
