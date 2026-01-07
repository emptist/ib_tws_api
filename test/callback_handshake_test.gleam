import connection
import gleam/bool
import gleam/int
import gleam/io
import gleam/option.{Some}
import protocol

// Test IB TWS API connection with callback-based message handling
// This properly handles the async timing issue by using callbacks
pub fn main() {
  io.println("\n=== IB TWS API Callback-Based Handshake Test ===")

  // Use auto port switcher with LiveTradingReadOnly for safety
  let account_type = connection.LiveTradingReadOnly
  let conn_config =
    connection.config_with_account_type("127.0.0.1", account_type, 1)

  let port_name = case account_type {
    connection.PaperTrading -> "PAPER TRADING (7497)"
    connection.LiveTradingReadOnly -> "LIVE TRADING READ-ONLY (7496)"
    connection.LiveTrading -> "LIVE TRADING (7496)"
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
        io.println("[CALLBACK] Sending client ID message...")

        // Note: We can't send client ID from here because we don't have
        // access to the connection object. This is a limitation of the current
        // callback design. In a real implementation, we'd need to pass the
        // connection to the callback or use a different pattern.
        io.println(
          "[CALLBACK] Client ID should be sent after this callback completes",
        )
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

          // Wait a bit to let the callback execute
          io.println("[TEST] Waiting 2 seconds for callback to execute...")
          connection.sleep(2000)

          // Now send the client ID message
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
