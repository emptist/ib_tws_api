import connection.{
  type AccountType, type ConnectionError, ConnectionConfig, close, connect,
}
import gleam/int
import gleam/io

pub fn test_tws_config() {
  // Test with paper trading account
  test_config(7497, connection.PaperTrading)
  // Test with live trading account (read-only)
  test_config(7496, connection.LiveTradingReadOnly)
}

fn test_config(port: Int, account_type: AccountType) {
  let config =
    ConnectionConfig(
      host: "localhost",
      port: port,
      client_id: 1,
      account_type: account_type,
    )

  case connect(config) {
    Ok(conn) -> {
      io.println(
        "✅ Successfully connected to TWS on port " <> int.to_string(port),
      )
      case close(conn) {
        Ok(_) -> io.println("✅ Connection closed cleanly")
        Error(e) ->
          io.println(
            "⚠️ Failed to close connection: " <> connection_error_to_string(e),
          )
      }
    }
    Error(e) -> {
      io.println(
        "❌ Failed to connect to TWS on port "
        <> int.to_string(port)
        <> ": "
        <> connection_error_to_string(e),
      )
      io.println("\n⚠️ TWS Configuration Check Failed!")
      io.println("Please verify these settings in TWS:")
      io.println("1. Enable API connections in TWS/IB Gateway")
      io.println("2. Set 'Socket Port' to " <> int.to_string(port))
      io.println("3. Allow connections from 'localhost' (127.0.0.1)")
      io.println("4. Enable 'Read-Only API' if testing with live account")
    }
  }
}

fn connection_error_to_string(error: ConnectionError) -> String {
  case error {
    connection.ConnectionFailed(msg) -> "Connection failed: " <> msg
    connection.InvalidHost -> "Invalid host"
    connection.InvalidPort -> "Invalid port"
    connection.SocketError(msg) -> "Socket error: " <> msg
    connection.Timeout -> "Timeout"
  }
}
