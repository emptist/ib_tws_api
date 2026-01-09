import connection.{
  type AccountType, type ConnectionError, ConnectionConfig, close, connect,
}
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/result
import protocol.{start_api_message_with_length}

pub fn test_handshake_versions() {
  // Test with standard version (100)
  test_handshake_version(100, 200)

  // Test with minimum supported version (1)
  test_handshake_version(1, 200)

  // Test with maximum possible version (999)
  test_handshake_version(999, 200)
}

fn test_handshake_version(version: Int, client_id: Int) {
  let config =
    ConnectionConfig(
      host: "localhost",
      port: 7497,
      // Paper trading port
      client_id: client_id,
      account_type: connection.PaperTrading,
    )

  case connect(config) {
    Ok(conn) -> {
      io.println(
        "Successfully connected with version " <> int.to_string(version),
      )
      // Close connection
      case close(conn) {
        Ok(_) -> io.println("Connection closed successfully")
        Error(e) ->
          io.println(
            "Failed to close connection: " <> connection_error_to_string(e),
          )
      }
    }
    Error(e) -> {
      io.println(
        "Failed to connect with version "
        <> int.to_string(version)
        <> ": "
        <> connection_error_to_string(e),
      )
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
