import connection
import gleam/io
import types

/// Main IB TWS API module
/// This provides the public API for connecting to and interacting with IB TWS
/// Connect to IB TWS API using paper trading (port 7497)
/// This is the recommended way for development and testing
pub fn connect_paper(
  host: String,
  client_id: Int,
) -> Result(types.Connection, types.ConnectionError) {
  let config = connection.config(host, 7497, client_id)
  case connection.connect(config) {
    Ok(conn) -> {
      io.println("Successfully connected to IB TWS Paper Trading!")
      Ok(conn)
    }
    Error(err) -> {
      io.println("Failed to connect: " <> error_to_string(err))
      Error(err)
    }
  }
}

/// Connect to IB TWS API using live trading (port 7496)
/// WARNING: This uses real money - use with extreme caution
pub fn connect_live(
  host: String,
  client_id: Int,
) -> Result(types.Connection, types.ConnectionError) {
  let config = connection.config(host, 7496, client_id)
  case connection.connect(config) {
    Ok(conn) -> {
      io.println("Successfully connected to IB TWS Live Trading!")
      Ok(conn)
    }
    Error(err) -> {
      io.println("Failed to connect: " <> error_to_string(err))
      Error(err)
    }
  }
}

/// Send raw data through the connection
pub fn send(
  conn: types.Connection,
  data: String,
) -> Result(Nil, types.ConnectionError) {
  connection.send(conn, data)
}

/// Receive raw data from the connection
pub fn receive(conn: types.Connection) -> Result(String, types.ConnectionError) {
  connection.receive(conn)
}

/// Close the connection
pub fn close(conn: types.Connection) -> Result(Nil, types.ConnectionError) {
  connection.close(conn)
}

/// Convert error to string for display
fn error_to_string(err: types.ConnectionError) -> String {
  case err {
    types.ConnectionFailed(msg) -> "Connection failed: " <> msg
    types.InvalidHost -> "Invalid host"
    types.InvalidPort -> "Invalid port"
    types.SocketError(msg) -> "Socket error: " <> msg
    types.Timeout -> "Connection timeout"
    types.ProtocolError(msg) -> "Protocol error: " <> msg
  }
}
