/// Connection type - opaque to hide implementation details
pub opaque type Connection {
  Connection(socket: Nil, host: String, port: Int)
}

/// Connection error types
pub type ConnectionError {
  ConnectionFailed(String)
  InvalidHost
  InvalidPort
  SocketError(String)
  Timeout
}

/// Create a connection configuration
pub fn config(host: String, port: Int, client_id: Int) -> ConnectionConfig {
  ConnectionConfig(host: host, port: port, client_id: client_id)
}

/// Connection configuration
pub type ConnectionConfig {
  ConnectionConfig(host: String, port: Int, client_id: Int)
}

/// Connect to IB TWS API using TCP socket
/// Returns a connection handle on success, or an error on failure
@external(javascript, "./connection_ffi.mjs", "connect")
pub fn connect(config: ConnectionConfig) -> Result(Connection, ConnectionError)

/// Send raw data through the connection
@external(javascript, "./connection_ffi.mjs", "send")
pub fn send(conn: Connection, data: String) -> Result(Nil, ConnectionError)

/// Receive raw data from the connection
@external(javascript, "./connection_ffi.mjs", "receive")
pub fn receive(conn: Connection) -> Result(String, ConnectionError)

/// Close the connection
@external(javascript, "./connection_ffi.mjs", "close")
pub fn close(conn: Connection) -> Result(Nil, ConnectionError)
