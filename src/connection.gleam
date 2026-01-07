/// TCP connection handling for IB TWS API
/// This module manages the socket connection to TWS/Gateway
import gleam/int
import gleam/io
import types

/// Create a connection configuration
pub fn config(host: String, port: Int, client_id: Int) -> types.ConnectionConfig {
  types.ConnectionConfig(host: host, port: port, client_id: client_id)
}

/// Connect to IB TWS API
/// This creates a TCP connection to the specified host and port
/// Currently a placeholder - will implement actual TCP in next phase
pub fn connect(
  config: types.ConnectionConfig,
) -> Result(types.Connection, types.ConnectionError) {
  let types.ConnectionConfig(host: host, port: port, client_id: _) = config

  // Placeholder - will implement actual TCP connection in next phase
  io.println("Connecting to " <> host <> ":" <> int.to_string(port))
  Ok(types.Connection(config: config))
}

/// Send raw data through the connection
/// Placeholder - will be implemented with actual socket
pub fn send(
  conn: types.Connection,
  data: String,
) -> Result(Nil, types.ConnectionError) {
  io.println("Sending data: " <> data)
  Ok(Nil)
}

/// Receive raw data from the connection
/// Placeholder - will be implemented with actual socket
pub fn receive(conn: types.Connection) -> Result(String, types.ConnectionError) {
  // Placeholder - will implement actual receive in next phase
  Ok("placeholder data")
}

/// Close the connection
/// Placeholder - will be implemented with actual socket
pub fn close(conn: types.Connection) -> Result(Nil, types.ConnectionError) {
  io.println("Closing connection")
  Ok(Nil)
}
