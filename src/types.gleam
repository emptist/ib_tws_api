/// Core type definitions for IB TWS API
/// This module contains the fundamental types used throughout the library
/// Connection configuration
pub type ConnectionConfig {
  ConnectionConfig(host: String, port: Int, client_id: Int)
}

/// Connection errors
pub type ConnectionError {
  ConnectionFailed(String)
  InvalidHost
  InvalidPort
  SocketError(String)
  Timeout
}

/// Connection state
pub type Connection {
  Connection(config: ConnectionConfig)
}
