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
  ProtocolError(String)
}

/// Connection state
pub type Connection {
  Connection(config: ConnectionConfig)
}

/// Message types from IB TWS
pub type MessageType {
  ServerTime
  Error
  MarketData
  OrderStatus
  // More message types will be added as needed
}

/// Protocol message
pub type Message {
  Message(message_type: MessageType, payload: String)
}
