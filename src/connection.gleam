import gleam/io
import gleam/option.{None, Some}
import node_socket_client.{CloseEvent, DataEvent, ErrorEvent, ReadyEvent}

/// Connection state for event handling
pub type ConnectionState {
  ConnectionState(
    connected: Bool,
    received_data: List(String),
    error: option.Option(String),
    should_close: Bool,
  )
}

/// Connection type - opaque to hide implementation details
pub opaque type Connection {
  Connection(socket: node_socket_client.SocketClient, state: ConnectionState)
}

/// Connection error types
pub type ConnectionError {
  ConnectionFailed(String)
  InvalidHost
  InvalidPort
  SocketError(String)
  Timeout
}

/// Account type for automatic port detection and trading safety
pub type AccountType {
  /// Paper trading account (default port 7497, trading allowed)
  PaperTrading
  /// Live trading account in read-only mode (default port 7496, NO TRADING)
  ///
  /// ⚠️ SAFETY: This type prevents trading operations during development
  /// Use this for testing connections to live account without risk of accidental trades
  LiveTradingReadOnly
  /// Live trading account with full trading permissions (default port 7496)
  ///
  /// ⚠️ PRODUCTION: Only use for production deployment
  /// Never use during development - use LiveTradingReadOnly instead
  LiveTrading
}

/// Connection configuration
pub type ConnectionConfig {
  ConnectionConfig(host: String, port: Int, client_id: Int)
}

/// Create connection config with explicit port
pub fn config(host: String, port: Int, client_id: Int) -> ConnectionConfig {
  ConnectionConfig(host: host, port: port, client_id: client_id)
}

/// Check if trading is allowed for the given account type
/// Returns True for PaperTrading and LiveTrading, False for LiveTradingReadOnly
pub fn is_trading_allowed(account_type: AccountType) -> Bool {
  case account_type {
    PaperTrading -> True
    LiveTradingReadOnly -> False
    LiveTrading -> True
  }
}

/// Get the port for the given account type
pub fn get_port_for_account_type(account_type: AccountType) -> Int {
  case account_type {
    PaperTrading -> 7497
    LiveTradingReadOnly -> 7496
    LiveTrading -> 7496
  }
}

/// Create connection config with account type (auto-detects port)
/// - PaperTrading uses port 7497, trading allowed
/// - LiveTradingReadOnly uses port 7496, NO TRADING (development mode)
/// - LiveTrading uses port 7496, trading allowed (production only)
pub fn config_with_account_type(
  host: String,
  account_type: AccountType,
  client_id: Int,
) -> ConnectionConfig {
  let port = get_port_for_account_type(account_type)
  ConnectionConfig(host: host, port: port, client_id: client_id)
}

/// Get current timestamp as string for debugging
/// Uses Node.js Date.now() via FFI
@external(javascript, "./connection_ffi.mjs", "get_timestamp")
pub fn get_timestamp() -> String

/// Set timeout callback
/// Uses Node.js setTimeout via FFI
@external(javascript, "./connection_ffi.mjs", "set_timeout")
fn set_timeout(milliseconds: Int, callback: fn() -> Nil) -> Nil

/// Sleep for specified milliseconds
/// Uses Node.js setTimeout via FFI
@external(javascript, "./connection_ffi.mjs", "sleep")
pub fn sleep(milliseconds: Int) -> Nil

/// Connect to IB TWS API using TCP socket
/// Returns a connection handle on success, or an error on failure
pub fn connect(config: ConnectionConfig) -> Result(Connection, ConnectionError) {
  let initial_state =
    ConnectionState(
      connected: False,
      received_data: [],
      error: None,
      should_close: False,
    )

  let socket =
    node_socket_client.connect(
      config.host,
      config.port,
      initial_state,
      fn(state, _socket, event) {
        let timestamp = get_timestamp()
        case event {
          ReadyEvent -> {
            io.println("[Connection " <> timestamp <> "] Socket ready")
            ConnectionState(..state, connected: True)
          }
          DataEvent(data) -> {
            io.println(
              "[Connection " <> timestamp <> "] Received data: " <> data,
            )
            let new_data = [data, ..state.received_data]
            ConnectionState(..state, received_data: new_data)
          }
          ErrorEvent(error) -> {
            io.println("[Connection " <> timestamp <> "] Error: " <> error)
            ConnectionState(..state, error: Some(error))
          }
          CloseEvent(_) -> {
            io.println("[Connection " <> timestamp <> "] Connection closed.")
            ConnectionState(..state, connected: False)
          }
          _other_event -> {
            // Ignore other events for now
            state
          }
        }
      },
    )

  Ok(Connection(socket: socket, state: initial_state))
}

/// Send raw data through the connection
pub fn send(conn: Connection, data: String) -> Result(Nil, ConnectionError) {
  let success = node_socket_client.write(conn.socket, data)
  case success {
    True -> Ok(Nil)
    False -> Error(SocketError("Failed to write to socket"))
  }
}

/// Send raw binary data through the connection
/// Takes a BitArray and sends it as raw bytes without string conversion
@external(javascript, "./connection_ffi.mjs", "send_bytes")
fn send_bytes_external(
  socket: node_socket_client.SocketClient,
  data: BitArray,
) -> Bool

/// Send raw binary data through the connection
pub fn send_bytes(
  conn: Connection,
  data: BitArray,
) -> Result(Nil, ConnectionError) {
  let success = send_bytes_external(conn.socket, data)
  case success {
    True -> Ok(Nil)
    False -> Error(SocketError("Failed to write bytes to socket"))
  }
}

/// Receive raw data from connection
/// Returns the most recent data received
pub fn receive(conn: Connection) -> Result(String, ConnectionError) {
  case conn.state.received_data {
    [] -> Error(ConnectionFailed("No data received yet"))
    [first, ..] -> Ok(first)
  }
}

/// Close connection gracefully
pub fn close(conn: Connection) -> Result(Nil, ConnectionError) {
  node_socket_client.end(conn.socket)
  Ok(Nil)
}

/// Destroy connection forcefully
pub fn destroy(conn: Connection) -> Result(Nil, ConnectionError) {
  node_socket_client.destroy(conn.socket)
  Ok(Nil)
}
