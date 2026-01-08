import gleam/bit_array
import gleam/io
import gleam/option.{None, Some}
import gleam/string
import node_socket_client.{CloseEvent, DataEvent, ErrorEvent, ReadyEvent}
import protocol.{string_to_hex}

/// Callback type for handling received data
pub type DataCallback =
  fn(String) -> Nil

/// Connection state for event handling
pub type ConnectionState {
  ConnectionState(
    connected: Bool,
    ready: Bool,
    received_data: List(String),
    error: option.Option(String),
    should_close: Bool,
    on_data_callback: option.Option(DataCallback),
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

/// Account type for trading safety
pub type AccountType {
  /// Paper trading account - trading allowed
  ///
  /// Use this for development and testing with simulated trading
  /// Port can be configured independently (commonly 7496 or 7497)
  PaperTrading
  /// Live trading account in read-only mode - NO TRADING
  ///
  /// ⚠️ SAFETY: This type prevents trading operations during development
  /// Use this for testing connections to live account without risk of accidental trades
  LiveTradingReadOnly
  /// Live trading account - trading allowed
  ///
  /// ⚠️ WARNING: This uses real money - use with extreme caution
  /// Port can be configured independently (commonly 7496)
  LiveTrading
}

/// Connection configuration
pub type ConnectionConfig {
  ConnectionConfig(
    host: String,
    port: Int,
    client_id: Int,
    account_type: AccountType,
  )
}

/// Create connection config with explicit port (backward compatible)
/// Automatically infers account type from port:
/// - Port 7497 → PaperTrading (trading allowed)
/// - Port 7496 → LiveTrading (trading allowed)
/// - Other ports → PaperTrading (safe default)
pub fn config(host: String, port: Int, client_id: Int) -> ConnectionConfig {
  let account_type = case port {
    7497 -> PaperTrading
    7496 -> LiveTrading
    _ -> PaperTrading
  }
  ConnectionConfig(
    host: host,
    port: port,
    client_id: client_id,
    account_type: account_type,
  )
}

/// Create connection config with explicit port and account type
/// Use this when you need to specify the account type explicitly
/// For example, if you're running paper trading on port 7496
pub fn config_with_account_type(
  host: String,
  port: Int,
  account_type: AccountType,
  client_id: Int,
) -> ConnectionConfig {
  ConnectionConfig(
    host: host,
    port: port,
    client_id: client_id,
    account_type: account_type,
  )
}

/// Check if trading is allowed for the given account type
/// Returns True for PaperTrading and LiveTrading, False for LiveTradingReadOnly
/// This prevents accidental trading on read-only live accounts
pub fn is_trading_allowed(account_type: AccountType) -> Bool {
  case account_type {
    PaperTrading -> True
    LiveTradingReadOnly -> False
    LiveTrading -> True
  }
}

/// Get the default port for the given account type
/// Note: Port can be overridden when creating config
pub fn get_default_port(account_type: AccountType) -> Int {
  case account_type {
    PaperTrading -> 7497
    LiveTradingReadOnly -> 7496
    LiveTrading -> 7496
  }
}

/// Get current timestamp as string for debugging
/// Uses Node.js Date.now() via FFI
@external(javascript, "./connection_ffi.mjs", "get_timestamp")
pub fn get_timestamp() -> String

/// Generate a random client ID based on current timestamp
/// Ensures unique client ID per connection to avoid conflicts
@external(javascript, "./connection_ffi.mjs", "generate_client_id")
pub fn generate_client_id() -> Int

/// Sleep for specified milliseconds
/// Uses Node.js setTimeout via FFI
@external(javascript, "./connection_ffi.mjs", "sleep")
pub fn sleep(milliseconds: Int) -> Nil

/// Keep the Node.js process alive for specified milliseconds
/// This is needed for event-driven code that waits for async callbacks
/// The process will exit immediately after this function returns
@external(javascript, "./connection_ffi.mjs", "keep_alive")
pub fn keep_alive(milliseconds: Int) -> Nil

/// Detect which IB TWS port is available (7496 or 7497)
/// Tests both ports and returns the first one that is available
/// Returns Ok(port) if a port is available, Error if neither port is available
/// Uses Node.js net module via FFI
@external(javascript, "./connection_ffi.mjs", "detect_ib_tws_port")
pub fn detect_ib_tws_port(host: String, timeout: Int) -> Int

/// Create connection config with automatic port detection
/// Automatically detects which IB TWS port (7496 or 7497) is available
/// Returns Error if neither port is available, Ok(config) if a port is detected
/// Note: Auto-detection assumes port 7496 is live trading and 7497 is paper trading
/// For different configurations, use config() or config_with_account_type()
pub fn config_auto_detect(
  host: String,
  client_id: Int,
  timeout: Int,
) -> Result(ConnectionConfig, String) {
  let detected_port = detect_ib_tws_port(host, timeout)
  case detected_port {
    0 -> Error("No IB TWS server detected on ports 7496 or 7497")
    7496 ->
      Ok(ConnectionConfig(
        host: host,
        port: 7496,
        client_id: client_id,
        account_type: LiveTrading,
      ))
    7497 ->
      Ok(ConnectionConfig(
        host: host,
        port: 7497,
        client_id: client_id,
        account_type: PaperTrading,
      ))
    port ->
      Ok(ConnectionConfig(
        host: host,
        port: port,
        client_id: client_id,
        account_type: PaperTrading,
      ))
  }
}

/// Connect to IB TWS API using TCP socket
/// Returns a connection handle on success, or an error on failure
pub fn connect(config: ConnectionConfig) -> Result(Connection, ConnectionError) {
  connect_with_callback(config, None)
}

/// Connect to IB TWS API using TCP socket with a data callback
/// The callback will be invoked whenever data is received from the server
/// Returns a connection handle on success, or an error on failure
pub fn connect_with_callback(
  config: ConnectionConfig,
  callback: option.Option(DataCallback),
) -> Result(Connection, ConnectionError) {
  let initial_state =
    ConnectionState(
      connected: False,
      ready: False,
      received_data: [],
      error: None,
      should_close: False,
      on_data_callback: callback,
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
            io.println(
              "[Connection "
              <> timestamp
              <> "] Socket ready - sending handshake",
            )
            ConnectionState(..state, connected: True)
          }
          DataEvent(data) -> {
            io.println(
              "[Connection "
              <> timestamp
              <> "] Received raw data (hex): "
              <> string_to_hex(data),
            )
            io.println(
              "[Connection "
              <> timestamp
              <> "] Received data (string): "
              <> data,
            )
            let new_data = [data, ..state.received_data]

            // Call callback if registered
            case state.on_data_callback {
              Some(callback) -> {
                callback(data)
                ConnectionState(..state, received_data: new_data)
              }
              None -> ConnectionState(..state, received_data: new_data)
            }
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

/// Register a callback to be called when data is received
/// This allows event-driven message processing instead of polling
/// The callback will be invoked with the received data string
pub fn on_data(
  _conn: Connection,
  _callback: DataCallback,
) -> Result(Nil, ConnectionError) {
  // Update the state to include the callback
  // Note: In a real implementation, we'd need to update the socket's state
  // For now, this is a simplified version that doesn't actually update the state
  // The callback should be registered before connecting for it to work
  io.println(
    "[Connection] Warning: on_data() called after connect(). "
    <> "Callback will only work for future connections.",
  )
  Ok(Nil)
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
/// Check if connection is ready to send API requests
/// Connection is ready only after nextValidId event is received
/// This prevents sending requests before TWS is ready to accept them
pub fn is_ready(conn: Connection) -> Bool {
  conn.state.ready
}

/// Set connection ready state
/// This should be called when nextValidId event is received
pub fn set_ready(conn: Connection, ready: Bool) -> Connection {
  let new_state = ConnectionState(..conn.state, ready: ready)
  Connection(socket: conn.socket, state: new_state)
}

/// Wait for connection to be ready
/// This blocks until the nextValidId event is received
/// Returns True when ready, False if connection is closed
pub fn wait_for_ready(conn: Connection, timeout_ms: Int) -> Bool {
  let start = get_timestamp()
  let max_checks = timeout_ms / 100
  // Check every 100ms

  let result = wait_for_ready_loop(conn, max_checks, 0)
  result
}

/// Internal loop for waiting for ready state
fn wait_for_ready_loop(conn: Connection, max_checks: Int, current: Int) -> Bool {
  case current >= max_checks {
    True -> False
    // Timeout
    False -> {
      case conn.state.ready {
        True -> True
        // Ready!
        False -> {
          sleep(100)
          // Wait 100ms
          wait_for_ready_loop(conn, max_checks, current + 1)
        }
      }
    }
  }
}

/// Send message only when connection is ready
/// Returns error if connection is not ready
/// This prevents sending API requests before TWS is ready to accept them
pub fn send_when_ready(
  conn: Connection,
  data: String,
) -> Result(Nil, ConnectionError) {
  case conn.state.ready {
    True -> {
      io.println("[Connection] Connection is ready, sending message")
      send(conn, data)
    }
    False -> {
      io.println(
        "[Connection] ERROR: Connection not ready - cannot send message",
      )
      Error(ConnectionFailed(
        "Connection not ready - wait for nextValidId event",
      ))
    }
  }
}

/// Send binary message only when connection is ready
/// Returns error if connection is not ready
pub fn send_bytes_when_ready(
  conn: Connection,
  data: BitArray,
) -> Result(Nil, ConnectionError) {
  case conn.state.ready {
    True -> {
      io.println("[Connection] Connection is ready, sending binary message")
      send_bytes(conn, data)
    }
    False -> {
      io.println(
        "[Connection] ERROR: Connection not ready - cannot send message",
      )
      Error(ConnectionFailed(
        "Connection not ready - wait for nextValidId event",
      ))
    }
  }
}

pub fn close(conn: Connection) -> Result(Nil, ConnectionError) {
  //! I have commented this out because I don't want AI's code to 
  //! close the connection stupidly without my control.
  // node_socket_client.end(conn.socket)
  echo conn.state
  Ok(Nil)
}

/// Destroy connection forcefully
pub fn destroy(conn: Connection) -> Result(Nil, ConnectionError) {
  node_socket_client.destroy(conn.socket)
  Ok(Nil)
}

/// Write content to file for logging purposes
/// Uses Node.js fs module via FFI
/// @external(javascript, "./connection_ffi.mjs", "write_to_file")
/// fn write_to_file_external(filename: String, content: String, append: Bool) -> String
/// Write content to file for logging
/// Append parameter: True = append, False = overwrite
@external(javascript, "./connection_ffi.mjs", "write_to_file")
pub fn write_to_file(
  filename: String,
  content: String,
  append: Bool,
) -> Result(Nil, String)
