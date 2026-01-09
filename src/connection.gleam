import gleam/bit_array
import gleam/io
import gleam/option.{None, Some}
import gleam/string
import node_socket_client.{CloseEvent, DataEvent, ErrorEvent, ReadyEvent}
import protocol.{string_to_hex}

/// Callback type for handling received data
pub type DataCallback =
  fn(String) -> Nil

/// Possible connection states
pub type ConnectionStateType {
  Disconnected
  Connecting
  Connected
  HandshakeSent
  Ready
  Failed(String)
  // Renamed from Error to avoid conflict
  Closing
  Closed
}

/// Connection state for event handling
pub type ConnectionState {
  ConnectionState(
    current_state: ConnectionStateType,
    previous_state: ConnectionStateType,
    received_data: List(String),
    error: option.Option(String),
    on_data_callback: option.Option(DataCallback),
    last_transition_time: String,
    handshake_version: option.Option(Int),
    server_version: option.Option(Int),
  )
}

/// Transition to a new state
fn transition_state(
  state: ConnectionState,
  new_state: ConnectionStateType,
) -> ConnectionState {
  ConnectionState(
    current_state: new_state,
    previous_state: state.current_state,
    received_data: state.received_data,
    error: state.error,
    on_data_callback: state.on_data_callback,
    last_transition_time: get_timestamp(),
    handshake_version: state.handshake_version,
    server_version: state.server_version,
  )
}

/// Validate if a transition is allowed
fn is_valid_transition(
  from: ConnectionStateType,
  to: ConnectionStateType,
) -> Bool {
  case from, to {
    Disconnected, Connecting -> True
    Connecting, Connected -> True
    Connecting, Failed(_) -> True
    Connected, HandshakeSent -> True
    Connected, Failed(_) -> True
    HandshakeSent, Ready -> True
    HandshakeSent, Failed(_) -> True
    Ready, Closing -> True
    Ready, Failed(_) -> True
    Closing, Closed -> True
    Failed(_), Closed -> True
    _, _ -> False
  }
}

/// Helper to convert ConnectionError to string for logging
fn connection_error_to_string(error: ConnectionError) -> String {
  case error {
    ConnectionFailed(msg) -> msg
    InvalidHost -> "Invalid host"
    InvalidPort -> "Invalid port"
    SocketError(msg) -> msg
    Timeout -> "Connection timeout"
  }
}

/// Helper to create Result with ConnectionFailed error
fn connection_failed_result(msg: String) -> Result(a, ConnectionError) {
  Error(ConnectionFailed(msg))
}

/// Helper to create Result with SocketError
fn socket_error_result(msg: String) -> Result(a, ConnectionError) {
  Error(SocketError(msg))
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
) -> Result(ConnectionConfig, ConnectionError) {
  let detected_port = detect_ib_tws_port(host, timeout)
  case detected_port {
    0 -> connection_failed_result("No IB TWS server detected")
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
      current_state: Disconnected,
      previous_state: Disconnected,
      received_data: [],
      error: None,
      on_data_callback: callback,
      last_transition_time: get_timestamp(),
      handshake_version: None,
      server_version: None,
    )

  let socket =
    node_socket_client.connect(
      config.host,
      config.port,
      initial_state,
      fn(state, socket, event) {
        let timestamp = get_timestamp()
        case event {
          ReadyEvent -> {
            io.println(
              "[Connection "
              <> timestamp
              <> "] Socket ready - transitioning to Connected state",
            )
            let new_state = transition_state(state, Connected)

            // Send properly formatted handshake immediately
            let handshake = protocol.start_api_message_with_length(100, 200)
            let success = send_bytes_external(socket, handshake)
            case success {
              True -> {
                io.println(
                  "[Connection " <> timestamp <> "] Handshake sent successfully",
                )
                let handshake_state = transition_state(new_state, HandshakeSent)
                handshake_state
              }
              False -> {
                io.println(
                  "[Connection "
                  <> timestamp
                  <> "] ERROR: Failed to send handshake - SocketError",
                )
                transition_state(
                  new_state,
                  Failed("Failed to send handshake: SocketError"),
                )
              }
            }
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
                // Update state but keep current state intact
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
            ConnectionState(..state, current_state: Closed)
          }
          _other_event -> {
            // Ignore other events for now
            state
          }
        }
      },
    )

  // Create connection with initial state
  // The socket client will update the state through the callback
  let conn = Connection(socket: socket, state: initial_state)
  Ok(conn)
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
  case is_connected(conn) {
    True -> {
      let success = node_socket_client.write(conn.socket, data)
      case success {
        True -> Ok(Nil)
        False -> socket_error_result("Failed to write to socket")
      }
    }
    False -> connection_failed_result("Not connected")
  }
}

/// Get the current state from socket (dynamic, updates via callbacks)
@external(javascript, "./connection_ffi.mjs", "get_socket_current_state")
fn get_socket_state(socket: node_socket_client.SocketClient) -> ConnectionState

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
  case is_connected(conn) {
    True -> {
      let success = send_bytes_external(conn.socket, data)
      case success {
        True -> Ok(Nil)
        False -> socket_error_result("Failed to write bytes to socket")
      }
    }
    False -> connection_failed_result("Not connected")
  }
}

/// Receive raw data from connection
/// Returns the most recent data received
pub fn receive(conn: Connection) -> Result(String, ConnectionError) {
  case is_connected(conn) {
    True -> {
      case conn.state.received_data {
        [] -> connection_failed_result("No data received yet")
        [first, ..] -> Ok(first)
      }
    }
    False -> connection_failed_result("Not connected")
  }
}

/// Close connection gracefully
/// Check if connection is ready to send API requests
/// Connection is ready only in Ready state
pub fn is_ready(conn: Connection) -> Bool {
  let current_state = get_socket_state(conn.socket)
  case current_state.current_state {
    Ready -> True
    _ -> False
  }
}

/// Transition to Ready state when nextValidId is received
pub fn set_ready(conn: Connection) -> Result(Connection, ConnectionError) {
  case is_valid_transition(conn.state.current_state, Ready) {
    True -> {
      let new_state = transition_state(conn.state, Ready)
      Ok(Connection(socket: conn.socket, state: new_state))
    }
    False -> {
      connection_failed_result(
        "Invalid state transition to Ready from "
        <> state_to_string(conn.state.current_state),
      )
    }
  }
}

/// Convert state to string for debugging
pub fn state_to_string(state: ConnectionStateType) -> String {
  case state {
    Disconnected -> "Disconnected"
    Connecting -> "Connecting"
    Connected -> "Connected"
    HandshakeSent -> "HandshakeSent"
    Ready -> "Ready"
    Failed(msg) -> "Failed(" <> msg <> ")"
    Closing -> "Closing"
    Closed -> "Closed"
  }
}

/// Check if connection is connected (not including ready state)
pub fn is_connected(conn: Connection) -> Bool {
  let current_state = get_socket_state(conn.socket)
  case current_state.current_state {
    Connected | HandshakeSent | Ready -> True
    _ -> False
  }
}

/// Get current connection state
pub fn get_state(conn: Connection) -> ConnectionStateType {
  let current_state = get_socket_state(conn.socket)
  current_state.current_state
}

/// Get previous connection state
pub fn get_previous_state(conn: Connection) -> ConnectionStateType {
  let current_state = get_socket_state(conn.socket)
  current_state.previous_state
}

/// Wait for connection to be ready
/// This blocks until the nextValidId event is received
/// Returns True when ready, False if connection is closed
pub fn wait_for_ready(conn: Connection, timeout_ms: Int) -> Bool {
  let _start = get_timestamp()
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
      let current_state = get_socket_state(conn.socket)
      case current_state.current_state {
        Ready -> True
        // Ready!
        _ -> {
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
  case is_ready(conn) {
    True -> send(conn, data)
    False -> {
      connection_failed_result(
        "Connection not ready - current state: "
        <> state_to_string(conn.state.current_state),
      )
    }
  }
}

/// Send binary message only when connection is ready
/// Returns error if connection is not ready
pub fn send_bytes_when_ready(
  conn: Connection,
  data: BitArray,
) -> Result(Nil, ConnectionError) {
  case is_ready(conn) {
    True -> send_bytes(conn, data)
    False -> {
      connection_failed_result(
        "Connection not ready - current state: "
        <> state_to_string(conn.state.current_state),
      )
    }
  }
}

pub fn close(conn: Connection) -> Result(Nil, ConnectionError) {
  //! I have commented this out because I don't want AI's code to
  //! close the connection stupidly without my control.
  // node_socket_client.end(conn.socket)
  io.println(
    "[Connection] Current state: " <> state_to_string(conn.state.current_state),
  )
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
