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
        case event {
          ReadyEvent -> {
            io.println("[Connection] Socket ready")
            ConnectionState(..state, connected: True)
          }
          DataEvent(data) -> {
            io.println("[Connection] Received data: " <> data)
            let new_data = [data, ..state.received_data]
            ConnectionState(..state, received_data: new_data)
          }
          ErrorEvent(error) -> {
            io.println("[Connection] Error: " <> error)
            ConnectionState(..state, error: Some(error))
          }
          CloseEvent(_) -> {
            io.println("[Connection] Connection closed")
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
