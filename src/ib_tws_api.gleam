import connection

/// Connect to IB TWS API using paper trading (port 7497)
/// This is the recommended way for development and testing
pub fn connect_paper(
  host: String,
  client_id: Int,
) -> Result(connection.Connection, connection.ConnectionError) {
  let config = connection.config(host, 7497, client_id)
  connection.connect(config)
}

/// Connect to IB TWS API using live trading (port 7496)
/// WARNING: This uses real money - use with extreme caution
pub fn connect_live(
  host: String,
  client_id: Int,
) -> Result(connection.Connection, connection.ConnectionError) {
  let config = connection.config(host, 7496, client_id)
  connection.connect(config)
}

/// Send raw data through the connection
pub fn send(
  conn: connection.Connection,
  data: String,
) -> Result(Nil, connection.ConnectionError) {
  connection.send(conn, data)
}

/// Receive raw data from the connection
pub fn receive(
  conn: connection.Connection,
) -> Result(String, connection.ConnectionError) {
  connection.receive(conn)
}

/// Close the connection
pub fn close(
  conn: connection.Connection,
) -> Result(Nil, connection.ConnectionError) {
  connection.close(conn)
}
