/// IB TWS Protocol handling
/// This module implements the IB TWS protocol handshake and message parsing
import gleam/int
import gleam/io
import gleam/string
import types

/// IB TWS API version we're using
const api_version = 976

/// Perform handshake with IB TWS server
/// This sends the initial connection messages and verifies the connection
pub fn handshake(conn: types.Connection) -> Result(Nil, types.ConnectionError) {
  let types.Connection(config: config) = conn

  // Step 1: Send API version
  io.println("Step 1: Sending API version " <> int.to_string(api_version))
  let version_msg = int.to_string(api_version)
  // In real implementation, this would send via actual socket
  io.println("  Sent: " <> version_msg)

  // Step 2: Send client ID
  let types.ConnectionConfig(client_id: client_id, ..) = config
  io.println("Step 2: Sending client ID " <> int.to_string(client_id))
  let client_id_msg = int.to_string(client_id)
  io.println("  Sent: " <> client_id_msg)

  // Step 3: Wait for server response (server time)
  io.println("Step 3: Waiting for server time...")
  // In real implementation, this would receive and parse server time
  io.println("  Received: Server time (placeholder)")

  Ok(Nil)
}

/// Parse a raw message from the server
/// Messages are length-prefixed in IB TWS protocol
pub fn parse_message(
  raw_data: String,
) -> Result(types.Message, types.ConnectionError) {
  // Placeholder - will implement actual message parsing
  // IB TWS messages have a specific format with message codes
  io.println("Parsing message: " <> raw_data)

  // For now, return a placeholder message
  Ok(types.Message(message_type: types.ServerTime, payload: raw_data))
}

/// Encode a message to send to the server
pub fn encode_message(msg_type: Int, params: List(String)) -> String {
  // IB TWS messages are encoded as: msg_type\0param1\0param2\0...
  let msg_type_str = int.to_string(msg_type)
  let params_str = string.join(params, "\\0")

  msg_type_str <> "\\0" <> params_str <> "\\0"
}

/// Message codes for IB TWS API
pub const msg_server_time = 9

pub const msg_error = 4

pub const msg_market_data = 1

pub const msg_order_status = 3
