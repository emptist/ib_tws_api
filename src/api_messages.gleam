import gleam/bit_array
import gleam/float
import gleam/int
import gleam/io
import gleam/string

/// Type-safe API message types
/// This prevents protocol errors at compile time by encoding all message structure in the type system
pub type ApiMessage {
  /// START_API message - sent after handshake to establish client connection
  /// Parameters: client_id, version (fixed at 2), optional capabilities
  StartApiMessage(client_id: Int, version: Int, capabilities: String)

  /// REQ_ACCOUNT_SUMMARY message - request account summary data
  /// Parameters: request_id, group_code, tags
  RequestAccountSummary(request_id: Int, group_code: String, tags: String)

  /// REQ_POSITIONS message - request current positions
  /// Parameters: request_id
  RequestPositions(request_id: Int)

  /// REQ_OPEN_ORDERS message - request open orders
  RequestOpenOrders

  /// CANCEL_ORDER message - cancel an order
  /// Parameters: request_id, order_id
  CancelOrder(request_id: Int, order_id: Int)

  /// PLACE_ORDER message - place a new order
  /// Parameters: request_id, contract_id, order_id, action, quantity, order_type, limit_price, stop_price
  PlaceOrder(
    request_id: Int,
    contract_id: Int,
    order_id: Int,
    action: OrderAction,
    quantity: Int,
    order_type: OrderType,
    limit_price: Float,
    stop_price: Float,
  )
}

/// Order action type
pub type OrderAction {
  Buy
  Sell
  Short
}

/// Order type type
pub type OrderType {
  Market
  Limit
  Stop
  StopLimit
}

/// More order types can be added as needed
/// Encode an API message to BitArray with proper protocol format
/// This is the ONLY function that should be used to create API messages
/// It ensures all messages have correct format: [4-byte length][NULL-separated tokens]
/// 
/// Parameters:
/// - message: The type-safe ApiMessage to encode
///
/// Returns: BitArray ready to send to TWS
pub fn encode_message(message: ApiMessage) -> BitArray {
  case message {
    StartApiMessage(client_id, version, capabilities) -> {
      let tokens = [
        int.to_string(71),
        // START_API message ID
        int.to_string(version),
        int.to_string(client_id),
        capabilities,
      ]
      encode_tokens_with_length(tokens)
    }

    RequestAccountSummary(request_id, group_code, tags) -> {
      let tokens = [
        int.to_string(6),
        // REQ_ACCOUNT_SUMMARY message ID
        int.to_string(request_id),
        group_code,
        tags,
      ]
      encode_tokens_with_length(tokens)
    }

    RequestPositions(request_id) -> {
      let tokens = [
        int.to_string(7),
        // REQ_POSITIONS message ID
        int.to_string(request_id),
      ]
      encode_tokens_with_length(tokens)
    }

    RequestOpenOrders -> {
      let tokens = [
        int.to_string(9),
        // REQ_OPEN_ORDERS message ID
      ]
      encode_tokens_with_length(tokens)
    }

    CancelOrder(request_id, order_id) -> {
      let tokens = [
        int.to_string(4),
        // CANCEL_ORDER message ID
        int.to_string(request_id),
        int.to_string(order_id),
      ]
      encode_tokens_with_length(tokens)
    }

    PlaceOrder(
      request_id,
      contract_id,
      order_id,
      action,
      quantity,
      order_type,
      limit_price,
      stop_price,
    ) -> {
      let action_str = case action {
        Buy -> "BUY"
        Sell -> "SELL"
        Short -> "SHORT"
      }
      let order_type_str = case order_type {
        Market -> "MKT"
        Limit -> "LMT"
        Stop -> "STP"
        StopLimit -> "STP LMT"
      }
      let tokens = [
        int.to_string(5),
        // PLACE_ORDER message ID
        int.to_string(request_id),
        int.to_string(contract_id),
        int.to_string(order_id),
        action_str,
        int.to_string(quantity),
        order_type_str,
        float_to_string(limit_price),
        float_to_string(stop_price),
      ]
      encode_tokens_with_length(tokens)
    }
  }
}

/// Internal function to encode tokens with length prefix
/// Parameters:
/// - tokens: List of string tokens
///
/// Returns: BitArray with [4-byte length][NULL-separated tokens]
fn encode_tokens_with_length(tokens: List(String)) -> BitArray {
  // Join with NULL and add final NULL terminator
  let message_string = string.join(tokens, "\u{0000}") <> "\u{0000}"

  io.println("[ApiMessages] Encoding message: " <> message_string)

  // Add 4-byte big-endian length prefix
  let size = string.length(message_string)
  let length_bytes = int_to_four_bytes_big_endian(size)
  let message_bytes = bit_array.from_string(message_string)
  let result = bit_array.concat([length_bytes, message_bytes])

  io.println(
    "[ApiMessages] Encoded "
    <> int.to_string(bit_array.byte_size(result))
    <> " bytes",
  )

  result
}

/// Convert float to string with fixed precision
fn float_to_string(value: Float) -> String {
  // For now, use simple conversion via JavaScript FFI
  // TODO: Add proper decimal formatting if needed
  float_to_string_external(value)
}

/// External function to convert float to string via JavaScript
@external(javascript, "./connection_ffi.mjs", "float_to_string")
fn float_to_string_external(value: Float) -> String

/// Convert integer to 4 bytes (big-endian)
fn int_to_four_bytes_big_endian(value: Int) -> BitArray {
  let b3 = value / 16_777_216
  let temp = value / 65_536
  let b2 = temp % 256
  let temp2 = value / 256
  let b1 = temp2 % 256
  let b0 = value % 256
  <<b3:8, b2:8, b1:8, b0:8>>
}

/// Create START_API message with default parameters
/// This is the most common way to create a START_API message
pub fn start_api_message(client_id: Int) -> ApiMessage {
  StartApiMessage(client_id: client_id, version: 2, capabilities: "")
}

/// Create REQ_ACCOUNT_SUMMARY message with common parameters
pub fn request_account_summary_message(request_id: Int) -> ApiMessage {
  RequestAccountSummary(
    request_id: request_id,
    group_code: "All",
    tags: "$LEDGER:ALL",
  )
}

/// Create REQ_POSITIONS message
pub fn request_positions_message(request_id: Int) -> ApiMessage {
  RequestPositions(request_id: request_id)
}

/// Create REQ_OPEN_ORDERS message
pub fn request_open_orders_message() -> ApiMessage {
  RequestOpenOrders
}

/// Create CANCEL_ORDER message
pub fn cancel_order_message(request_id: Int, order_id: Int) -> ApiMessage {
  CancelOrder(request_id: request_id, order_id: order_id)
}

/// Create PLACE_ORDER message for market order
pub fn place_market_order_message(
  request_id: Int,
  contract_id: Int,
  order_id: Int,
  action: OrderAction,
  quantity: Int,
) -> ApiMessage {
  PlaceOrder(
    request_id: request_id,
    contract_id: contract_id,
    order_id: order_id,
    action: action,
    quantity: quantity,
    order_type: Market,
    limit_price: 0.0,
    stop_price: 0.0,
  )
}

/// Create PLACE_ORDER message for limit order
pub fn place_limit_order_message(
  request_id: Int,
  contract_id: Int,
  order_id: Int,
  action: OrderAction,
  quantity: Int,
  limit_price: Float,
) -> ApiMessage {
  PlaceOrder(
    request_id: request_id,
    contract_id: contract_id,
    order_id: order_id,
    action: action,
    quantity: quantity,
    order_type: Limit,
    limit_price: limit_price,
    stop_price: 0.0,
  )
}

/// Create PLACE_ORDER message for stop order
pub fn place_stop_order_message(
  request_id: Int,
  contract_id: Int,
  order_id: Int,
  action: OrderAction,
  quantity: Int,
  stop_price: Float,
) -> ApiMessage {
  PlaceOrder(
    request_id: request_id,
    contract_id: contract_id,
    order_id: order_id,
    action: action,
    quantity: quantity,
    order_type: Stop,
    limit_price: 0.0,
    stop_price: stop_price,
  )
}
