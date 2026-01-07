import connection
import gleam/bit_array
import gleam/float
import gleam/int
import gleam/io
import protocol

/// Message handler type for processing incoming IB TWS API messages
pub type MessageHandler {
  MessageHandler(
    on_server_time: fn(Int, String) -> Nil,
    on_error: fn(Int, Int, String) -> Nil,
    on_tick_price: fn(Int, Int, Float, Int) -> Nil,
    on_tick_size: fn(Int, Int, Float, Int) -> Nil,
    on_order_status: fn(Int, String, Int, Float, Int, String, Int) -> Nil,
    on_position: fn(Int, String, Float, Float) -> Nil,
  )
}

/// Default message handler that logs all messages
pub fn default_handler() -> MessageHandler {
  MessageHandler(
    on_server_time: fn(version, timestamp) {
      io.println(
        "[Message] Server Time - Version: "
        <> int.to_string(version)
        <> ", Timestamp: "
        <> timestamp,
      )
    },
    on_error: fn(error_code, error_id, error_msg) {
      io.println(
        "[Error] Code: "
        <> int.to_string(error_code)
        <> ", ID: "
        <> int.to_string(error_id)
        <> ", Message: "
        <> error_msg,
      )
    },
    on_tick_price: fn(ticker_id, field, price, size) {
      io.println(
        "[TickPrice] Ticker ID: "
        <> int.to_string(ticker_id)
        <> ", Field: "
        <> int.to_string(field)
        <> ", Price: "
        <> float.to_string(price)
        <> ", Size: "
        <> int.to_string(size),
      )
    },
    on_tick_size: fn(ticker_id, field, size, _) {
      io.println(
        "[TickSize] Ticker ID: "
        <> int.to_string(ticker_id)
        <> ", Field: "
        <> int.to_string(field)
        <> ", Size: "
        <> float.to_string(size),
      )
    },
    on_order_status: fn(order_id, status, filled, remaining, avg_fill, _, _) {
      io.println(
        "[OrderStatus] ID: "
        <> int.to_string(order_id)
        <> ", Status: "
        <> status
        <> ", Filled: "
        <> int.to_string(filled)
        <> ", Remaining: "
        <> float.to_string(remaining)
        <> ", Avg Fill: "
        <> int.to_string(avg_fill),
      )
    },
    on_position: fn(account, contract, position, _) {
      io.println(
        "[Position] Account ID: "
        <> int.to_string(account)
        <> ", Contract: "
        <> contract
        <> ", Position: "
        <> float.to_string(position),
      )
    },
  )
}

/// Process incoming raw data and dispatch to appropriate handler
/// This is a simplified parser - will be expanded as needed
pub fn process_message(data: String, handler: MessageHandler) -> Nil {
  // Simple parsing based on message content
  // Full IB TWS protocol parsing will be implemented as needed

  // Check if this is a server time response
  case protocol.parse_server_response(data) {
    Ok(#(version, timestamp)) -> {
      handler.on_server_time(version, timestamp)
    }
    Error(_) -> {
      // Not a server time message, check other patterns
      io.println("[MessageHandler] Received unhandled message: " <> data)
    }
  }
}

/// Process incoming binary data and dispatch to appropriate handler
/// This is a simplified parser - will be expanded as needed
pub fn process_binary_message(data: BitArray, handler: MessageHandler) -> Nil {
  let size = bit_array.byte_size(data)

  case size {
    0 -> {
      io.println("[MessageHandler] Empty message received")
    }
    _ -> {
      // Check for known message patterns
      // This is a simplified parser - will be expanded as needed
      io.println(
        "[MessageHandler] Received binary message: "
        <> int.to_string(size)
        <> " bytes",
      )
    }
  }
}
