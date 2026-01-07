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
    on_real_time_bar: fn(Int, Int, Float, Float, Float, Float, Int, Float, Int) ->
      Nil,
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
    on_real_time_bar: fn(
      req_id,
      time,
      open,
      high,
      low,
      close,
      volume,
      wap,
      count,
    ) {
      io.println(
        "[RealTimeBar] Req ID: "
        <> int.to_string(req_id)
        <> ", Time: "
        <> int.to_string(time)
        <> ", O: "
        <> float.to_string(open)
        <> ", H: "
        <> float.to_string(high)
        <> ", L: "
        <> float.to_string(low)
        <> ", C: "
        <> float.to_string(close)
        <> ", Vol: "
        <> int.to_string(volume)
        <> ", WAP: "
        <> float.to_string(wap)
        <> ", Count: "
        <> int.to_string(count),
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

/// Parse a real-time bar message (message code 52)
/// Format: msg_id(2) + req_id(4) + time(4) + open(8) + high(8) + low(8) + close(8) + volume(4) + wap(8) + count(4)
fn parse_real_time_bar(
  data: BitArray,
) -> Result(#(Int, Int, Float, Float, Float, Float, Int, Float, Int), String) {
  // Try to parse the entire message in one pattern match
  case data {
    <<
      52:16,
      req_id:32,
      time:32,
      open:float,
      high:float,
      low:float,
      close:float,
      volume:32,
      wap:float,
      count:32,
      _rest:bits,
    >> -> {
      Ok(#(req_id, time, open, high, low, close, volume, wap, count))
    }
    _ -> {
      Error("Not a real-time bar message (expected message code 52)")
    }
  }
}

/// Process incoming binary data and dispatch to appropriate handler
/// Enhanced to properly parse real-time bar messages
pub fn process_binary_message(data: BitArray, handler: MessageHandler) -> Nil {
  let size = bit_array.byte_size(data)

  case size {
    0 -> {
      io.println("[MessageHandler] Empty message received")
    }
    _ -> {
      // Try to parse as real-time bar message (code 52)
      case parse_real_time_bar(data) {
        Ok(#(req_id, time, open, high, low, close, volume, wap, count)) -> {
          handler.on_real_time_bar(
            req_id,
            time,
            open,
            high,
            low,
            close,
            volume,
            wap,
            count,
          )
        }
        Error(_) -> {
          // Not a real-time bar message, log for now
          // Additional message types will be added as needed
          io.println(
            "[MessageHandler] Received binary message: "
            <> int.to_string(size)
            <> " bytes (not a RealTimeBar)",
          )
        }
      }
    }
  }
}
