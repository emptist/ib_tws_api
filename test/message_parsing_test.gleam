import gleam/bit_array
import gleam/float
import gleam/int
import gleam/io
import message_handler
import messages

pub fn main() {
  io.println("=== Message Parsing Test ===")
  io.println("")

  // Test 1: Error message (code 4)
  io.println("Test 1: Parsing error message (code 4)")
  let error_msg_data = <<4:16, 0:32, 0:32, "Test error":utf8>>

  case bit_array.to_string(error_msg_data) {
    Ok(error_msg_str) -> {
      case messages.parse_message(error_msg_str) {
        Ok(messages.ErrorMsg(error_data)) -> {
          io.println("✓ Successfully parsed error message")
          io.println("  Error ID: " <> int.to_string(error_data.error_id))
          io.println("  Error Code: " <> int.to_string(error_data.error_code))
          io.println("  Error Message: " <> error_data.error_message)
        }
        Ok(other) -> {
          io.println("✗ Unexpected message type: " <> debug_message(other))
        }
        Error(err) -> {
          io.println("✗ Failed to parse: " <> err)
        }
      }
    }
    Error(_) -> {
      io.println("✗ Failed to convert bit array to string")
    }
  }
  io.println("")

  // Test 2: Tick price message (code 1)
  io.println("Test 2: Parsing tick price message (code 1)")
  let tick_price_data = <<1:16, 100:32, 1:32, 123.45:64-float, 100:32>>

  case bit_array.to_string(tick_price_data) {
    Ok(tick_price_str) -> {
      case messages.parse_message(tick_price_str) {
        Ok(messages.TickPrice(tick_data)) -> {
          io.println("✓ Successfully parsed tick price message")
          io.println("  Ticker ID: " <> int.to_string(tick_data.ticker_id))
          io.println("  Tick Type: " <> int.to_string(tick_data.tick_type))
          io.println("  Price: " <> float.to_string(tick_data.price))
          io.println("  Size: " <> int.to_string(tick_data.size))
        }
        Ok(other) -> {
          io.println("✗ Unexpected message type: " <> debug_message(other))
        }
        Error(err) -> {
          io.println("✗ Failed to parse: " <> err)
        }
      }
    }
    Error(_) -> {
      io.println("✗ Failed to convert bit array to string")
    }
  }
  io.println("")

  // Test 3: Unknown message code
  io.println("Test 3: Parsing unknown message code (code 99)")
  let unknown_data = <<99:16, "Unknown data":utf8>>

  case bit_array.to_string(unknown_data) {
    Ok(unknown_str) -> {
      case messages.parse_message(unknown_str) {
        Ok(messages.Unknown(data)) -> {
          io.println("✓ Successfully handled unknown message")
          io.println("  Raw data: " <> data)
        }
        Ok(other) -> {
          io.println("✗ Unexpected message type: " <> debug_message(other))
        }
        Error(err) -> {
          io.println("✗ Failed to parse: " <> err)
        }
      }
    }
    Error(_) -> {
      io.println("✗ Failed to convert bit array to string")
    }
  }
  io.println("")

  // Test 4: Too short message
  io.println("Test 4: Handling too short message")
  let short_data = <<1:8>>

  case bit_array.to_string(short_data) {
    Ok(short_str) -> {
      case messages.parse_message(short_str) {
        Ok(msg) -> {
          io.println("✗ Should have failed but got: " <> debug_message(msg))
        }
        Error(err) -> {
          io.println("✓ Correctly rejected short message")
          io.println("  Error: " <> err)
        }
      }
    }
    Error(_) -> {
      io.println("✗ Failed to convert bit array to string")
    }
  }
  io.println("")

  // Test 5: Real-time bar message (code 52)
  io.println("Test 5: Parsing real-time bar message (code 52)")
  // Create a real-time bar message with sample data
  // Format: msg_id(2) + req_id(4) + time(4) + open(8) + high(8) + low(8) + close(8) + volume(4) + wap(8) + count(4)
  let real_time_bar_data = <<
    52:16,
    100:32,
    1_704_067_200:32,
    450.0:float,
    455.0:float,
    448.0:float,
    452.0:float,
    10_000:32,
    451.5:float,
    500:32,
  >>

  io.println("  Testing real-time bar parsing with message handler...")
  io.println(
    "  Message size: "
    <> int.to_string(bit_array.byte_size(real_time_bar_data))
    <> " bytes",
  )

  // Test with a custom handler that captures the data
  let handler =
    message_handler.MessageHandler(
      on_server_time: fn(_, _) { Nil },
      on_error: fn(_, _, _) { Nil },
      on_tick_price: fn(_, _, _, _) { Nil },
      on_tick_size: fn(_, _, _, _) { Nil },
      on_order_status: fn(_, _, _, _, _, _, _) { Nil },
      on_position: fn(_, _, _, _) { Nil },
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
        io.println("✓ Successfully parsed real-time bar message")
        io.println("  Request ID: " <> int.to_string(req_id))
        io.println("  Time: " <> int.to_string(time))
        io.println("  Open: " <> float.to_string(open))
        io.println("  High: " <> float.to_string(high))
        io.println("  Low: " <> float.to_string(low))
        io.println("  Close: " <> float.to_string(close))
        io.println("  Volume: " <> int.to_string(volume))
        io.println("  WAP: " <> float.to_string(wap))
        io.println("  Count: " <> int.to_string(count))
      },
    )

  // Process the binary message
  message_handler.process_binary_message(real_time_bar_data, handler)
  io.println("")

  io.println("=== Test Complete ===")
}

/// Helper function to debug message types
fn debug_message(msg: messages.Message) -> String {
  case msg {
    messages.ErrorMsg(_) -> "ErrorMsg"
    messages.TickPrice(_) -> "TickPrice"
    messages.TickSize(_) -> "TickSize"
    messages.OrderStatus(_) -> "OrderStatus"
    messages.Position(_) -> "Position"
    messages.AccountSummary(_) -> "AccountSummary"
    messages.RealTimeBar(_) -> "RealTimeBar"
    messages.HistoricalBar(_) -> "HistoricalBar"
    messages.Unknown(_) -> "Unknown"
  }
}
