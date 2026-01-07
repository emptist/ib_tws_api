import gleam/bit_array
import gleam/float
import gleam/int
import gleam/io
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
    messages.Unknown(_) -> "Unknown"
  }
}
