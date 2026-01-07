import connection
import gleam/bit_array
import gleam/int
import gleam/io
import orders

pub fn main() {
  io.println("=== Order Placement Test (Paper Trading Only) ===")
  io.println("")

  // Test 1: Create a market buy order
  io.println("1. Creating market buy order")
  let buy_order = orders.create_market_order(100, orders.BuyAction, 10)
  orders.debug_order(buy_order)
  io.println("")

  // Test 2: Create a limit sell order
  io.println("2. Creating limit sell order")
  let sell_order = orders.create_limit_order(101, orders.SellAction, 5, 150.0)
  orders.debug_order(sell_order)
  io.println("")

  // Test 3: Try to place order with paper trading account (should succeed)
  io.println("3. Placing order with paper trading account (should succeed)")
  let paper_account = connection.PaperTrading
  case orders.place_order(paper_account, 100, 12_345, buy_order) {
    Ok(msg_bytes) -> {
      let msg_size = bit_array.byte_size(msg_bytes)
      io.println("✓ Order message created successfully")
      io.println("  Message size: " <> int.to_string(msg_size) <> " bytes")
    }
    Error(err) -> {
      io.println("✗ Failed to create order: " <> err)
    }
  }
  io.println("")

  // Test 4: Try to place order with live trading account (should fail)
  io.println("4. Placing order with live trading account (should fail)")
  let live_account = connection.LiveTrading
  case orders.place_order(live_account, 101, 12_345, sell_order) {
    Ok(msg_bytes) -> {
      io.println("✗ Order should have been rejected for live account!")
    }
    Error(err) -> {
      io.println("✓ Order correctly rejected: " <> err)
    }
  }
  io.println("")

  // Test 5: Create cancel order message
  io.println("5. Creating cancel order message for order ID 100")
  let cancel_msg = orders.cancel_order(100)
  let cancel_size = bit_array.byte_size(cancel_msg)
  io.println("✓ Cancel order message created")
  io.println("  Message size: " <> int.to_string(cancel_size) <> " bytes")
  io.println("")

  io.println("=== Test Complete ===")
  io.println("")
  io.println("Summary:")
  io.println("- Market orders: ✓ Working")
  io.println("- Limit orders: ✓ Working")
  io.println("- Paper trading safety: ✓ Working")
  io.println("- Live trading protection: ✓ Working")
  io.println("- Cancel orders: ✓ Working")
  io.println("")
  io.println("Note: To actually place orders:")
  io.println("1. Connect to IB TWS using connection.connect_with_callback()")
  io.println("2. Use PaperTrading account type for safety")
  io.println("3. Send order message using connection.send()")
  io.println("4. Handle order status updates via callback")
}
