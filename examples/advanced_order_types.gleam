import connection
import gleam/int
import gleam/io
import gleam/option.{Some}
import market_data
import orders

/// Demonstrates all advanced order types with Gleam's type safety
/// This example shows how Gleam's type system prevents invalid order configurations
pub fn main() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("Advanced Order Types Example - Type-Safe Order Creation")
  io.println(repeat_char("=", 70))
  io.println("")

  // Configuration
  let account_type = connection.PaperTrading
  let client_id = connection.generate_client_id()
  let config =
    connection.config_with_account_type(
      "127.0.0.1",
      7497,
      account_type,
      client_id,
    )

  io.println("Account Type: Paper Trading (Port 7497)")
  io.println(
    "Trading Allowed: "
    <> bool.to_string(connection.is_trading_allowed(account_type)),
  )
  io.println("")

  // ========================================================================
  // 1. Market Order - Executes immediately at current market price
  // ========================================================================
  io.println(repeat_char("-", 70))
  io.println("1. MARKET ORDER")
  io.println(repeat_char("-", 70))
  io.println("Type: Executes immediately at current market price")
  io.println("Use: When you want to enter/exit position immediately")
  io.println("")

  let market_buy = orders.create_market_order(100, orders.BuyAction, 100)
  orders.debug_order(market_buy)
  io.println("✓ Market order created - type-safe (no price needed!)")
  io.println("")

  // ========================================================================
  // 2. Limit Order - Executes at specified price or better
  // ========================================================================
  io.println(repeat_char("-", 70))
  io.println("2. LIMIT ORDER")
  io.println(repeat_char("-", 70))
  io.println("Type: Executes at specified price or better")
  io.println("Use: When you want to control entry/exit price")
  io.println("")

  let limit_buy = orders.create_limit_order(101, orders.BuyAction, 100, 150.0)
  orders.debug_order(limit_buy)
  io.println("✓ Limit order created - type-safe (limit_price required!)")
  io.println("")

  // ========================================================================
  // 3. Stop Order - Becomes market order when stop price is reached
  // ========================================================================
  io.println(repeat_char("-", 70))
  io.println("3. STOP ORDER")
  io.println(repeat_char("-", 70))
  io.println("Type: Becomes market order when stop price is reached")
  io.println("Use: For stop-loss or breakout trading")
  io.println("")

  let stop_sell = orders.create_stop_order(102, orders.SellAction, 100, 145.0)
  orders.debug_order(stop_sell)
  io.println("✓ Stop order created - type-safe (stop_price required!)")
  io.println("")

  // ========================================================================
  // 4. Stop-Limit Order - Becomes limit order when stop price is reached
  // ========================================================================
  io.println(repeat_char("-", 70))
  io.println("4. STOP-LIMIT ORDER")
  io.println(repeat_char("-", 70))
  io.println("Type: Becomes limit order when stop price is reached")
  io.println("Use: For precise stop-loss with price control")
  io.println("")

  let stop_limit_sell =
    orders.create_stop_limit_order(103, orders.SellAction, 100, 145.0, 144.0)
  orders.debug_order(stop_limit_sell)
  io.println("✓ Stop-limit order created - type-safe (both prices required!)")
  io.println("")

  // ========================================================================
  // 5. Trailing Stop Order (by amount) - Stop price trails market
  // ========================================================================
  io.println(repeat_char("-", 70))
  io.println("5. TRAILING STOP ORDER (BY AMOUNT)")
  io.println(repeat_char("-", 70))
  io.println("Type: Stop price trails market by fixed dollar amount")
  io.println("Use: For trailing stop-loss in trending markets")
  io.println("")

  let trail_amount_sell =
    orders.create_trailing_stop_amount(104, orders.SellAction, 100, 5.0)
  orders.debug_order(trail_amount_sell)
  io.println("✓ Trailing stop (amount) created - type-safe!")
  io.println("")

  // ========================================================================
  // 6. Trailing Stop Order (by percent) - Stop price trails market by %
  // ========================================================================
  io.println(repeat_char("-", 70))
  io.println("6. TRAILING STOP ORDER (BY PERCENT)")
  io.println(repeat_char("-", 70))
  io.println("Type: Stop price trails market by percentage")
  io.println("Use: For trailing stop-loss with proportional risk")
  io.println("")

  let trail_percent_sell =
    orders.create_trailing_stop_percent(105, orders.SellAction, 100, 3.0)
  orders.debug_order(trail_percent_sell)
  io.println("✓ Trailing stop (percent) created - type-safe!")
  io.println("")

  // ========================================================================
  // Type Safety Demonstration
  // ========================================================================
  io.println(repeat_char("=", 70))
  io.println("GLEAM TYPE SYSTEM BENEFITS")
  io.println(repeat_char("=", 70))
  io.println("")
  io.println("✓ Invalid states are IMPOSSIBLE at compile time:")
  io.println("")
  io.println("  1. Market orders CANNOT have prices (type-safe)")
  io.println("  2. Limit orders MUST have limit_price (type-safe)")
  io.println("  3. Stop orders MUST have stop_price (type-safe)")
  io.println("  4. Stop-limit orders MUST have both prices (type-safe)")
  io.println(
    "  5. Trailing orders MUST have trailing amount or percent (type-safe)",
  )
  io.println("")

  // ========================================================================
  // Helper Functions Demonstration
  // ========================================================================
  io.println(repeat_char("-", 70))
  io.println("HELPER FUNCTIONS")
  io.println(repeat_char("-", 70))
  io.println("")

  io.println("Extracting order ID from any order type:")
  let order_id = orders.get_order_id(market_buy)
  io.println("  Order ID: " <> int.to_string(order_id))
  io.println("")

  io.println("Extracting order action from any order type:")
  let action = orders.get_order_action(limit_buy)
  let action_str = case action {
    orders.BuyAction -> "BUY"
    orders.SellAction -> "SELL"
    orders.ShortAction -> "SSHORT"
  }
  io.println("  Order Action: " <> action_str)
  io.println("")

  // ========================================================================
  // Creating Order Messages
  // ========================================================================
  io.println(repeat_char("-", 70))
  io.println("CREATING ORDER MESSAGES FOR IB TWS")
  io.println(repeat_char("-", 70))
  io.println("")

  let contract_id = 12_345

  case orders.place_order(account_type, contract_id, market_buy) {
    Ok(msg_bytes) -> {
      io.println("✓ Market order message created successfully")
      io.println("  This would be sent to IB TWS to place the order")
      io.println("")
    }
    Error(err) -> {
      io.println("✗ Failed to create market order: " <> err)
    }
  }

  case orders.place_order(account_type, contract_id, limit_buy) {
    Ok(msg_bytes) -> {
      io.println("✓ Limit order message created successfully")
      io.println("  This would be sent to IB TWS to place the order")
      io.println("")
    }
    Error(err) -> {
      io.println("✗ Failed to create limit order: " <> err)
    }
  }

  case orders.place_order(account_type, contract_id, stop_sell) {
    Ok(msg_bytes) -> {
      io.println("✓ Stop order message created successfully")
      io.println("  This would be sent to IB TWS to place the order")
      io.println("")
    }
    Error(err) -> {
      io.println("✗ Failed to create stop order: " <> err)
    }
  }

  case orders.place_order(account_type, contract_id, stop_limit_sell) {
    Ok(msg_bytes) -> {
      io.println("✓ Stop-limit order message created successfully")
      io.println("  This would be sent to IB TWS to place the order")
      io.println("")
    }
    Error(err) -> {
      io.println("✗ Failed to create stop-limit order: " <> err)
    }
  }

  case orders.place_order(account_type, contract_id, trail_amount_sell) {
    Ok(msg_bytes) -> {
      io.println("✓ Trailing stop (amount) message created successfully")
      io.println("  This would be sent to IB TWS to place the order")
      io.println("")
    }
    Error(err) -> {
      io.println("✗ Failed to create trailing stop (amount): " <> err)
    }
  }

  case orders.place_order(account_type, contract_id, trail_percent_sell) {
    Ok(msg_bytes) -> {
      io.println("✓ Trailing stop (percent) message created successfully")
      io.println("  This would be sent to IB TWS to place the order")
      io.println("")
    }
    Error(err) -> {
      io.println("✗ Failed to create trailing stop (percent): " <> err)
    }
  }

  // ========================================================================
  // Safety Check with LiveTradingReadOnly
  // ========================================================================
  io.println(repeat_char("=", 70))
  io.println("TRADING SAFETY CHECK")
  io.println(repeat_char("=", 70))
  io.println("")

  let read_only = connection.LiveTradingReadOnly
  case orders.place_order(read_only, contract_id, market_buy) {
    Ok(_) -> {
      io.println("✗ ERROR: Order should have been blocked!")
    }
    Error(err) -> {
      io.println("✓ LiveTradingReadOnly correctly blocks trading:")
      io.println("  " <> err)
    }
  }

  io.println("")
  io.println(repeat_char("=", 70))
  io.println("EXAMPLE COMPLETE")
  io.println(repeat_char("=", 70))
  io.println("")
  io.println("Summary:")
  io.println("✓ All order types work with Gleam's type system")
  io.println("✓ Invalid order configurations are impossible at compile time")
  io.println("✓ Trading safety enforced at type level")
  io.println("✓ Helper functions work with any order type")
  io.println("")
  io.println("Next Steps:")
  io.println("1. Connect to IB TWS using connection.connect_with_callback()")
  io.println("2. Send order messages using connection.send_bytes()")
  io.println("3. Handle order status updates via callback")
  io.println("4. Cancel orders using orders.cancel_order()")
  io.println("")
}

fn repeat_char(char: String, times: Int) -> String {
  list.range(0, times - 1)
  |> list.map(fn(_) { char })
  |> string.concat
}
