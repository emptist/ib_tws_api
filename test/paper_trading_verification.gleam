import connection
import gleam/bool
import gleam/io
import market_data
import orders

/// This test verifies that paper trading account on port 7497 allows trading
/// Run with: gleam run --module paper_trading_verification
pub fn main() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("PAPER TRADING ACCOUNT VERIFICATION TEST")
  io.println("Testing connection to port 7497 (Paper Trading)")
  io.println(repeat_char("=", 70))

  // Step 1: Create config for paper trading on port 7497
  io.println("\n1. Creating connection config for paper trading...")
  let config = connection.config("127.0.0.1", 7497, 1)
  io.println("   Host: " <> config.host)
  io.println("   Port: " <> int.to_string(config.port))
  io.println("   Client ID: " <> int.to_string(config.client_id))
  io.println("   Account Type: " <> account_type_to_string(config.account_type))

  // Step 2: Verify trading is allowed
  io.println("\n2. Checking trading permissions...")
  let trading_allowed = connection.is_trading_allowed(config.account_type)
  io.println("   Trading allowed: " <> bool.to_string(trading_allowed))

  case trading_allowed {
    True -> io.println("   ✓ PASS: Trading is ALLOWED for this account type")
    False -> {
      io.println("   ✗ FAIL: Trading is NOT ALLOWED for this account type")
      io.println("   This should not happen for paper trading!")
    }
  }

  // Step 3: Create test orders
  io.println("\n3. Creating test orders...")

  let market_order = orders.create_market_order(100, orders.BuyAction, 10)
  io.println(
    "   ✓ Market order created (ID: "
    <> int.to_string(orders.get_order_id(market_order))
    <> ")",
  )

  let limit_order = orders.create_limit_order(101, orders.SellAction, 5, 150.0)
  io.println(
    "   ✓ Limit order created (ID: "
    <> int.to_string(orders.get_order_id(limit_order))
    <> ")",
  )

  // Step 4: Test placing orders with paper trading account
  io.println("\n4. Testing order placement with paper trading account...")

  case orders.place_order(config.account_type, 12_345, market_order) {
    Ok(msg_bytes) -> {
      io.println("   ✓ PASS: Market order message created successfully")
      io.println(
        "   Message size: "
        <> int.to_string(bit_array.byte_size(msg_bytes))
        <> " bytes",
      )
    }
    Error(err) -> {
      io.println("   ✗ FAIL: Failed to create market order: " <> err)
    }
  }

  case orders.place_order(config.account_type, 12_345, limit_order) {
    Ok(msg_bytes) -> {
      io.println("   ✓ PASS: Limit order message created successfully")
      io.println(
        "   Message size: "
        <> int.to_string(bit_array.byte_size(msg_bytes))
        <> " bytes",
      )
    }
    Error(err) -> {
      io.println("   ✗ FAIL: Failed to create limit order: " <> err)
    }
  }

  // Step 5: Test with LiveTradingReadOnly (should fail)
  io.println(
    "\n5. Testing safety - LiveTradingReadOnly should reject orders...",
  )

  case
    orders.place_order(connection.LiveTradingReadOnly, 12_345, market_order)
  {
    Ok(_) -> {
      io.println(
        "   ✗ FAIL: Order should have been rejected for LiveTradingReadOnly!",
      )
    }
    Error(err) -> {
      io.println("   ✓ PASS: Order correctly rejected: " <> err)
    }
  }

  io.println("\n" <> repeat_char("=", 70))
  io.println("VERIFICATION COMPLETE")
  io.println("Paper trading account on port 7497 should allow trading ✓")
  io.println(repeat_char("=", 70) <> "\n")
}

fn repeat_char(char: String, times: Int) -> String {
  list.range(0, times - 1)
  |> list.map(fn(_) { char })
  |> string.concat
}

fn account_type_to_string(account_type: connection.AccountType) -> String {
  case account_type {
    connection.PaperTrading -> "PaperTrading"
    connection.LiveTrading -> "LiveTrading"
    connection.LiveTradingReadOnly -> "LiveTradingReadOnly"
  }
}

import gleam/bit_array
import gleam/int
import gleam/list
import gleam/string
