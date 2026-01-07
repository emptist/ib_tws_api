import connection
import gleam/float
import gleam/int
import gleam/io
import gleam/option
import messages
import orders
import protocol

/// Order placement example (PAPER TRADING ONLY)
/// Demonstrates how to place and cancel orders safely
pub fn main() {
  io.println("=== Order Placement Example (Paper Trading Only) ===")
  io.println("")

  // Generate a unique client ID
  let client_id = connection.generate_client_id()

  // Create configuration with paper trading account
  let config =
    connection.config_with_account_type(
      "127.0.0.1",
      7497,
      connection.PaperTrading,
      client_id,
    )

  io.println("Connecting to IB TWS...")
  let result =
    connection.connect_with_callback(
      config,
      Some(fn(data) {
        // Parse incoming messages
        case messages.parse_message(data) {
          Ok(messages.ErrorMsg(err)) -> {
            io.println("Error: " <> err.error_message)
          }
          Ok(messages.OrderStatus(status)) -> {
            io.println(
              "Order Status - ID: "
              <> int.to_string(status.order_id)
              <> ", Status: "
              <> status.status
              <> ", Filled: "
              <> int.to_string(status.filled)
              <> ", Remaining: "
              <> int.to_string(status.remaining)
              <> ", Avg Price: "
              <> float.to_string(status.avg_fill_price),
            )
          }
          _other -> {
            // Ignore other messages for this example
            Nil
          }
        }
      }),
    )

  case result {
    Ok(conn) -> {
      io.println("✓ Connected successfully")
      io.println("")

      // Perform handshake
      io.println("Performing handshake...")
      let handshake = protocol.start_api_message()
      let _ = connection.send_bytes(conn, handshake)
      connection.sleep(1000)
      let client_id_msg = protocol.client_id_message(config.client_id)
      let _ = connection.send_bytes(conn, client_id_msg)
      io.println("✓ Handshake complete")
      io.println("")

      // Create a market buy order
      io.println("Creating market buy order...")
      let order_id = 101
      let contract_id = 12_345
      let order =
        orders.create_market_order(
          order_id: order_id,
          action: orders.BuyAction,
          quantity: 10,
        )
      io.println("✓ Order created")
      io.println("")

      // Demonstrate safety: Try to place order with paper trading (should succeed)
      io.println("Attempting to place order with paper trading account...")
      case
        orders.place_order(
          connection.PaperTrading,
          order_id,
          contract_id,
          order,
        )
      {
        Ok(msg_bytes) -> {
          io.println("✓ Order message created successfully")
          io.println("")
          io.println("Sending order to IB TWS...")
          let _ = connection.send_bytes(conn, msg_bytes)
          io.println("✓ Order sent")
          io.println("")
        }
        Error(err) -> {
          io.println("✗ Order failed: " <> err)
        }
      }

      // Wait for order status updates
      io.println("Waiting for order status updates (5 seconds)...")
      io.println("You should see order status messages below:")
      io.println("")
      connection.sleep(5000)

      // Cancel the order
      io.println("")
      io.println("Cancelling order...")
      let cancel_msg = orders.cancel_order(order_id)
      let _ = connection.send_bytes(conn, cancel_msg)
      io.println("✓ Cancel order sent")
      io.println("")

      // Wait for cancellation confirmation
      io.println("Waiting for cancellation confirmation (2 seconds)...")
      connection.sleep(2000)

      // Demonstrate safety: Try to place order with live trading (should fail)
      io.println("")
      io.println("=== Safety Demonstration ===")
      io.println("Attempting to place order with live trading account...")
      case
        orders.place_order(connection.LiveTrading, order_id, contract_id, order)
      {
        Ok(msg_bytes) -> {
          io.println("✗ Order should have been rejected!")
          io.println(
            "This is a safety issue - live trading should be disabled.",
          )
        }
        Error(err) -> {
          io.println("✓ Order correctly rejected: " <> err)
          io.println("Safety feature working as expected!")
        }
      }
      io.println("")

      // Close connection
      io.println("Closing connection...")
      let _ = connection.close(conn)
      io.println("✓ Connection closed")
    }
    Error(err) -> {
      io.println("✗ Connection failed: " <> err)
    }
  }

  io.println("")
  io.println("=== Example Complete ===")
  io.println("")
  io.println("⚠️  IMPORTANT SAFETY NOTES:")
  io.println("1. This example uses PAPER TRADING (port 7497)")
  io.println("2. No real money is at risk")
  io.println("3. The library prevents trading on live accounts by default")
  io.println("4. Always test thoroughly with paper trading before live trading")
  io.println("5. Never use live trading (port 7496) during development")
  io.println("")
  io.println("To see actual order execution:")
  io.println("1. Make sure IB TWS or IB Gateway is running")
  io.println("2. Ensure API connections are enabled")
  io.println("3. Have sufficient paper trading funds")
  io.println("4. The order will execute immediately for market orders")
}
