import connection
import gleam/bit_array
import gleam/float
import gleam/int
import gleam/io
import order_management

/// Example: Order Management
/// This demonstrates querying, modifying, and canceling orders
pub fn main() {
  io.println("=== Order Management Example ===")
  io.println("")

  // Example 1: Request all open orders
  io.println("Example 1: Request all open orders")
  let open_orders_msg = order_management.request_open_orders()
  io.println(
    "Request open orders message created: "
    <> int.to_string(bit_array.byte_size(open_orders_msg))
    <> " bytes",
  )
  io.println("  This will cause TWS to send all open order data")
  io.println("")

  // Example 2: Cancel a specific order
  io.println("Example 2: Cancel a specific order")
  let cancel_msg = order_management.cancel_order(order_id: 100)
  io.println(
    "Cancel order message created: "
    <> int.to_string(bit_array.byte_size(cancel_msg))
    <> " bytes",
  )
  io.println("  Order ID to cancel: 100")
  io.println("")

  // Example 3: Cancel all open orders
  io.println("Example 3: Cancel all open orders")
  let cancel_all_msg = order_management.cancel_all_orders()
  io.println(
    "Cancel all orders message created: "
    <> int.to_string(bit_array.byte_size(cancel_all_msg))
    <> " bytes",
  )
  io.println("  WARNING: This cancels ALL open orders!")
  io.println("")

  // Example 4: Modify an existing order
  io.println("Example 4: Modify an existing order")
  let modify_msg =
    order_management.modify_order(
      order_id: 101,
      contract_id: 12_345,
      action: "BUY",
      quantity: 15.0,
      order_type: "LMT",
      limit_price: 150.0,
      stop_price: 0.0,
    )
  io.println(
    "Modify order message created: "
    <> int.to_string(bit_array.byte_size(modify_msg))
    <> " bytes",
  )
  io.println("  Order ID: 101")
  io.println("  New Quantity: 15.0")
  io.println("  New Limit Price: 150.0")
  io.println("")

  // Example 5: Order status types
  io.println("Example 5: Order status types")
  io.println(
    "  PendingSubmit: "
    <> order_management.order_status_to_string(order_management.PendingSubmit),
  )
  io.println(
    "  Submitted: "
    <> order_management.order_status_to_string(order_management.Submitted),
  )
  io.println(
    "  Filled: "
    <> order_management.order_status_to_string(order_management.Filled),
  )
  io.println(
    "  Cancelled: "
    <> order_management.order_status_to_string(order_management.Cancelled),
  )
  io.println("")

  // Example 6: Connect and manage orders (commented out - requires TWS running)
  io.println(
    "Example 6: Connect to TWS and manage orders (requires TWS running)",
  )
  io.println("  // To actually manage orders:")
  io.println(
    "  // let assert Ok(conn) = connection.connect(\"127.0.0.1\", 7497)",
  )
  io.println("  // connection.send_bytes(conn, open_orders_msg)")
  io.println("  // The server will respond with open order data")
  io.println("  // connection.send_bytes(conn, cancel_msg)")
  io.println("  // This will cancel order 100")
  io.println("")

  io.println("=== Order Management Example Complete ===")
  io.println("")
  io.println("Note: Order management is useful for:")
  io.println("  - Monitoring active orders")
  io.println("  - Canceling unwanted orders")
  io.println("  - Modifying order parameters")
  io.println("  - Managing order lifecycle")
  io.println("  - Risk management")
}
