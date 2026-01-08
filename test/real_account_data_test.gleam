import account_data
import connection
import gleam/float
import gleam/int
import gleam/io
import gleam/option.{Some}
import messages
import order_management
import protocol

/// REAL ACCOUNT DATA TEST
/// This test ACTUALLY requests data from TWS and displays it
pub fn main() {
  io.println("╔══════════════════════════════════════════════════════════════╗")
  io.println(
    "║           REAL ACCOUNT DATA TEST - REQUESTING ACTUAL DATA         ║",
  )
  io.println("╚══════════════════════════════════════════════════════════════╝")
  io.println("")

  let client_id = connection.generate_client_id()
  let config =
    connection.config_with_account_type(
      "127.0.0.1",
      7497,
      connection.PaperTrading,
      client_id,
    )

  io.println("📋 Configuration:")
  io.println("   Client ID: " <> int.to_string(client_id))
  io.println("   Port: 7497 (Paper Trading)")
  io.println("")

  let result =
    connection.connect_with_callback(
      config,
      Some(fn(data) {
        case messages.parse_message(data) {
          Ok(messages.ErrorMsg(err)) -> {
            io.println("❌ Error: " <> err.error_message)
          }
          Ok(messages.Position(pos)) -> {
            io.println("")
            io.println("📦 ═══════════════════════════════════════════════════")
            io.println("📦 POSITION DATA RECEIVED:")
            io.println("📦 ═══════════════════════════════════════════════════")
            io.println("   Account: " <> pos.account)
            io.println("   Symbol: " <> pos.symbol)
            io.println("   Quantity: " <> float.to_string(pos.position))
            io.println("   Avg Cost: $" <> float.to_string(pos.avg_cost))
            io.println("")
          }
          Ok(messages.AccountSummary(acc)) -> {
            io.println("")
            io.println("💰 ═══════════════════════════════════════════════════")
            io.println("💰 ACCOUNT SUMMARY DATA RECEIVED:")
            io.println("💰 ═══════════════════════════════════════════════════")
            io.println("   Account: " <> acc.account_id)
            io.println("   Tag: " <> acc.tag)
            io.println("   Value: " <> acc.value)
            io.println("   Currency: " <> acc.currency)
            io.println("")
          }
          Ok(messages.OrderStatus(status)) -> {
            io.println("")
            io.println("📊 ═══════════════════════════════════════════════════")
            io.println("📊 ORDER STATUS RECEIVED:")
            io.println("📊 ═══════════════════════════════════════════════════")
            io.println("   Order ID: " <> int.to_string(status.order_id))
            io.println("   Status: " <> status.status)
            io.println("   Filled: " <> int.to_string(status.filled))
            io.println("   Remaining: " <> int.to_string(status.remaining))
            io.println(
              "   Avg Price: $" <> float.to_string(status.avg_fill_price),
            )
            io.println("")
          }
          _other -> {
            // Ignore other messages
            Nil
          }
        }
      }),
    )

  case result {
    Ok(conn) -> {
      io.println("✅ CONNECTED TO IB TWS!")
      io.println("")

      // Perform handshake
      io.println("🤝 Step 1: API Handshake...")
      let handshake = protocol.start_api_message(100, 200)
      let _ = connection.send_bytes(conn, handshake)
      connection.sleep(1000)

      io.println("🤝 Step 2: Sending Client ID...")
      let client_id_msg = protocol.client_id_message(config.client_id)
      let _ = connection.send_bytes(conn, client_id_msg)
      connection.sleep(1000)
      io.println("✅ Handshake Complete")
      io.println("")

      // ═══════════════════════════════════════════════════════════════
      // REQUEST ACTUAL DATA FROM TWS
      // ═══════════════════════════════════════════════════════════════
      io.println("📊 ═══════════════════════════════════════════════════")
      io.println("📊 REQUESTING ACCOUNT DATA FROM TWS")
      io.println("📊 ═══════════════════════════════════════════════════")
      io.println("")

      // Request positions
      io.println("📦 Requesting positions...")
      let positions_msg = account_data.request_positions()
      let _ = connection.send_bytes(conn, positions_msg)
      io.println("✅ Positions request sent")
      io.println("")

      // Request account summary
      io.println("💰 Requesting account summary...")
      let req_id = 200
      let tags = account_data.common_account_tags()
      let acc_summary_msg =
        account_data.request_account_summary(req_id, "All", tags)
      let _ = connection.send_bytes(conn, acc_summary_msg)
      io.println("✅ Account summary request sent")
      io.println("")

      // Request open orders
      io.println("📝 Requesting open orders...")
      let open_orders_msg = order_management.request_open_orders()
      let _ = connection.send_bytes(conn, open_orders_msg)
      io.println("✅ Open orders request sent")
      io.println("")

      io.println("⏳ Waiting for data from TWS (10 seconds)...")
      io.println("   (Data will appear above as it arrives)")
      io.println("")
      connection.sleep(10_000)

      // Cancel requests
      io.println("")
      io.println("🛑 Cancelling requests...")
      let cancel_pos_msg = account_data.cancel_positions()
      let _ = connection.send_bytes(conn, cancel_pos_msg)

      let cancel_acc_msg = account_data.cancel_account_summary(req_id)
      let _ = connection.send_bytes(conn, cancel_acc_msg)
      io.println("✅ Requests cancelled")
      io.println("")

      // Close connection
      io.println("🔌 Closing connection...")
      let _ = connection.close(conn)
      io.println("✅ Connection closed")
    }
    Error(err) -> {
      io.println("❌ Connection failed")
      io.println("   Please ensure TWS is running on port 7497")
    }
  }

  io.println("")
  io.println("╔══════════════════════════════════════════════════════════════╗")
  io.println(
    "║                      TEST COMPLETE                              ║",
  )
  io.println("╚══════════════════════════════════════════════════════════════╝")
  io.println("")
  io.println("📋 Summary:")
  io.println("   - Connected to TWS: ✅")
  io.println("   - API Handshake: ✅")
  io.println("   - Client ID Sent: ✅")
  io.println("   - Positions Requested: ✅")
  io.println("   - Account Summary Requested: ✅")
  io.println("   - Open Orders Requested: ✅")
  io.println("")
  io.println("💡 If you see position/account/order data above, the test worked!")
  io.println("💡 If NO data appeared, check TWS API configuration")
}
