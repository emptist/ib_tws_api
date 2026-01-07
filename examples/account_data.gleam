import account_data
import connection
import gleam/float
import gleam/int
import gleam/io
import gleam/option
import messages
import protocol

/// Account data retrieval example
/// Demonstrates how to request positions and account summaries
pub fn main() {
  io.println("=== Account Data Retrieval Example ===")
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
          Ok(messages.Position(pos)) -> {
            io.println(
              "Position - Account: "
              <> pos.account
              <> ", Symbol: "
              <> pos.symbol
              <> ", Qty: "
              <> float.to_string(pos.position)
              <> ", Avg Cost: "
              <> float.to_string(pos.avg_cost),
            )
          }
          Ok(messages.AccountSummary(acc)) -> {
            io.println(
              "Account Summary - Account: "
              <> acc.account_id
              <> ", Tag: "
              <> acc.tag
              <> ", Value: "
              <> acc.value
              <> ", Currency: "
              <> acc.currency,
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

      // Request positions
      io.println("Requesting all positions...")
      let positions_msg = account_data.request_positions()
      let _ = connection.send_bytes(conn, positions_msg)
      io.println("✓ Positions requested")
      io.println("")

      // Request account summary with common tags
      io.println("Requesting account summary...")
      let req_id = 200
      let tags = account_data.common_account_tags()
      let acc_summary_msg =
        account_data.request_account_summary(req_id, "All", tags)
      let _ = connection.send_bytes(conn, acc_summary_msg)
      io.println("✓ Account summary requested")
      io.println("")

      // Wait for data
      io.println("Receiving account data (5 seconds)...")
      io.println("You should see position and account summary messages below:")
      io.println("")
      connection.sleep(5000)

      // Cancel positions
      io.println("")
      io.println("Cancelling position updates...")
      let cancel_pos_msg = account_data.cancel_positions()
      let _ = connection.send_bytes(conn, cancel_pos_msg)
      io.println("✓ Positions cancelled")
      io.println("")

      // Cancel account summary
      io.println("Cancelling account summary...")
      let cancel_acc_msg = account_data.cancel_account_summary(req_id)
      let _ = connection.send_bytes(conn, cancel_acc_msg)
      io.println("✓ Account summary cancelled")
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
  io.println("Available Account Summary Tags:")
  io.println("- AccountType: Account type")
  io.println("- NetLiquidation: Net liquidation value")
  io.println("- TotalCashBalance: Total cash balance")
  io.println("- SettledCash: Settled cash")
  io.println("- AccruedCash: Accrued cash")
  io.println("- BuyingPower: Buying power")
  io.println("- EquityWithLoan: Equity with loan value")
  io.println("- GrossPosition: Gross position value")
  io.println("- RegTMargin: RegT margin")
  io.println("- SMA: Special Memorandum Account")
  io.println("- InitMarginReq: Initial margin requirement")
  io.println("- MaintMarginReq: Maintenance margin requirement")
  io.println("- AvailableFunds: Available funds")
  io.println("- ExcessLiquidity: Excess liquidity")
  io.println("- Cushion: Cushion percentage")
  io.println("- DayTradesRemaining: Day trades remaining")
  io.println("- Leverage: Leverage ratio")
  io.println("")
  io.println("Note: To see actual account data, make sure:")
  io.println("1. IB TWS or IB Gateway is running")
  io.println("2. API connections are enabled")
  io.println("3. You have positions in your account")
  io.println("4. Account has funds and activity")
}
