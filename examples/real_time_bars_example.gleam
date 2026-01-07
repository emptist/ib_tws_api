import connection
import gleam/float
import gleam/int
import gleam/io
import gleam/option
import messages
import protocol
import real_time_bars

/// Real-Time Bars Example
/// 
/// This example demonstrates how to request real-time bars for a stock
/// with specific bar sizes (e.g., 1-minute bars for NVDA)
///
/// This addresses the user's request: "Can we get market data on 
/// specified stock such as NVDA, with specific bar size?"
pub fn main() {
  io.println("=== Real-Time Bars Example ===")
  io.println("")
  io.println("This example demonstrates real-time bars with specific bar sizes")
  io.println("Stock: NVDA")
  io.println("Bar Size: 1 minute")
  io.println("Data Type: Trades")
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
          Ok(messages.RealTimeBar(bar)) -> {
            // Handle real-time bar data
            io.println("")
            io.println("=== Real-Time Bar ===")
            io.println("Request ID: " <> int.to_string(bar.req_id))
            io.println("Time: " <> int.to_string(bar.time))
            io.println("Open: " <> float.to_string(bar.open))
            io.println("High: " <> float.to_string(bar.high))
            io.println("Low: " <> float.to_string(bar.low))
            io.println("Close: " <> float.to_string(bar.close))
            io.println("Volume: " <> int.to_string(bar.volume))
            io.println("WAP: " <> float.to_string(bar.wap))
            io.println("Count: " <> int.to_string(bar.count))
          }
          Ok(other) -> {
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

      // Request real-time bars for NVDA
      io.println("Requesting real-time bars for NVDA (1-minute bars)...")

      // NVDA contract details
      let ticker_id = 100
      let contract_id = 12_345
      let exchange = "SMART"
      let symbol = "NVDA"
      let sec_type = "STK"
      let currency = "USD"
      let bar_size = real_time_bars.OneMinute
      let what_to_show = real_time_bars.Trades
      let use_rth = True

      let bars_msg =
        real_time_bars.request_real_time_bars(
          ticker_id,
          contract_id,
          exchange,
          symbol,
          sec_type,
          currency,
          bar_size,
          what_to_show,
          use_rth,
        )

      let _ = connection.send_bytes(conn, bars_msg)
      io.println("✓ Real-time bars requested")
      io.println("")

      // Debug: Show request details
      real_time_bars.debug_real_time_bars_request(
        ticker_id,
        contract_id,
        exchange,
        symbol,
        sec_type,
        currency,
        bar_size,
        what_to_show,
        use_rth,
      )

      // Wait for real-time bar updates
      io.println("Receiving real-time bars (30 seconds)...")
      io.println("You should see OHLCV data (Open, High, Low, Close, Volume):")
      io.println("")
      connection.sleep(30_000)

      // Cancel real-time bars
      io.println("")
      io.println("Cancelling real-time bars...")
      let cancel_msg = real_time_bars.cancel_real_time_bars(ticker_id)
      let _ = connection.send_bytes(conn, cancel_msg)
      io.println("✓ Real-time bars cancelled")
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
  io.println("Available Bar Sizes:")
  io.println("- 5 secs, 10 secs, 15 secs, 30 secs")
  io.println("- 1 min, 2 mins, 3 mins, 5 mins, 10 mins")
  io.println("- 15 mins, 20 mins, 30 mins")
  io.println("- 1 hour, 2 hours, 3 hours, 4 hours, 8 hours")
  io.println("- 1 day, 1 week, 1 month")
  io.println("")
  io.println("Available Data Types:")
  io.println("- Trades: Trade prices and volumes")
  io.println("- Bid: Bid prices")
  io.println("- Ask: Ask prices")
  io.println("- Midpoint: Midpoint of bid/ask")
  io.println("- Bid/Ask: Both bid and ask prices")
  io.println("- Historical Volatility: Historical volatility")
  io.println("- Implied Volatility: Implied volatility")
  io.println("")
  io.println("Note: To see actual real-time bars, make sure:")
  io.println("1. IB TWS or IB Gateway is running")
  io.println("2. API connections are enabled")
  io.println("3. You have market data subscriptions for NVDA")
  io.println("4. Market is open (or use use_rth: False for pre-market)")
}
