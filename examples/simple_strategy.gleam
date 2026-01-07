import account_data
import connection
import gleam/float
import gleam/int
import gleam/io
import gleam/option
import market_data
import messages
import protocol

/// Simple Moving Average Crossover Strategy
/// 
/// This example demonstrates a simple trading strategy that:
/// 1. Monitors a stock's price in real-time
/// 2. Calculates a simple moving average
/// 3. Generates buy/sell signals when price crosses the average
/// 
/// NATURAL NEED: This strategy requires real-time bars to calculate
/// moving averages over time. Single tick prices are insufficient.
pub fn main() {
  io.println("=== Simple Moving Average Crossover Strategy ===")
  io.println("")
  io.println("Strategy: Buy when price crosses above SMA, Sell when below")
  io.println("Symbol: NVDA")
  io.println("Bar Size: 1 minute")
  io.println("SMA Period: 5 bars")
  io.println("")

  // Generate a unique client ID
  let client_id = connection.generate_client_id()

  // Create configuration with paper trading account
  let config =
    connection.config_with_account_type(
      "127.0.0.1",
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
          Ok(messages.TickPrice(tick)) -> {
            // NOTE: Tick prices are not enough for moving averages
            // We need real-time bars with OHLCV data
            io.println(
              "Tick Price - ID: "
              <> int.to_string(tick.ticker_id)
              <> ", Type: "
              <> int.to_string(tick.tick_type)
              <> ", Price: "
              <> float.to_string(tick.price),
            )
          }
          Ok(messages.TickSize(tick)) -> {
            io.println(
              "Tick Size - ID: "
              <> int.to_string(tick.ticker_id)
              <> ", Type: "
              <> int.to_string(tick.tick_type)
              <> ", Size: "
              <> int.to_string(tick.size),
            )
          }
          // TODO: We need to handle RealTimeBar messages here
          // This will be implemented when we add real-time bars support
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

      // Create a stock contract for NVDA
      io.println("Creating contract for NVDA...")
      let nvda_contract =
        market_data.create_stock_contract(
          contract_id: 12_345,
          symbol: "NVDA",
          exchange: "SMART",
          currency: "USD",
        )
      io.println("✓ Contract created")
      io.println("")

      // TODO: Request real-time bars for NVDA
      // This feature is NOT YET IMPLEMENTED
      // When implemented, we'll use:
      // let bar_request = real_time_bars.request_real_time_bars(
      //   ticker_id: 100,
      //   contract: nvda_contract,
      //   bar_size: real_time_bars.OneMinute,
      //   what_to_show: real_time_bars.Trades,
      //   use_rth: True
      // )
      // connection.send_bytes(conn, bar_request)

      io.println("⚠️  REAL-TIME BARS NOT YET IMPLEMENTED")
      io.println("")
      io.println("To implement this strategy, we need:")
      io.println("1. Real-time bars with 1-minute bar size")
      io.println("2. OHLCV data (Open, High, Low, Close, Volume)")
      io.println("3. Moving average calculation over multiple bars")
      io.println("4. Signal generation when price crosses SMA")
      io.println("")

      // For now, let's request basic market data to show what we have
      io.println("Requesting basic market data (tick prices)...")
      let ticker_id = 100
      let market_data_msg =
        market_data.request_market_data(ticker_id, nvda_contract)
      let _ = connection.send_bytes(conn, market_data_msg)
      io.println("✓ Market data requested")
      io.println("")

      // Wait for market data updates
      io.println("Receiving market data (10 seconds)...")
      io.println("Note: This shows tick prices, NOT bar data")
      io.println("For moving averages, we need real-time bars")
      io.println("")
      connection.sleep(10_000)

      // Cancel market data subscription
      io.println("")
      io.println("Cancelling market data subscription...")
      let cancel_msg = market_data.cancel_market_data(ticker_id)
      let _ = connection.send_bytes(conn, cancel_msg)
      io.println("✓ Market data cancelled")
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
  io.println("=== Strategy Analysis ===")
  io.println("")
  io.println("Current Limitations:")
  io.println("1. ❌ Cannot request real-time bars with specific bar sizes")
  io.println("2. ❌ Cannot get OHLCV data (Open, High, Low, Close, Volume)")
  io.println("3. ❌ Cannot calculate moving averages over time")
  io.println("4. ❌ Cannot implement time-based trading strategies")
  io.println("")
  io.println("What We Have:")
  io.println("1. ✅ Can get individual tick prices")
  io.println("2. ✅ Can place orders (paper trading)")
  io.println("3. ✅ Can monitor positions and account data")
  io.println("")
  io.println("Next Natural Step:")
  io.println("→ Implement real-time bars to enable time-based strategies")
  io.println("→ This will naturally lead to moving average calculations")
  io.println("→ Then we can implement crossover strategies")
  io.println("")
  io.println("=== Example Complete ===")
}
