import account_data
import connection
import gleam/bit_array
import gleam/float
import gleam/int
import gleam/io
import gleam/option
import market_data
import messages
import protocol

/// Market data subscription example
/// Demonstrates how to request and receive market data for a stock
pub fn main() {
  io.println("=== Market Data Subscription Example ===")
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

      // Create a stock contract for Apple
      io.println("Creating contract for AAPL...")
      let aapl_contract =
        market_data.create_stock_contract(
          contract_id: 12_345,
          symbol: "AAPL",
          exchange: "SMART",
          currency: "USD",
        )
      io.println("✓ Contract created")
      io.println("")

      // Request market data
      io.println("Requesting market data for AAPL...")
      let ticker_id = 100
      let market_data_msg =
        market_data.request_market_data(ticker_id, aapl_contract)
      let _ = connection.send_bytes(conn, market_data_msg)
      io.println("✓ Market data requested")
      io.println("")

      // Wait for market data updates
      io.println("Receiving market data (10 seconds)...")
      io.println("You should see tick price and tick size messages below:")
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
  io.println("=== Example Complete ===")
  io.println("")
  io.println("Note: To see actual market data, make sure:")
  io.println("1. IB TWS or IB Gateway is running")
  io.println("2. API connections are enabled")
  io.println("3. Market data subscriptions are enabled in TWS")
  io.println("4. You have market data permissions for the requested symbols")
}
