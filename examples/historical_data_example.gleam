import connection
import gleam/bit_array
import gleam/int
import gleam/io
import historical_data

/// Example: Requesting Historical Data for NVDA
/// This demonstrates how to request historical OHLCV data for backtesting
pub fn main() {
  io.println("=== Historical Data Example ===")
  io.println("")

  // Example 1: Request 1 day of 5-minute bars for NVDA
  io.println("Example 1: Request 1 day of 5-minute bars for NVDA")
  let hist_msg_1 =
    historical_data.request_historical_data(
      ticker_id: 200,
      contract_id: 12_345,
      exchange: "SMART",
      symbol: "NVDA",
      sec_type: "STK",
      currency: "USD",
      end_date_time: "20240101 16:00:00",
      // End at market close
      duration_str: "1 D",
      // 1 day of data
      bar_size: historical_data.FiveMinutes,
      what_to_show: historical_data.Trades,
      use_rth: True,
      format_date: 1,
      // Format: yyyymmdd HH:mm:ss
    )

  io.println(
    "Historical data request created: "
    <> int.to_string(bit_array.byte_size(hist_msg_1))
    <> " bytes",
  )
  io.println("  Request ID: 200")
  io.println("  Symbol: NVDA")
  io.println("  Bar Size: 5 minutes")
  io.println("  Duration: 1 day")
  io.println("  End Date: 20240101 16:00:00")
  io.println("")

  // Example 2: Request 1 week of daily bars for AAPL
  io.println("Example 2: Request 1 week of daily bars for AAPL")
  let hist_msg_2 =
    historical_data.request_historical_data(
      ticker_id: 201,
      contract_id: 54_321,
      exchange: "SMART",
      symbol: "AAPL",
      sec_type: "STK",
      currency: "USD",
      end_date_time: "20240101 16:00:00",
      duration_str: "1 W",
      // 1 week of data
      bar_size: historical_data.OneDay,
      what_to_show: historical_data.Trades,
      use_rth: True,
      format_date: 1,
    )

  io.println(
    "Historical data request created: "
    <> int.to_string(bit_array.byte_size(hist_msg_2))
    <> " bytes",
  )
  io.println("  Request ID: 201")
  io.println("  Symbol: AAPL")
  io.println("  Bar Size: 1 day")
  io.println("  Duration: 1 week")
  io.println("")

  // Example 3: Request 1 month of hourly bars for TSLA
  io.println("Example 3: Request 1 month of hourly bars for TSLA")
  let hist_msg_3 =
    historical_data.request_historical_data(
      ticker_id: 202,
      contract_id: 98_765,
      exchange: "SMART",
      symbol: "TSLA",
      sec_type: "STK",
      currency: "USD",
      end_date_time: "20240101 16:00:00",
      duration_str: "1 M",
      // 1 month of data
      bar_size: historical_data.OneHour,
      what_to_show: historical_data.Trades,
      use_rth: True,
      format_date: 1,
    )

  io.println(
    "Historical data request created: "
    <> int.to_string(bit_array.byte_size(hist_msg_3))
    <> " bytes",
  )
  io.println("  Request ID: 202")
  io.println("  Symbol: TSLA")
  io.println("  Bar Size: 1 hour")
  io.println("  Duration: 1 month")
  io.println("")

  // Example 4: Cancel historical data request
  io.println("Example 4: Cancel historical data request")
  let cancel_msg = historical_data.cancel_historical_data(ticker_id: 200)
  io.println(
    "Cancel historical data message created: "
    <> int.to_string(bit_array.byte_size(cancel_msg))
    <> " bytes",
  )
  io.println("  Request ID to cancel: 200")
  io.println("")

  // Example 5: Connect and send request (commented out - requires TWS running)
  io.println(
    "Example 5: Connect to TWS and send request (requires TWS running)",
  )
  io.println("  // To actually send the request:")
  io.println(
    "  // let assert Ok(conn) = connection.connect(\"127.0.0.1\", 7497)",
  )
  io.println("  // connection.send_bytes(conn, hist_msg_1)")
  io.println("  // The server will respond with historical bar data")
  io.println("")

  io.println("=== Historical Data Example Complete ===")
  io.println("")
  io.println("Note: Historical data is useful for:")
  io.println("  - Backtesting trading strategies")
  io.println("  - Analyzing price patterns")
  io.println("  - Calculating technical indicators")
  io.println("  - Building trading models")
}
