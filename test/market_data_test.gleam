import gleam/bit_array
import gleam/int
import gleam/io
import market_data

pub fn main() {
  io.println("=== Market Data Request Test ===")
  io.println("")

  // Create a stock contract
  io.println("1. Creating stock contract for AAPL")
  let apple_contract = market_data.create_stock_contract("AAPL")
  market_data.debug_contract(apple_contract)
  io.println("")

  // Generate market data request message
  io.println("2. Creating market data request for ticker ID 100")
  let request_msg = market_data.request_market_data(100, apple_contract)
  let msg_size = bit_array.byte_size(request_msg)
  io.println("Request message size: " <> int.to_string(msg_size) <> " bytes")
  io.println("")

  // Generate cancel market data message
  io.println("3. Creating cancel market data request for ticker ID 100")
  let cancel_msg = market_data.cancel_market_data(100)
  let cancel_size = bit_array.byte_size(cancel_msg)
  io.println("Cancel message size: " <> int.to_string(cancel_size) <> " bytes")
  io.println("")

  // Test with another contract
  io.println("4. Creating stock contract for TSLA")
  let tesla_contract = market_data.create_stock_contract("TSLA")
  market_data.debug_contract(tesla_contract)
  io.println("")

  io.println("5. Creating market data request for ticker ID 101")
  let _tesla_request = market_data.request_market_data(101, tesla_contract)
  io.println("Request created successfully")
  io.println("")

  io.println("=== Test Complete ===")
  io.println("")
  io.println(
    "Note: To actually send these requests to IB TWS, you would need to:",
  )
  io.println("1. Connect to IB TWS using connection.connect_with_callback()")
  io.println("2. Send the request message using connection.send()")
  io.println("3. Handle incoming market data messages via the callback")
}
