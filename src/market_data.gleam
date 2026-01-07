import gleam/io
import gleam/string

/// Contract type for IB TWS API
pub type Contract {
  Contract(
    contract_id: Int,
    symbol: String,
    security_type: String,
    exchange: String,
    currency: String,
  )
}

/// Request market data for a contract
/// Returns: message bytes to send to IB TWS
pub fn request_market_data(ticker_id: Int, contract: Contract) -> BitArray {
  // IB TWS API message code for requesting market data is 1
  // Format: version (int), ticker_id (int), contract fields...

  let symbol_len = string.length(contract.symbol)
  let sec_type_len = string.length(contract.security_type)
  let exchange_len = string.length(contract.exchange)
  let currency_len = string.length(contract.currency)

  // Build the complete message using bit array syntax
  <<
    1:16,
    9:32,
    ticker_id:32,
    contract.contract_id:32,
    symbol_len:8,
    contract.symbol:utf8,
    sec_type_len:8,
    contract.security_type:utf8,
    exchange_len:8,
    contract.exchange:utf8,
    currency_len:8,
    contract.currency:utf8,
  >>
}

/// Cancel market data request
/// Returns: message bytes to send to IB TWS
pub fn cancel_market_data(ticker_id: Int) -> BitArray {
  // IB TWS API message code for canceling market data is 2
  // Format: version (int), ticker_id (int)
  <<2:16, 1:32, ticker_id:32>>
}

/// Create a simple stock contract
pub fn create_stock_contract(symbol: String) -> Contract {
  Contract(
    contract_id: 0,
    // Will be assigned by TWS
    symbol: symbol,
    security_type: "STK",
    exchange: "SMART",
    currency: "USD",
  )
}

/// Debug print contract details
pub fn debug_contract(contract: Contract) {
  io.println("Contract:")
  io.println("  Symbol: " <> contract.symbol)
  io.println("  Type: " <> contract.security_type)
  io.println("  Exchange: " <> contract.exchange)
  io.println("  Currency: " <> contract.currency)
}
