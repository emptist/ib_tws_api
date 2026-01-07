import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/string

/// Contract details query for obtaining contract information
/// Essential for trading - you need contract details before placing orders
/// Contract type
pub type ContractType {
  Stock
  Option
  Future
  Forex
  Bond
  ContractForDifference
  FuturesOption
  MutualFund
  Warrant
  Bag
  Crypto
}

/// Right type for options
pub type RightType {
  Put
  Call
}

/// Security ID type
pub type SecurityIdType {
  CUSIP
  ISIN
  RIC
  SEDOL
  BloombergTicker
}

/// Contract specification for querying details
pub type ContractSpec {
  ContractSpec(
    /// Contract type (stock, option, future, etc.)
    contract_type: ContractType,
    /// Symbol/ticker (e.g., "AAPL")
    symbol: String,
    /// Exchange (e.g., "SMART")
    exchange: String,
    /// Currency (e.g., "USD")
    currency: String,
    /// Expiration date for options/futures (format: YYYYMM)
    expiry: String,
    /// Strike price for options
    strike: Float,
    /// Right type for options (Put/Call)
    right: RightType,
    /// Multiplier for futures/options
    multiplier: String,
    /// Security ID type
    sec_id_type: SecurityIdType,
    /// Security ID value
    sec_id: String,
    /// Local symbol
    local_symbol: String,
  )
}

/// Contract details returned by IB TWS
pub type ContractDetails {
  ContractDetails(
    /// Contract ID (unique identifier)
    contract_id: Int,
    /// Symbol/ticker
    symbol: String,
    /// Contract type
    contract_type: String,
    /// Strike price
    strike: Float,
    /// Right type (Put/Call)
    right: String,
    /// Exchange
    exchange: String,
    /// Currency
    currency: String,
    /// Local symbol
    local_symbol: String,
    /// Market name
    market_name: String,
    /// Trading class
    trading_class: String,
    /// Contract description
    long_name: String,
    /// Industry
    industry: String,
    /// Category
    category: String,
    /// Subcategory
    subcategory: String,
    /// Minimum tick size
    min_tick: Float,
    /// Price magnifier
    price_magnifier: Int,
    /// Order types supported
    order_types: String,
    /// Valid exchanges
    valid_exchanges: String,
    /// Underlying contract ID (for options/futures)
    under_con_id: Int,
  )
}

/// Create default contract specification for a stock
pub fn create_stock_contract_spec(symbol: String) -> ContractSpec {
  ContractSpec(
    contract_type: Stock,
    symbol: symbol,
    exchange: "SMART",
    currency: "USD",
    expiry: "",
    strike: 0.0,
    right: Call,
    multiplier: "",
    sec_id_type: CUSIP,
    sec_id: "",
    local_symbol: "",
  )
}

/// Create contract specification with custom parameters
pub fn create_contract_spec(
  contract_type: ContractType,
  symbol: String,
  exchange: String,
  currency: String,
) -> ContractSpec {
  ContractSpec(
    contract_type: contract_type,
    symbol: symbol,
    exchange: exchange,
    currency: currency,
    expiry: "",
    strike: 0.0,
    right: Call,
    multiplier: "",
    sec_id_type: CUSIP,
    sec_id: "",
    local_symbol: "",
  )
}

/// Request contract details
/// req_id: Unique request ID to track the response
/// contract_spec: Contract specification to query
/// Returns: Message bytes to send to IB TWS
pub fn request_contract_details(
  req_id: Int,
  contract_spec: ContractSpec,
) -> BitArray {
  // Message format for REQ_CONTRACT_DATA (MsgCode 9):
  // version (int) + req_id (int) + contract_id (int) + symbol + sec_type + expiry + strike + right + multiplier + exchange + primary_exchange + currency + local_symbol + trading_class + include_expired + sec_id_type + sec_id

  let version = 9
  let contract_id = 0
  // Use 0 to query by symbol

  // Convert contract type to string
  let contract_type_str = contract_type_to_string(contract_spec.contract_type)

  // Convert right type to string
  let right_str = right_type_to_string(contract_spec.right)

  // Convert sec_id_type to string
  let sec_id_type_str = sec_id_type_to_string(contract_spec.sec_id_type)

  // Build message fields
  let fields = [
    int_to_field(version),
    int_to_field(req_id),
    int_to_field(contract_id),
    string_to_field(contract_spec.symbol),
    string_to_field(contract_type_str),
    string_to_field(contract_spec.expiry),
    float_to_field(contract_spec.strike),
    string_to_field(right_str),
    string_to_field(contract_spec.multiplier),
    string_to_field(contract_spec.exchange),
    string_to_field(""),
    // primary_exchange (empty for stocks)
    string_to_field(contract_spec.currency),
    string_to_field(contract_spec.local_symbol),
    string_to_field(""),
    // trading_class (empty for stocks)
    int_to_field(0),
    // include_expired (0 = false)
    string_to_field(sec_id_type_str),
    string_to_field(contract_spec.sec_id),
  ]

  // Add message code (9 for REQ_CONTRACT_DATA)
  let message_code = <<9>>

  // Combine fields with null separators
  let body =
    list.map(fields, fn(f) { f })
    |> list.intersperse(<<0>>)
    |> bit_array.concat

  bit_array.concat([message_code, body])
}

/// Cancel contract details request
/// req_id: The request ID from the original request
/// Returns: Message bytes to send to IB TWS
pub fn cancel_contract_details(req_id: Int) -> BitArray {
  // Message format for CANCEL_CONTRACT_DATA (MsgCode 10):
  // req_id (int)

  let fields = [
    int_to_field(req_id),
  ]

  // Add message code (10 for CANCEL_CONTRACT_DATA)
  let message_code = <<10>>

  // Combine fields with null separators
  let body =
    list.map(fields, fn(f) { f })
    |> list.intersperse(<<0>>)
    |> bit_array.concat

  bit_array.concat([message_code, body])
}

/// Parse contract details from message data
/// This is a simplified parser - actual implementation would handle all IB TWS message formats
pub fn parse_contract_details(data: BitArray) -> Result(ContractDetails, String) {
  // Simplified parsing - in production, this would parse the actual IB message format
  // For now, return an error as this is a placeholder
  Error(
    "Contract details parsing not yet implemented - requires full message handler integration",
  )
}

/// Convert contract type to string
pub fn contract_type_to_string(contract_type: ContractType) -> String {
  case contract_type {
    Stock -> "STK"
    Option -> "OPT"
    Future -> "FUT"
    Forex -> "CASH"
    Bond -> "BOND"
    ContractForDifference -> "CFD"
    FuturesOption -> "FOP"
    MutualFund -> "FUND"
    Warrant -> "WAR"
    Bag -> "BAG"
    Crypto -> "CRYPTO"
  }
}

/// Parse contract type from string
pub fn parse_contract_type(s: String) -> Result(ContractType, String) {
  case s {
    "STK" -> Ok(Stock)
    "OPT" -> Ok(Option)
    "FUT" -> Ok(Future)
    "CASH" -> Ok(Forex)
    "BOND" -> Ok(Bond)
    "CFD" -> Ok(ContractForDifference)
    "FOP" -> Ok(FuturesOption)
    "FUND" -> Ok(MutualFund)
    "WAR" -> Ok(Warrant)
    "BAG" -> Ok(Bag)
    "CRYPTO" -> Ok(Crypto)
    _ -> Error("Invalid contract type: " <> s)
  }
}

/// Convert right type to string
pub fn right_type_to_string(right: RightType) -> String {
  case right {
    Put -> "P"
    Call -> "C"
  }
}

/// Parse right type from string
pub fn parse_right_type(s: String) -> Result(RightType, String) {
  case s {
    "P" -> Ok(Put)
    "C" -> Ok(Call)
    _ -> Error("Invalid right type: " <> s)
  }
}

/// Convert security ID type to string
pub fn sec_id_type_to_string(sec_id_type: SecurityIdType) -> String {
  case sec_id_type {
    CUSIP -> "CUSIP"
    ISIN -> "ISIN"
    RIC -> "RIC"
    SEDOL -> "SEDOL"
    BloombergTicker -> "TICKER"
  }
}

/// Helper: Convert integer to IB protocol field (big-endian int)
fn int_to_field(n: Int) -> BitArray {
  <<n:32>>
}

/// Helper: Convert float to IB protocol field (big-endian double)
fn float_to_field(f: Float) -> BitArray {
  <<f:64-float>>
}

/// Helper: Convert string to IB protocol field
fn string_to_field(s: String) -> BitArray {
  bit_array.from_string(s)
}

/// Format contract details for display
pub fn format_contract_details(details: ContractDetails) -> String {
  "Contract ID: "
  <> int.to_string(details.contract_id)
  <> "\n"
  <> "Symbol: "
  <> details.symbol
  <> "\n"
  <> "Type: "
  <> details.contract_type
  <> "\n"
  <> "Exchange: "
  <> details.exchange
  <> "\n"
  <> "Currency: "
  <> details.currency
  <> "\n"
  <> "Name: "
  <> details.long_name
  <> "\n"
  <> "Industry: "
  <> details.industry
  <> "\n"
  <> "Min Tick: "
  <> float.to_string(details.min_tick)
  <> "\n"
  <> "Order Types: "
  <> details.order_types
  <> "\n"
  <> "Valid Exchanges: "
  <> details.valid_exchanges
}

/// Create a sample contract details for testing
pub fn create_sample_contract_details() -> ContractDetails {
  ContractDetails(
    contract_id: 12_345,
    symbol: "AAPL",
    contract_type: "STK",
    strike: 0.0,
    right: "",
    exchange: "SMART",
    currency: "USD",
    local_symbol: "AAPL",
    market_name: "NASDAQ",
    trading_class: "NMS",
    long_name: "Apple Inc",
    industry: "Technology",
    category: "Computers",
    subcategory: "Consumer Electronics",
    min_tick: 0.01,
    price_magnifier: 1,
    order_types: "ACTIVETIMETYPE,AO,ATOPEN,AVGCOST",
    valid_exchanges: "SMART,AMEX,NYSE,NASDAQ",
    under_con_id: 0,
  )
}
