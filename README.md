# IB TWS API Wrapper for Gleam

A Gleam language wrapper for the Interactive Brokers Trader Workstation (TWS) API, targeting JavaScript.

## Overview

This library provides a Gleam interface to interact with the IB TWS API for paper trading and live trading operations. It follows a bottom-up development approach, implementing features incrementally as they are naturally needed.

## Status

✅ **Core Features Implemented** - Ready for paper trading development and testing.

Currently implemented:
- ✅ TCP connection with automatic port switching
- ✅ Automatic port detection (7496 or 7497)
- ✅ IB TWS V100+ protocol handshake
- ✅ Async message handling with callbacks
- ✅ Market data subscription
- ✅ Real-time bars with specific bar sizes (5 secs to 1 month)
- ✅ Order placement (paper trading only)
- ✅ Position and account data retrieval
- ✅ Message parsing for common message types
- ✅ Type-level trading safety

## Installation

```sh
gleam add ib_tws_api
```

## Configuration

The IB TWS API requires the TWS or IB Gateway application to be running with API connections enabled.

### Paper Trading (Development)
- **Port**: 7497
- **Use for**: Development, testing, and learning
- **Safety**: No real money at risk
- **Trading**: ✅ Allowed

### Live Trading (Production)
- **Port**: 7496
- **Use for**: Real trading operations
- **Warning**: Real money involved
- **Trading**: ✅ Allowed with LiveTrading, ❌ Disabled with LiveTradingReadOnly

## Quick Start

### 1. Connect to IB TWS (Automatic Port Detection)

The library can automatically detect which IB TWS port is available (7496 or 7497):

```gleam
import connection
import protocol
import gleam/option

// Automatically detect which port is available
case connection.config_auto_detect("127.0.0.1", connection.generate_client_id(), 1) {
  Ok(config) -> {
    io.println("✓ Connected to port: " <> int.to_string(config.port))
    
    // Connect with a callback to handle incoming messages
    let result = connection.connect_with_callback(config, Some(fn(data) {
      io.println("Received: " <> data)
    }))
  }
  Error(err) -> {
    io.println("❌ " <> err)
  }
}
```

**Or specify the port and account type explicitly:**

```gleam
// Configure connection with explicit port and account type
let config = connection.config_with_account_type(
  "127.0.0.1",
  7497,  // Port: 7497 for paper trading, 7496 for live trading
  connection.PaperTrading,  // Account type: PaperTrading, LiveTrading, or LiveTradingReadOnly
  connection.generate_client_id(),
)

// Connect with a callback to handle incoming messages
let result = connection.connect_with_callback(config, Some(fn(data) {
  io.println("Received: " <> data)
}))
```

**Or use the backward-compatible config (infers account type from port):**

```gleam
// Configure connection with explicit port (account type inferred)
// Port 7497 → PaperTrading (trading allowed)
// Port 7496 → LiveTrading (trading allowed)
let config = connection.config("127.0.0.1", 7497, connection.generate_client_id())

// Connect with a callback to handle incoming messages
let result = connection.connect_with_callback(config, Some(fn(data) {
  io.println("Received: " <> data)
}))
```

### 2. Perform Handshake

```gleam
import protocol

case result {
  Ok(conn) -> {
    // Send V100+ handshake
    let handshake = protocol.start_api_message()
    connection.send_bytes(conn, handshake)
    
    // Wait for server response, then send client ID
    // (In real code, you'd wait for the response event)
    connection.sleep(1000)
    
    let client_id_msg = protocol.client_id_message(config.client_id)
    connection.send_bytes(conn, client_id_msg)
  }
  Error(err) -> {
    io.println("Connection failed: " <> err)
  }
}
```

### 3. Request Market Data

```gleam
import market_data

// Create a stock contract
let contract = market_data.create_stock_contract(
  contract_id: 12345,
  symbol: "AAPL",
  exchange: "SMART",
  currency: "USD",
)

// Request market data
let market_data_msg = market_data.request_market_data(100, contract)
connection.send_bytes(conn, market_data_msg)

// Cancel when done
let cancel_msg = market_data.cancel_market_data(100)
connection.send_bytes(conn, cancel_msg)
```

### 4. Place an Order (Paper Trading Only)

```gleam
import orders

// Create a market buy order
let order = orders.create_market_order(
  order_id: 101,
  action: orders.BuyAction,
  quantity: 10,
)

// Place the order (will fail if LiveTradingReadOnly)
case orders.place_order(connection.PaperTrading, 101, 12345, order) {
  Ok(msg_bytes) -> {
    connection.send_bytes(conn, msg_bytes)
    io.println("Order placed successfully")
  }
  Error(err) -> {
    io.println("Order failed: " <> err)
  }
}

// Cancel the order
let cancel_msg = orders.cancel_order(101)
connection.send_bytes(conn, cancel_msg)
```

### 5. Request Real-Time Bars

```gleam
import real_time_bars

// Request real-time bars for NVDA with 1-minute bars
let ticker_id = 100
let contract_id = 12345
let exchange = "SMART"
let symbol = "NVDA"
let sec_type = "STK"
let currency = "USD"
let bar_size = real_time_bars.OneMinute
let what_to_show = real_time_bars.Trades
let use_rth = True

let bars_msg = real_time_bars.request_real_time_bars(
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

connection.send_bytes(conn, bars_msg)

// Cancel when done
let cancel_msg = real_time_bars.cancel_real_time_bars(ticker_id)
connection.send_bytes(conn, cancel_msg)
```

**Available Bar Sizes:**
- 5 secs, 10 secs, 15 secs, 30 secs
- 1 min, 2 mins, 3 mins, 5 mins, 10 mins
- 15 mins, 20 mins, 30 mins
- 1 hour, 2 hours, 3 hours, 4 hours, 8 hours
- 1 day, 1 week, 1 month

**Available Data Types:**
- Trades: Trade prices and volumes
- Bid: Bid prices
- Ask: Ask prices
- Midpoint: Midpoint of bid/ask
- Bid/Ask: Both bid and ask prices
- Historical Volatility: Historical volatility
- Implied Volatility: Implied volatility

### 6. Request Account Data

```gleam
import account_data

// Request all positions
let positions_msg = account_data.request_positions()
connection.send_bytes(conn, positions_msg)

// Request account summary
let tags = account_data.common_account_tags()
let acc_summary_msg = account_data.request_account_summary(
  req_id: 200,
  group_name: "All",
  tags: tags,
)
connection.send_bytes(conn, acc_summary_msg)

// Cancel when done
connection.send_bytes(conn, account_data.cancel_positions())
connection.send_bytes(conn, account_data.cancel_account_summary(200))
```

### 6. Parse Incoming Messages

```gleam
import messages

// In your callback
case messages.parse_message(data) {
  Ok(messages.ErrorMsg(err)) -> {
    io.println("Error: " <> err.error_message)
  }
  Ok(messages.TickPrice(tick)) -> {
    io.println("Price: " <> float.to_string(tick.price))
  }
  Ok(messages.OrderStatus(status)) -> {
    io.println("Order " <> int.to_string(status.order_id) <> " status: " <> status.status)
  }
  Ok(messages.Position(pos)) -> {
    io.println("Position: " <> pos.symbol <> " = " <> float.to_string(pos.position))
  }
  Ok(messages.AccountSummary(acc)) -> {
    io.println(acc.tag <> ": " <> acc.value)
  }
  Ok(messages.RealTimeBar(bar)) -> {
    io.println("Real-Time Bar - ID: " <> int.to_string(bar.req_id))
    io.println("  Open: " <> float.to_string(bar.open))
    io.println("  High: " <> float.to_string(bar.high))
    io.println("  Low: " <> float.to_string(bar.low))
    io.println("  Close: " <> float.to_string(bar.close))
    io.println("  Volume: " <> int.to_string(bar.volume))
  }
  _other -> {
    io.println("Unknown message")
  }
}
```

## API Reference

### Connection Module

**Types:**
- `AccountType` - PaperTrading, LiveTradingReadOnly, LiveTrading
- `ConnectionConfig` - Connection configuration
- `Connection` - Connection handle (opaque)
- `ConnectionError` - Connection error types
- `DataCallback` - Callback type for handling received data

**Functions:**
- `config(host, port, client_id)` - Create connection config with explicit port (account type inferred from port)
- `config_with_account_type(host, port, account_type, client_id)` - Create connection config with explicit port and account type
- `config_auto_detect(host, client_id, timeout)` - Automatically detect which IB TWS port (7496 or 7497) is available
- `detect_ib_tws_port(host, timeout)` - Detect which IB TWS port is available (returns 0 if none, otherwise port number)
- `is_trading_allowed(account_type)` - Check if trading is allowed (True for PaperTrading and LiveTrading, False for LiveTradingReadOnly)
- `connect(config)` - Connect to IB TWS
- `connect_with_callback(config, callback)` - Connect with data callback
- `send(conn, data)` - Send string data
- `send_bytes(conn, data)` - Send binary data
- `receive(conn)` - Receive data
- `close(conn)` - Close connection
- `generate_client_id()` - Generate time-based random client ID
- `sleep(milliseconds)` - Sleep for specified milliseconds

### Protocol Module

**Functions:**
- `start_api_message()` - Create V100+ handshake message
- `client_id_message(client_id)` - Create client ID message
- `parse_server_response(data)` - Parse server response (with control char filtering)

### Market Data Module

**Types:**
- `Contract` - Contract specification (id, symbol, security_type, exchange, currency)

**Functions:**
- `create_stock_contract(contract_id, symbol, exchange, currency)` - Create stock contract
- `request_market_data(ticker_id, contract)` - Request market data
- `cancel_market_data(ticker_id)` - Cancel market data subscription

### Orders Module

**Types:**
- `OrderType` - Market, Limit, Stop, StopLimit
- `OrderSide` - Buy, Sell, Short
- `OrderAction` - BuyAction, SellAction, ShortAction
- `TimeInForce` - Day, GTC, IOC, AON
- `Order` - Order specification

**Functions:**
- `create_market_order(order_id, action, quantity)` - Create market order
- `create_limit_order(order_id, action, quantity, price)` - Create limit order
- `place_order(account_type, order_id, contract_id, order)` - Place order (paper trading only)
- `cancel_order(order_id)` - Cancel order

### Account Data Module

**Types:**
- `AccountSummaryTag` - 25 different account summary tags (NetLiquidation, BuyingPower, etc.)

**Functions:**
- `request_positions()` - Request all positions
- `cancel_positions()` - Cancel position updates
- `request_account_summary(req_id, group_name, tags)` - Request account summary
- `cancel_account_summary(req_id)` - Cancel account summary
- `common_account_tags()` - Get common account summary tags
- `account_summary_tag_to_string(tag)` - Convert tag to string

### Real-Time Bars Module

**Types:**
- `BarSize` - Bar size (FiveSeconds to OneMonth)
- `WhatToShow` - Data type (Trades, Bid, Ask, Midpoint, etc.)
- `RealTimeBar` - Real-time bar data (req_id, time, open, high, low, close, volume, wap, count)

**Functions:**
- `request_real_time_bars(ticker_id, contract_id, exchange, symbol, sec_type, currency, bar_size, what_to_show, use_rth)` - Request real-time bars
- `cancel_real_time_bars(ticker_id)` - Cancel real-time bars subscription
- `debug_real_time_bars_request(...)` - Debug print request details

### Messages Module

**Types:**
- `Message` - Parsed message variant (ErrorMsg, TickPrice, TickSize, OrderStatus, Position, AccountSummary, RealTimeBar, Unknown)
- `ErrorData` - Error message data
- `TickPriceData` - Tick price data
- `TickSizeData` - Tick size data
- `OrderStatusData` - Order status data
- `PositionData` - Position data
- `AccountSummaryData` - Account summary data
- `RealTimeBarData` - Real-time bar data

**Functions:**
- `parse_message(data)` - Parse message from received data

## Safety Features

### Type-Level Trading Safety

The library uses type-level safety to control trading operations:

```gleam
// These will succeed (both allow trading)
orders.place_order(connection.PaperTrading, 101, 12345, order)
orders.place_order(connection.LiveTrading, 101, 12345, order)

// This will fail (read-only mode)
orders.place_order(connection.LiveTradingReadOnly, 101, 12345, order)
// Error: "Trading is not allowed with this account type. Please use paper trading account for safety."
```

**Important**: Trading is allowed for both `PaperTrading` and `LiveTrading` account types. Use `LiveTradingReadOnly` to disable trading for safety.

### Automatic Port Switching

```gleam
// Paper trading on port 7497 with trading enabled
let config = connection.config_with_account_type(
  "127.0.0.1",
  7497,
  connection.PaperTrading,
  client_id,
)

// Live trading on port 7496 with trading enabled
let config = connection.config_with_account_type(
  "127.0.0.1",
  7496,
  connection.LiveTrading,
  client_id,
)

// Live trading on port 7496 with trading disabled (read-only)
let config = connection.config_with_account_type(
  "127.0.0.1",
  7496,
  connection.LiveTradingReadOnly,
  client_id,
)
```

### Automatic Port Detection

The library can automatically detect which IB TWS port is available (7496 or 7497):

```gleam
// Automatically detect which port is available
case connection.config_auto_detect("127.0.0.1", client_id, 1) {
  Ok(config) -> {
    io.println("✓ Connected to port: " <> int.to_string(config.port))
    // Proceed with connection
  }
  Error(err) -> {
    io.println("❌ " <> err)
    // Handle error - neither port 7496 nor 7497 is available
  }
}
```

This is useful when switching between paper trading (port 7497) during the day and live trading (port 7496) at night.

### Unique Client IDs

```gleam
// Generate time-based random client ID to avoid conflicts
let client_id = connection.generate_client_id()
```

## Development

### Testing Philosophy

This project follows a **FACT DISCOVERY** approach to testing:

**Principles:**
1. **Connect to REAL TWS instance** - All tests connect to actual IB TWS API
2. **Send REAL protocol messages** - Tests use actual IB TWS protocol messages
3. **Discover FACTS about TWS behavior** - Tests learn how TWS really responds
4. **Use Result types honestly** - Tests report PASS/FAIL/UNKNOWN based on actual behavior
5. **NO fake data** - Tests never use hardcoded fake data to simulate responses
6. **NO cheating the compiler** - Tests don't just check return types or trivial properties

**Test Results:**
- **PASS** - Feature works as expected with real TWS
- **FAIL** - Feature does not work with real TWS
- **PARTIAL** - Feature partially works (e.g., connection succeeds but close fails)
- **UNKNOWN** - Feature appears to work but needs manual verification (e.g., callback receives data)

**Example Test Result:**
```gleam
TestResult(
  test_name: "TCP Connection to Paper Trading",
  status: "PASS",
  details: "Successfully established TCP connection to TWS on port 7497",
  fact_discovered: "TWS is listening on port 7497 for paper trading connections",
)
```

This approach ensures that **every test discovers a FACT about real TWS behavior**, not just checks that code compiles or returns the right type.

### Running Tests

```sh
# Run all tests
gleam test

# Tests will:
# - Connect to real TWS instance (paper trading on port 7497)
# - Send real protocol messages
# - Discover facts about TWS behavior
# - Report honest PASS/FAIL/UNKNOWN results
```

**Note:** Tests require TWS application to be running with API connections enabled on port 7497 (paper trading) or 7496 (live trading).

### Building

```sh
gleam build
```

### Project Structure

```
src/
├── connection.gleam       # TCP connection and account types
├── connection_ffi.mjs     # JavaScript FFI for Node.js integration
├── protocol.gleam        # IB TWS protocol message construction
├── market_data.gleam      # Market data requests
├── orders.gleam           # Order placement and management
├── account_data.gleam     # Position and account data requests
└── messages.gleam         # Message parsing

test/
├── ib_tws_api_test.gleam       # Basic connection tests
├── callback_handshake_test.gleam # Handshake with callback tests
├── market_data_test.gleam       # Market data tests
├── orders_test.gleam            # Order placement tests
└── account_data_test.gleam      # Account data tests
```

## API Coverage

The library is being developed incrementally. Currently implemented features:

- [x] Project setup and planning
- [x] Basic TCP connection with automatic port switching
- [x] Protocol handshake (IB TWS V100+)
- [x] Async message handling with callbacks
- [x] Message parsing for common message types
- [x] Market data subscription
- [x] Real-time bars with specific bar sizes
- [x] Order management (paper trading only)
- [x] Account information (positions and summaries)
- [ ] Historical data
- [ ] Advanced order types
- [ ] Portfolio management
- [ ] Error handling and resilience improvements

## Development Plan

See [`DEVELOPMENT_PLAN.md`](DEVELOPMENT_PLAN.md) for the complete development roadmap and implementation strategy.

## Safety Notes

⚠️ **Important Safety Guidelines**:

1. **Always use paper trading (port 7497) for development and testing**
2. **Use LiveTradingReadOnly account type to disable trading for safety**
3. **The library allows trading for both PaperTrading and LiveTrading account types**
4. **Test thoroughly on paper account before using live trading**
5. **The library is in early development - use at your own risk**
6. **Be cautious when automating buy/sell operations on live account (port 7496)**

## License

Apache-2.0

## Disclaimer

This software is provided as-is for educational and development purposes. Trading involves significant risk of loss. Always test thoroughly with paper trading before using real money.

## Resources

- [IB TWS API Documentation](https://www.interactivebrokers.com/campus/ibkr-api-page/twsapi-doc/)
- [IB TWS API GitHub](https://github.com/InteractiveBrokers/tws-api)
- [Gleam Language](https://gleam.run/)
- [Development Plan](DEVELOPMENT_PLAN.md)
- [Technical Documentation](TECHNICAL_NOTES.md)
