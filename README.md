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
- **Trading**: ❌ Disabled by default (safety feature)

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

**Or manually specify the account type (auto-detects port):**

```gleam
// Configure connection with account type (auto-detects port)
let config = connection.config_with_account_type(
  "127.0.0.1",
  connection.PaperTrading,  // or LiveTradingReadOnly for live account
  connection.generate_client_id(),
)

// Connect with a callback to handle incoming messages
let result = connection.connect_with_callback(config, Some(fn(data) {
  io.println("Received: " <> data)
}))
```

**Or specify the port explicitly:**

```gleam
// Configure connection with explicit port
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

// Place the order (will fail if not paper trading)
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

### 5. Request Account Data

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
- `config(host, port, client_id)` - Create connection config with explicit port
- `config_with_account_type(host, account_type, client_id)` - Auto-detect port based on account type
- `config_auto_detect(host, client_id, timeout)` - Automatically detect which IB TWS port (7496 or 7497) is available
- `detect_ib_tws_port(host, timeout)` - Detect which IB TWS port is available (returns 0 if none, otherwise port number)
- `is_trading_allowed(account_type)` - Check if trading is allowed
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

### Messages Module

**Types:**
- `Message` - Parsed message variant (ErrorMsg, TickPrice, TickSize, OrderStatus, Position, AccountSummary, Unknown)
- `ErrorData` - Error message data
- `TickPriceData` - Tick price data
- `TickSizeData` - Tick size data
- `OrderStatusData` - Order status data
- `PositionData` - Position data
- `AccountSummaryData` - Account summary data

**Functions:**
- `parse_message(data)` - Parse message from received data

## Safety Features

### Type-Level Trading Safety

The library uses type-level safety to prevent accidental trading on live accounts:

```gleam
// This will succeed (paper trading allows trading)
orders.place_order(connection.PaperTrading, 101, 12345, order)

// This will fail (live trading disabled by default)
orders.place_order(connection.LiveTrading, 101, 12345, order)
// Error: "Trading is not allowed with this account type. Please use paper trading account for safety."
```

### Automatic Port Switching

```gleam
// Automatically uses port 7497 for paper trading
let config = connection.config_with_account_type("127.0.0.1", connection.PaperTrading, client_id)

// Automatically uses port 7496 for live trading
let config = connection.config_with_account_type("127.0.0.1", connection.LiveTradingReadOnly, client_id)
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

### Running Tests

```sh
# Run all tests
gleam test

# Run specific test module
gleam run --module orders_test
gleam run --module market_data_test
gleam run --module account_data_test
```

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
- [x] Order management (paper trading only)
- [x] Account information (positions and summaries)
- [ ] Historical data
- [ ] Real-time bars
- [ ] Advanced order types
- [ ] Portfolio management
- [ ] Error handling and resilience improvements

## Development Plan

See [`DEVELOPMENT_PLAN.md`](DEVELOPMENT_PLAN.md) for the complete development roadmap and implementation strategy.

## Safety Notes

⚠️ **Important Safety Guidelines**:

1. **Always use paper trading (port 7497) for development and testing**
2. **Live trading is disabled by default - use PaperTrading account type for safety**
3. **The library prevents trading on live accounts at the type level**
4. **Test thoroughly on paper account before using live trading**
5. **The library is in early development - use at your own risk**
6. **Never automate buy/sell operations on live account (port 7496)**

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
