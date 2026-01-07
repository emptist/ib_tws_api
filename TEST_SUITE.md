# Test Suite Documentation

## Overview
This document describes the comprehensive test suite for the IB TWS API Gleam wrapper. The test suite is located in [`test/ib_tws_api_test.gleam`](test/ib_tws_api_test.gleam:1) and covers all major functionality of the API.

## Running Tests

To run the complete test suite:
```bash
gleam test
```

This will execute all test functions and display detailed output showing:
- Test names and descriptions
- Test results with checkmarks (✓) or crosses (✗)
- Summary information for each test
- Overall test results

## Test Coverage

### Test 1: Connection Configuration and Trading Safety
**File:** [`test/ib_tws_api_test.gleam:24`](test/ib_tws_api_test.gleam:24)

Tests the connection configuration system and trading safety features:

- **1.1:** Explicit port configuration
  - Creates config with explicit port (7497)
  - Verifies port and client ID are set correctly
  
- **1.2:** Paper trading configuration
  - Uses [`connection.config_with_account_type()`](src/connection.gleam:84) with [`connection.PaperTrading`](src/connection.gleam:37)
  - Verifies port 7497 is used for paper trading
  
- **1.3:** Live trading configuration
  - Uses [`connection.config_with_account_type()`](src/connection.gleam:84) with [`connection.LiveTradingReadOnly`](src/connection.gleam:39)
  - Verifies port 7496 is used for live trading
  
- **1.4:** Trading permissions
  - Tests [`connection.is_trading_allowed()`](src/connection.gleam:63)
  - Verifies paper trading allows trading
  - Verifies live trading read-only blocks trading

**Output:**
```
✓ Explicit port configuration works
✓ Paper trading config uses port 7497
✓ Live trading config uses port 7496
✓ Paper trading allows trading: True
✓ Live trading read-only blocks trading: False
```

---

### Test 2: Protocol Message Construction
**File:** [`test/ib_tws_api_test.gleam:76`](test/ib_tws_api_test.gleam:76)

Tests protocol message creation and parsing:

- **2.1:** START_API handshake message
  - Creates handshake using [`protocol.start_api_message()`](src/protocol.gleam:50)
  - Verifies message size is valid (≥ 9 bytes)
  
- **2.2:** Client ID message
  - Creates client ID message using [`protocol.client_id_message()`](src/protocol.gleam:102)
  - Verifies message size is exactly 4 bytes
  
- **2.3:** Server response parsing
  - Tests [`protocol.parse_server_response()`](src/protocol.gleam:122)
  - Verifies version number is extracted correctly
  - Verifies timestamp is extracted (if present)
  
- **2.4:** Control character filtering
  - Tests [`protocol.filter_control_characters()`](src/protocol.gleam:9)
  - Verifies control characters are removed from strings

**Output:**
```
✓ Handshake message created: 17 bytes
✓ Client ID message created: 4 bytes
✓ Server response parsed successfully
  Version: 20020260107
  Timestamp: No timestamp
✓ Control characters filtered successfully
```

---

### Test 3: Market Data Request Messages
**File:** [`test/ib_tws_api_test.gleam:132`](test/ib_tws_api_test.gleam:132)

Tests market data request functionality:

- **3.1:** Stock contract creation
  - Creates AAPL contract using [`market_data.create_stock_contract()`](src/market_data.gleam:52)
  - Verifies symbol, security type, exchange, and currency
  
- **3.2:** Market data request
  - Creates request using [`market_data.request_market_data()`](src/market_data.gleam:17)
  - Verifies message size is valid
  
- **3.3:** Cancel market data request
  - Creates cancel request using [`market_data.cancel_market_data()`](src/market_data.gleam:45)
  - Verifies message size is valid
  
- **3.4:** Multiple contracts
  - Creates TSLA contract to verify multiple symbols work

**Output:**
```
✓ AAPL contract created:
  Symbol: AAPL
  Type: STK
  Exchange: SMART
  Currency: USD
✓ Market data request created: 33 bytes
✓ Cancel market data request created: 10 bytes
✓ TSLA contract created
```

---

### Test 4: Order Placement Messages
**File:** [`test/ib_tws_api_test.gleam:185`](test/ib_tws_api_test.gleam:185)

Tests order placement functionality and trading safety:

- **4.1:** Market buy order
  - Creates market order using [`orders.create_market_order()`](src/orders.gleam:115)
  - Verifies order ID, quantity, type, and action
  
- **4.2:** Limit sell order
  - Creates limit order using [`orders.create_limit_order()`](src/orders.gleam:132)
  - Verifies order ID, quantity, limit price, type, and action
  
- **4.3:** Paper trading order placement
  - Places order with [`orders.place_order()`](src/orders.gleam:59) using [`connection.PaperTrading`](src/connection.gleam:37)
  - Verifies order message is created successfully
  
- **4.4:** Live trading order rejection
  - Attempts to place order with [`connection.LiveTrading`](src/connection.gleam:47)
  - Verifies order is rejected for safety
  
- **4.5:** Cancel order message
  - Creates cancel order using [`orders.cancel_order()`](src/orders.gleam:108)
  - Verifies message size is valid

**Output:**
```
✓ Market buy order created:
  Order ID: 100
  Quantity: 10
  Type: Market
  Action: BUY
✓ Limit sell order created:
  Order ID: 101
  Quantity: 5
  Limit Price: 150.0
  Type: Limit
  Action: SELL
✓ Order message created successfully: 46 bytes
✓ Order correctly rejected: Trading is not allowed with this account type...
✓ Cancel order message created: 10 bytes
```

---

### Test 5: Account Data Request Messages
**File:** [`test/ib_tws_api_test.gleam:263`](test/ib_tws_api_test.gleam:263)

Tests account data request functionality:

- **5.1:** Position request
  - Creates request using [`account_data.request_positions()`](src/account_data.gleam:11)
  - Verifies message size is valid (6 bytes)
  
- **5.2:** Cancel positions request
  - Creates cancel using [`account_data.cancel_positions()`](src/account_data.gleam:20)
  - Verifies message size is valid (2 bytes)
  
- **5.3:** Account summary with common tags
  - Creates request with 15 common tags using [`account_data.request_account_summary()`](src/account_data.gleam:121)
  - Verifies message size is valid (214 bytes)
  
- **5.4:** Account summary with specific tags
  - Creates request with 3 specific tags
  - Verifies message size is valid (58 bytes)
  
- **5.5:** Cancel account summary
  - Creates cancel using [`account_data.cancel_account_summary()`](src/account_data.gleam:156)
  - Verifies message size is valid (6 bytes)

**Output:**
```
✓ Position request created: 6 bytes
✓ Cancel positions message created: 2 bytes
✓ Account summary request created: 214 bytes
  Request ID: 100
  Group: All
  Tags: 15 tags
✓ Specific account summary request created: 58 bytes
✓ Cancel account summary message created: 6 bytes
```

---

### Test 6: Automatic Port Detection
**File:** [`test/ib_tws_api_test.gleam:340`](test/ib_tws_api_test.gleam:340)

Tests automatic port detection for IB TWS:

- **6.1:** Port detection
  - Uses [`connection.detect_ib_tws_port()`](src/connection.gleam:113) to find available port
  - Tests port 7497 (paper trading) first
  - Tests port 7496 (live trading) second
  - Verifies detected port is valid
  
- **6.2:** Auto-detect config
  - Uses [`connection.config_auto_detect()`](src/connection.gleam:118) to create config
  - Verifies config is created with detected port

**Output:**
```
[Port Detection] Starting detection for 127.0.0.1 with timeout 1s
[Port Detection] Checking port 7497 (Paper Trading)...
[Port Detection] Port 7497 available: false
[Port Detection] Checking port 7496 (Live Trading)...
[Port Detection] Port 7496 available: true
[Port Detection] Port 7496 (Live Trading) is available
[Port Detection] Returning 7496
✓ Port detection completed
  Detected port: 7496
  Account type: Live Trading
✓ Auto-detect config created:
  Host: 127.0.0.1
  Port: 7496
  Client ID: 1
```

---

### Test 7: FFI Functions
**File:** [`test/ib_tws_api_test.gleam:391`](test/ib_tws_api_test.gleam:391)

Tests JavaScript FFI functions:

- **7.1:** Timestamp generation
  - Tests [`connection.get_timestamp()`](src/connection.gleam:96)
  - Verifies timestamp is a non-empty string
  
- **7.2:** Client ID generation
  - Tests [`connection.generate_client_id()`](src/connection.gleam:101)
  - Verifies client ID is positive
  
- **7.3:** Sleep function
  - Tests [`connection.sleep()`](src/connection.gleam:106)
  - Verifies sleep completes without error

**Output:**
```
✓ Timestamp generated: 1767808992195
✓ Client ID generated: 846344
✓ Sleep function completed
```

---

### Test 8: Message Size Validation
**File:** [`test/ib_tws_api_test.gleam:422`](test/ib_tws_api_test.gleam:422)

Validates message sizes for various API messages:

- **8.1:** Handshake message
  - Verifies handshake message is ≥ 9 bytes
  - Expected: API\0 (4) + length (4) + version string (≥1)
  
- **8.2:** Client ID message
  - Verifies client ID message is exactly 4 bytes
  
- **8.3:** Market data request
  - Verifies market data request has valid size
  
- **8.4:** Order message
  - Verifies order message has valid size

**Output:**
```
✓ Handshake message size: 17 bytes (valid)
✓ Client ID message size: 4 bytes (valid)
✓ Market data request message size: 33 bytes (valid)
✓ Order message size: 46 bytes (valid)
```

---

### Test 9: Account Summary Tags
**File:** [`test/ib_tws_api_test.gleam:489`](test/ib_tws_api_test.gleam:489)

Tests account summary tag functionality:

- **9.1:** Tag conversion
  - Converts 5 tags to strings using [`account_data.account_summary_tag_to_string()`](src/account_data.gleam:83)
  - Verifies all tags convert correctly
  
- **9.2:** Common tags
  - Gets common tags using [`account_data.common_account_tags()`](src/account_data.gleam:164)
  - Verifies 15 common tags are returned

**Output:**
```
✓ All tags converted to strings:
  - AccountType
  - NetLiquidation
  - TotalCashBalance
  - SettledCash
  - BuyingPower
✓ Common account tags count: 15
```

---

### Test 10: Order Types and Actions
**File:** [`test/ib_tws_api_test.gleam:526`](test/ib_tws_api_test.gleam:526)

Tests different order types and actions:

- **10.1:** Order types
  - Tests [`orders.MarketOrder`](src/orders.gleam:9)
  - Tests [`orders.LimitOrder`](src/orders.gleam:10)
  
- **10.2:** Order actions
  - Tests [`orders.BuyAction`](src/orders.gleam:24)
  - Tests [`orders.SellAction`](src/orders.gleam:26)
  - Tests [`orders.ShortAction`](src/orders.gleam:28)

**Output:**
```
✓ Market order created
✓ Limit order created
✓ Buy action order created
✓ Sell action order created
✓ Short action order created
```

## Test Results Summary

All tests use the `gleeunit` testing framework and provide:

1. **Clear test names** - Each test has a descriptive name and section header
2. **Detailed output** - Shows what's being tested and the results
3. **Assertions** - Uses `should.equal()`, `should.be_true()`, `should.be_false()` for verification
4. **Pass/fail indicators** - ✓ for passing, ✗ for failing
5. **Summary sections** - Each test ends with a summary line

## Functionality Covered

The test suite covers all major functionality defined in the development plan:

✅ **Connection Management**
- Configuration with explicit ports
- Configuration with account types
- Automatic port detection
- Trading safety enforcement

✅ **Protocol Layer**
- Handshake message construction
- Client ID message construction
- Server response parsing
- Control character filtering

✅ **Market Data**
- Stock contract creation
- Market data requests
- Cancel market data requests

✅ **Order Management**
- Market order creation
- Limit order creation
- Order placement with safety checks
- Cancel order creation
- Different order actions (Buy, Sell, Short)

✅ **Account Data**
- Position requests
- Account summary requests
- Multiple account summary tags
- Cancel requests

✅ **FFI Integration**
- Timestamp generation
- Client ID generation
- Sleep functionality

✅ **Message Validation**
- Size validation for all message types
- Proper byte encoding

## Running Individual Tests

To run a specific test function:
```bash
gleam run -m ib_tws_api_test test_function_name
```

For example:
```bash
gleam run -m ib_tws_api_test connection_config_test
```

## Test Dependencies

The test suite depends on:
- `gleeunit` - Testing framework
- `gleeunit/should` - Assertion library
- All project modules (connection, protocol, market_data, orders, account_data)

## Notes

- Tests are designed to run independently
- No external connections are required for most tests
- Port detection test requires IB TWS to be running (optional)
- All tests provide detailed output for debugging
- Tests follow Gleam naming conventions (test functions are public)