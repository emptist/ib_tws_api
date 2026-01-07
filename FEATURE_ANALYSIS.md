# IB TWS API Wrapper - Feature Analysis and Gap Assessment

## Executive Summary

This document provides a comprehensive comparison between the implemented features and the development plan, identifying gaps and enhancement opportunities for the IB TWS API Gleam wrapper.

## ✅ Implemented Features

### 1. Core Infrastructure
- ✅ TCP connection to IB TWS API using Node.js sockets
- ✅ Automatic port switching based on account type (7496/7497)
- ✅ Type-level trading safety with three account types
- ✅ Time-based random client ID generation
- ✅ Async message handling with callbacks
- ✅ Control character filtering for protocol parsing
- ✅ Automatic port detection (checks 7497 then 7496)

### 2. Protocol Implementation
- ✅ IB TWS V100+ handshake protocol
- ✅ Client ID message transmission
- ✅ Server response parsing (version and timestamp)
- ✅ Message code detection and routing

### 3. Market Data (Basic)
- ✅ Market data subscription requests
- ✅ Market data cancellation
- ✅ Stock contract creation (symbol, exchange, currency)
- ✅ Tick price and tick size message parsing (placeholder)

### 4. Order Management (Basic)
- ✅ Market order placement
- ✅ Limit order placement
- ✅ Order cancellation
- ✅ Order status message parsing (placeholder)
- ✅ Paper trading safety enforcement
- ✅ Live trading protection (disabled by default)

### 5. Account Data
- ✅ Position data requests
- ✅ Account summary requests with 25 different tags
- ✅ Position and account summary message parsing (placeholder)
- ✅ Request cancellation

### 6. Documentation & Testing
- ✅ Comprehensive README with API reference
- ✅ Quick start guide (6-step tutorial)
- ✅ Technical documentation
- ✅ Development plan
- ✅ 4 practical examples (connection, market data, orders, account data)
- ✅ Comprehensive test suite (10 tests)

## ❌ Missing Features

### 1. Advanced Market Data

#### 1.1 Real-Time Bars (HIGH PRIORITY)
**Status:** NOT IMPLEMENTED

**Description:** Request real-time bar data with specific bar sizes and durations.

**User Need:** "Can we get market data on specified stock such as NVDA, with specific bar size?"

**IB TWS API Message:** `REQ_REAL_TIME_BARS` (code 50)

**Required Parameters:**
- `ticker_id`: Integer identifier for the request
- `contract`: Contract specification
- `bar_size`: Bar size (5 secs, 10 secs, 15 secs, 30 secs, 1 min, 2 mins, 3 mins, 5 mins, 10 mins, 15 mins, 20 mins, 30 mins, 1 hour, 2 hours, 3 hours, 4 hours, 8 hours, 1 day, 1 week, 1 month)
- `what_to_show`: Type of data (TRADES, BID, ASK, MIDPOINT)
- `use_rth`: Use regular trading hours (1 = yes, 0 = no)
- `real_time_bars_options`: Additional options

**Message Parsing Needed:**
- `REAL_TIME_BARS` (code 50): Parse bar data (time, open, high, low, close, volume, wap, count)

**Example Usage:**
```gleam
let nvda_contract = market_data.create_stock_contract(
  contract_id: 12345,
  symbol: "NVDA",
  exchange: "SMART",
  currency: "USD"
)

// Request 1-minute real-time bars for NVDA
let bar_request = real_time_bars.request_real_time_bars(
  ticker_id: 100,
  contract: nvda_contract,
  bar_size: real_time_bars.OneMinute,
  what_to_show: real_time_bars.Trades,
  use_rth: True
)

connection.send_bytes(conn, bar_request)
```

**Expected Output:**
```
Real-Time Bar - ID: 100, Time: 2026-01-07 10:30:00, 
Open: 145.50, High: 146.20, Low: 145.30, Close: 146.00, 
Volume: 1000000, WAP: 145.75
```

---

#### 1.2 Historical Data (HIGH PRIORITY)
**Status:** NOT IMPLEMENTED

**Description:** Request historical bar data for analysis and backtesting.

**IB TWS API Message:** `REQ_HISTORICAL_DATA` (code 20)

**Required Parameters:**
- `ticker_id`: Integer identifier for the request
- `contract`: Contract specification
- `end_date_time`: End date and time or empty string for current time
- `duration_str`: Duration string (1 D, 1 W, 1 M, 1 Y, etc.)
- `bar_size_setting`: Bar size (1 min, 5 min, 15 min, 1 day, 1 week, 1 month)
- `what_to_show`: Type of data (TRADES, MIDPOINT, BID, ASK, BID_ASK, etc.)
- `use_rth`: Use regular trading hours (1 = yes, 0 = no)
- `format_date`: Date format (1 = yyyymmdd {space} hh:mm:ss, 2 = yyyymmdd {space} hh:mm:ss, etc.)
- `keep_up_to_date`: Keep updated (1 = yes, 0 = no)
- `chart_options`: Additional chart options

**Message Parsing Needed:**
- `HISTORICAL_DATA` (code 20): Parse historical bar data (date, open, high, low, close, volume, count, wap, has_gaps)
- `HISTORICAL_DATA_END` (code 22): End of historical data marker

**Example Usage:**
```gleam
// Request 1-day historical data for NVDA for the last 30 days
let hist_request = historical_data.request_historical_data(
  ticker_id: 200,
  contract: nvda_contract,
  end_date_time: "",
  duration_str: "30 D",
  bar_size_setting: "1 day",
  what_to_show: "TRADES",
  use_rth: True,
  format_date: 1,
  keep_up_to_date: False
)

connection.send_bytes(conn, hist_request)
```

**Expected Output:**
```
Historical Bar - ID: 200, Date: 2026-01-07, 
Open: 145.50, High: 146.20, Low: 145.30, Close: 146.00, 
Volume: 1000000, Count: 50000, WAP: 145.75
...
Historical Data End - ID: 200
```

---

#### 1.3 Market Depth (Level 2 Data)
**Status:** NOT IMPLEMENTED

**Description:** Request market depth (Level 2) data showing order book depth.

**IB TWS API Message:** `REQ_MKT_DEPTH` (code 10)

**Required Parameters:**
- `ticker_id`: Integer identifier for the request
- `contract`: Contract specification
- `num_rows`: Number of rows to request
- `smart_depth`: Smart depth routing
- `mkt_depth_options`: Additional options

**Message Parsing Needed:**
- `MKT_DEPTH` (code 10): Parse depth data (position, operation, side, price, size)
- `MKT_DEPTH_L2` (code 11): Parse Level 2 depth data

---

### 2. Advanced Order Management

#### 2.1 Extended Order Types
**Status:** PARTIALLY IMPLEMENTED

**Current:** Market and Limit orders only

**Missing:**
- Stop orders
- Stop-Limit orders
- Trailing Stop orders
- Trailing Stop-Limit orders
- Market-on-Close orders
- Limit-on-Close orders
- Relative orders

**Required Implementation:**
```gleam
pub type OrderType {
  MarketOrder
  LimitOrder
  StopOrder
  StopLimitOrder
  TrailingStopOrder
  TrailingStopLimitOrder
  MarketOnCloseOrder
  LimitOnCloseOrder
  RelativeOrder
}
```

---

#### 2.2 Order Modifications
**Status:** NOT IMPLEMENTED

**Description:** Modify existing order parameters (price, quantity, etc.)

**IB TWS API Message:** `ORDER_STATUS` (code 9) for status, need `REQ_OPEN_ORDERS` to query

**Required:**
- Function to modify order: `modify_order(order_id, new_parameters)`
- Message parsing for order modifications

---

#### 2.3 Open Orders Query
**Status:** NOT IMPLEMENTED

**Description:** Query all open orders for the account.

**IB TWS API Message:** `REQ_OPEN_ORDERS` (code 9)

**Message Parsing Needed:**
- `OPEN_ORDER` (code 9): Parse open order details

**Example Usage:**
```gleam
connection.send_bytes(conn, orders.request_open_orders())
```

**Expected Output:**
```
Open Order - ID: 101, Action: BUY, Type: LMT, 
Quantity: 100, Limit Price: 145.50, Status: Submitted
```

---

### 3. Portfolio Management

#### 3.1 Portfolio Updates
**Status:** NOT IMPLEMENTED

**Description:** Real-time portfolio position updates.

**IB TWS API Message:** `REQ_POSITIONS` is implemented, but `PORTFOLIO_VALUE` is not

**Message Parsing Needed:**
- `PORTFOLIO_VALUE` (code 8): Parse portfolio value updates

---

#### 3.2 Account Updates
**Status:** NOT IMPLEMENTED

**Description:** Real-time account value updates (not just summary).

**IB TWS API Message:** `REQ_ACCOUNT_UPDATES` (code 6)

**Message Parsing Needed:**
- `ACCOUNT_UPDATE_TIME` (code 7): Parse account update timestamp
- `ACCOUNT_VALUE` (code 6): Parse account value updates

---

### 4. Contract Information

#### 4.1 Contract Details
**Status:** NOT IMPLEMENTED

**Description:** Request contract specification details.

**IB TWS API Message:** `REQ_CONTRACT_DETAILS` (code 8)

**Required Parameters:**
- `req_id`: Request ID
- `contract`: Contract specification

**Message Parsing Needed:**
- `CONTRACT_DETAILS` (code 10): Parse contract details (symbol, exchange, currency, etc.)

**Example Usage:**
```gleam
let contract_req = contract_data.request_contract_details(
  req_id: 300,
  contract: nvda_contract
)

connection.send_bytes(conn, contract_req)
```

**Expected Output:**
```
Contract Details - Symbol: NVDA, Exchange: SMART, 
Currency: USD, SecType: STK, ConID: 123456789
```

---

### 5. Scanner Features

#### 5.1 Market Scanner
**Status:** NOT IMPLEMENTED

**Description:** Scan market for stocks matching specific criteria.

**IB TWS API Message:** `REQ_SCANNER_SUBSCRIPTION` (code 12)

**Required Parameters:**
- `req_id`: Request ID
- `subscription`: Scanner subscription parameters
- `scanner_subscription_options`: Additional options

**Message Parsing Needed:**
- `SCANNER_DATA` (code 12): Parse scanner results
- `SCANNER_PARAMETERS` (code 13): Parse scanner parameters

---

### 6. News and Research

#### 6.1 News Bulletins
**Status:** NOT IMPLEMENTED

**Description:** Request news bulletins for subscribed stocks.

**IB TWS API Message:** `REQ_NEWS_BULLETINS` (code 11)

---

#### 6.2 News Article Data
**Status:** NOT IMPLEMENTED

**Description:** Request detailed news article data.

**IB TWS API Message:** `REQ_NEWS_ARTICLE` (code 12)

---

### 7. Fundamental Data

#### 7.1 Fundamentals
**Status:** NOT IMPLEMENTED

**Description:** Request fundamental data for a stock (PE ratio, market cap, etc.).

**IB TWS API Message:** `REQ_FUNDAMENTAL_DATA` (code 14)

---

### 8. Execution and Commission

#### 8.1 Execution Details
**Status:** NOT IMPLEMENTED

**Description:** Request execution details for orders.

**IB TWS API Message:** `REQ_EXECUTIONS` (code 7)

**Message Parsing Needed:**
- `EXECUTION_DATA` (code 9): Parse execution details

---

#### 8.2 Commission Reports
**Status:** NOT IMPLEMENTED

**Description:** Request commission reports.

**IB TWS API Message:** `REQ_COMMISSION_REPORT` (code 12)

---

### 9. Enhanced Message Parsing

**Status:** PARTIALLY IMPLEMENTED (placeholders only)

**Issue:** Current message parsing only returns placeholder values (0, "Unknown", etc.)

**Required:**
- Parse actual tick price data from binary messages
- Parse actual tick size data from binary messages
- Parse actual order status data from binary messages
- Parse actual position data from binary messages
- Parse actual account summary data from binary messages
- Parse error messages with proper error codes and IDs

**Current Implementation:**
```gleam
// Current: Returns placeholder values
Ok(TickPrice(TickPriceData(
  ticker_id: 0,
  tick_type: 0,
  price: 0.0,
  size: 0,
)))
```

**Required Implementation:**
```gleam
// Parse actual values from binary message
Ok(TickPrice(TickPriceData(
  ticker_id: parsed_ticker_id,
  tick_type: parsed_tick_type,
  price: parsed_price,
  size: parsed_size,
)))
```

---

## 🎯 Priority Recommendations

### Phase 6: High Priority (Immediate Implementation)

1. **Real-Time Bars** - User explicitly requested this feature
   - Implement `REQ_REAL_TIME_BARS` message construction
   - Implement `REAL_TIME_BARS` message parsing
   - Add example with NVDA and specific bar sizes
   - **Estimated Effort:** 4-6 hours

2. **Historical Data** - Essential for analysis and backtesting
   - Implement `REQ_HISTORICAL_DATA` message construction
   - Implement `HISTORICAL_DATA` and `HISTORICAL_DATA_END` parsing
   - Add example for historical data retrieval
   - **Estimated Effort:** 4-6 hours

3. **Enhanced Message Parsing** - Critical for all features
   - Parse actual values from binary messages
   - Implement proper field extraction
   - Add comprehensive error handling
   - **Estimated Effort:** 8-10 hours

### Phase 7: Medium Priority (Near-Term)

4. **Open Orders Query** - Important for order management
   - Implement `REQ_OPEN_ORDERS` message
   - Parse `OPEN_ORDER` messages
   - **Estimated Effort:** 2-3 hours

5. **Order Modifications** - Essential for trading operations
   - Implement `modify_order()` function
   - Handle order modification responses
   - **Estimated Effort:** 2-3 hours

6. **Extended Order Types** - Expand trading capabilities
   - Add Stop, Stop-Limit, Trailing orders
   - Implement order type constructors
   - **Estimated Effort:** 3-4 hours

### Phase 8: Low Priority (Future Enhancement)

7. **Market Depth** - Advanced market data
   - Implement `REQ_MKT_DEPTH` message
   - Parse depth data
   - **Estimated Effort:** 4-5 hours

8. **Contract Details** - Useful for contract validation
   - Implement `REQ_CONTRACT_DETAILS` message
   - Parse contract details
   - **Estimated Effort:** 2-3 hours

9. **Scanner Features** - Market scanning
   - Implement scanner subscription
   - Parse scanner results
   - **Estimated Effort:** 6-8 hours

10. **Portfolio and Account Updates** - Real-time updates
    - Implement portfolio value updates
    - Implement account value updates
    - **Estimated Effort:** 3-4 hours

### Phase 9: Advanced Features (Future)

11. **News and Research**
    - **Estimated Effort:** 4-6 hours

12. **Fundamental Data**
    - **Estimated Effort:** 2-3 hours

13. **Execution and Commission Reports**
    - **Estimated Effort:** 2-3 hours

---

## 📊 Implementation Status Summary

| Feature Category | Implemented | Partial | Missing | Priority |
|-----------------|-------------|---------|---------|----------|
| Core Infrastructure | ✅ 7/7 | 0 | 0 | - |
| Protocol | ✅ 4/4 | 0 | 0 | - |
| Market Data | 2/5 | 0 | 3 | HIGH |
| Order Management | 2/7 | 0 | 5 | HIGH |
| Account Data | 2/3 | 0 | 1 | MEDIUM |
| Contract Info | 0/1 | 0 | 1 | LOW |
| Scanner | 0/1 | 0 | 1 | LOW |
| News & Research | 0/2 | 0 | 2 | LOW |
| Fundamental Data | 0/1 | 0 | 1 | LOW |
| Execution & Reports | 0/2 | 0 | 2 | LOW |
| Message Parsing | 0/6 | 6 | 0 | CRITICAL |

**Total:** 17/40 features fully implemented (42.5%)

---

## 🚀 Recommended Next Steps

### Step 1: Enhance Message Parsing (CRITICAL)
Before adding new features, we must fix the message parsing to return actual values instead of placeholders.

**Tasks:**
1. Implement proper binary message parsing for all message types
2. Extract actual field values from byte arrays
3. Add comprehensive error handling
4. Test with real IB TWS data

### Step 2: Implement Real-Time Bars (HIGH PRIORITY)
This directly addresses the user's request for market data with specific bar sizes.

**Tasks:**
1. Create `real_time_bars.gleam` module
2. Implement `request_real_time_bars()` function
3. Implement `cancel_real_time_bars()` function
4. Add bar size type definitions
5. Implement `REAL_TIME_BARS` message parsing
6. Create example with NVDA
7. Update documentation

### Step 3: Implement Historical Data (HIGH PRIORITY)
Essential for analysis and backtesting.

**Tasks:**
1. Create `historical_data.gleam` module
2. Implement `request_historical_data()` function
3. Implement `cancel_historical_data()` function
4. Add duration and bar size type definitions
5. Implement `HISTORICAL_DATA` and `HISTORICAL_DATA_END` parsing
6. Create example
7. Update documentation

### Step 4: Implement Open Orders Query (MEDIUM)
Important for order management workflow.

**Tasks:**
1. Add `request_open_orders()` to `orders.gleam`
2. Implement `OPEN_ORDER` message parsing
3. Create example
4. Update documentation

### Step 5: Implement Order Modifications (MEDIUM)
Essential for trading operations.

**Tasks:**
1. Add `modify_order()` function to `orders.gleam`
2. Handle modification responses
3. Create example
4. Update documentation

---

## 📝 Development Notes

### Message Parsing Complexity
The current placeholder parsing is insufficient. Proper binary message parsing requires:
1. Understanding IB TWS binary protocol format
2. Handling variable-length fields
3. Managing endianness (big-endian for integers)
4. Parsing string fields with length prefixes
5. Handling optional fields

### Protocol Documentation Reference
- IB TWS API Documentation: https://www.interactivebrokers.com/campus/ibkr-api-page/twsapi-doc/
- IB API Message Codes: Available in IB documentation
- Protocol Version: Currently using v100..200

### Testing Strategy
For each new feature:
1. Unit tests for message construction
2. Unit tests for message parsing
3. Integration tests with paper trading account
4. Examples demonstrating usage
5. Documentation updates

---

## 🎓 Conclusion

The IB TWS API wrapper has a solid foundation with 42.5% of planned features implemented. The core infrastructure, protocol handling, and basic market data/order/account functionality are working well.

**Critical Gap:** Message parsing needs enhancement to return actual values instead of placeholders.

**High Priority:** Real-time bars and historical data should be implemented next to meet user requirements for market data with specific bar sizes.

**Next Steps:** Follow the recommended implementation plan, starting with message parsing enhancement, then real-time bars, followed by historical data.

The bottom-up development approach has been successful so far. Continue this methodology by implementing features as they become naturally needed in real-world usage scenarios.