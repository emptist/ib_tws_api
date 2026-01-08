# IB TWS API Protocol Specification

## Overview

The IB TWS API uses a custom binary/text hybrid protocol called "V100+". This specification documents the exact protocol format as implemented in the reference TypeScript implementation.

---

## Protocol Versions

### V100+ Protocol (Current Standard)
- Used by TWS/Gateway versions 38+
- Binary length prefix + NULL-separated text tokens
- Bidirectional communication over TCP socket

### Legacy Protocol (Pre-V100)
- Fixed-width fields
- Deprecated
- Not covered in this spec

---

## Connection Lifecycle

### Phase 1: TCP Connection
```
Client → Server: TCP SYN
Server → Client: TCP SYN/ACK
Client → Server: TCP ACK
[Connection established]
```

### Phase 2: Handshake
```
Client → Server: "API\0" + [4-byte length] + version_string
Server → Client: [server_version, server_time, ...] (NULL-separated)
```

### Phase 3: API Initialization
```
Client → Server: START_API message (NULL-separated tokens)
Server → Client: nextValidId event (connection ready)
```

### Phase 4: Active Communication
```
Client → Server: API requests (encoded messages)
Server → Client: Event responses (decoded messages)
```

---

## Message Format

### V100+ Message Structure

```
[4-byte length prefix (big-endian)][message data][NULL terminator]
```

#### Length Prefix
- Size: 4 bytes
- Encoding: Big-endian unsigned integer
- Value: Length of message data in bytes
- Example: Message length 100 → `0x00 0x00 0x00 0x64`

#### Message Data
- Format: NULL-separated tokens (0x00 byte)
- Each token is a string representation of a value
- Empty tokens represent NULL/undefined values

#### NULL Terminator
- Single 0x00 byte at end of message
- Marks end of message boundary

### Example Message Encoding

**Request: REQ_POSITIONS (message ID 61, version 1)**

```
Token Array: [61, 1]
Message Data: "61\01\0"
Length: 5 bytes
Full Message: [0x00 0x00 0x00 0x05] + "61\01\0"
```

**Decoded:**
```
Length: 5
Token 1: "61" (message ID)
Token 2: "1" (version)
```

---

## Handshake Protocol

### Client Handshake Message

**Format:**
```
"API\0" + [4-byte big-endian length] + version_string
```

**Components:**
1. `"API\0"` - 4 ASCII bytes + NULL terminator
2. `[4-byte length]` - Length of version string
3. `version_string` - Version in format "v{MAX}.{MIN}"

**Example:**
```
Version: MAX=176, MIN=38
Version String: "v176.38"
Length: 7 bytes

Full Message:
  "API\0" + [0x00 0x00 0x00 0x07] + "v176.38"
  = "API" + 0x00 + [0x00, 0x00, 0x00, 0x07] + "v176.38"
```

### Server Handshake Response

**Format:**
```
NULL-separated tokens (no length prefix for handshake response)
```

**Tokens:**
1. Server version (string)
2. Server time (string, Unix timestamp)
3. Additional tokens (varies by server version)

**Example Response:**
```
"176\01736457890\0"
```

**Decoded:**
```
Token 1: "176" (server version)
Token 2: "1736457890" (server time)
```

---

## START_API Message

### Purpose
Initializes the API connection after successful handshake.

### Format
```
NULL-separated token array
```

### Tokens
1. Message ID: `71` (OUT_MSG_ID.START_API)
2. Version: `2`
3. Client ID: Integer (user-defined)
4. Optional Capabilities: Empty string ""

### Example
```
Client ID: 1
Tokens: [71, 2, 1, ""]
Message Data: "71\02\01\0\0"
Length: 8 bytes
Full Message: [0x00 0x00 0x00 0x08] + "71\02\01\0\0"
```

### Response
Server sends `nextValidId` event when API is ready to accept requests.

---

## Message Encoding Rules

### Token Types

#### Integer
- Format: Decimal string
- Example: `12345` → `"12345"`
- Empty value: `""` (interpreted as 0)

#### Double/Float
- Format: Decimal string
- Example: `123.45` → `"123.45"`
- Empty value: `""` (interpreted as 0 or undefined)
- MAX_VALUE: `"2147483647"` (interpreted as undefined)

#### Boolean
- Format: "0" or "1"
- Example: `true` → `"1"`, `false` → `"0"`

#### String
- Format: Direct value
- Example: `"SMART"` → `"SMART"`
- Empty value: `""` (NULL/undefined)

### Special Values

#### Empty String (`""`)
- Represents NULL/undefined in most contexts
- For integers: interpreted as 0
- For doubles: interpreted as 0 or undefined based on context
- For booleans: interpreted as false

#### MAX_VALUE (`"2147483647"`)
- Special sentinel for undefined values
- Used in doubles to indicate "no value"
- Should be decoded to `undefined`

---

## Common Message Types

### Incoming Messages (Server → Client)

#### NEXT_VALID_ID (9)
**Purpose:** Signal that connection is ready for API requests

**Tokens:**
1. Version: Integer
2. Order ID: Integer (next valid order ID to use)

**Example:**
```
Tokens: [9, 1, 100]
Decoded: version=9, orderId=100
```

**Critical:** This is the signal that the connection is READY.

#### ERR_MSG (4)
**Purpose:** Error or informational message from server

**Tokens:**
1. Version: Integer
2. Request ID: Integer (-1 if no associated request)
3. Error Code: Integer
4. Error Message: String
5. Advanced Order Reject: JSON string (if server version supports it)

**Example:**
```
Tokens: [4, 2, 100, 200, "No security definition has been found", ""]
Decoded: version=2, id=100, code=200, msg="No security definition has been found"
```

**Error Codes:**
- `-1` (NO_VALID_ID): Informational message, not an error
- `200`: No security definition found
- `201`: Order rejected
- `202`: Order cancelled
- `203`: Security not allowed
- And many more...

#### ACCOUNT_VALUE (6)
**Purpose:** Account value updates

**Tokens:**
1. Version: Integer
2. Key: String (e.g., "AccountType", "NetLiquidation")
3. Value: String
4. Currency: String
5. Account Name: String

**Example:**
```
Tokens: [6, 1, "AccountType", "UNIVERSAL", "USD", "DU1234567"]
Decoded: AccountType=UNIVERSAL, currency=USD, account=DU1234567
```

#### PORTFOLIO_VALUE (7)
**Purpose:** Portfolio position updates

**Tokens (version 8+):**
1. Version: Integer
2. Contract conId: Integer
3. Symbol: String
4. Security Type: String (STK, OPT, FUT, etc.)
5. Last Trade Date: String
6. Strike: Double
7. Right: String (CALL, PUT, "")
8. Multiplier: Double
9. Primary Exchange: String
10. Currency: String
11. Local Symbol: String
12. Trading Class: String
13. Position: Decimal
14. Market Price: Double
15. Market Value: Double
16. Average Cost: Double
17. Unrealized P&L: Double
18. Realized P&L: Double
19. Account Name: String

**Example:**
```
Tokens: [8, 756733, "AAPL", "STK", "", 0.0, "", 1.0, "SMART", "USD", "AAPL", "", 100, 150.25, 15025.0, 140.0, 1025.0, 0.0, "DU1234567"]
Decoded: 100 shares of AAPL at $150.25, avg cost $140.0
```

#### POSITION (61)
**Purpose:** Current position data

**Tokens (version 2+):**
1. Version: Integer
2. Account: String
3. Contract conId: Integer
4. Symbol: String
5. Security Type: String
6. Last Trade Date: String
7. Strike: Double
8. Right: String
9. Multiplier: Double
10. Exchange: String
11. Currency: String
12. Local Symbol: String
13. Trading Class: String
14. Position: Decimal
15. Average Cost: Double

**Example:**
```
Tokens: [2, "DU1234567", 756733, "AAPL", "STK", "", 0.0, "", 1.0, "SMART", "USD", "AAPL", "", 100, 140.0]
Decoded: Account DU1234567 has 100 shares of AAPL at avg cost $140.0
```

#### POSITION_END (62)
**Purpose:** End of position list marker

**Tokens:**
1. Version: Integer

**Example:**
```
Tokens: [2]
Decoded: End of position list
```

#### ORDER_STATUS (5)
**Purpose:** Order status update

**Tokens:**
1. Version: Integer
2. Order ID: Integer
3. Status: String (PendingSubmit, Submitted, PreSubmitted, Cancelled, Filled, etc.)
4. Filled: Decimal
5. Remaining: Decimal
6. Average Fill Price: Double
7. Permanent ID: Integer
8. Parent ID: Integer
9. Last Fill Price: Double
10. Client ID: Integer
11. Why Held: String
12. Market Cap Price: Double (if server version supports)

**Status Values:**
- `PendingSubmit`: Order submitted but not yet processed
- `Submitted`: Order accepted by exchange
- `PreSubmitted`: Simulated order status
- `Cancelled`: Order cancelled
- `Filled`: Order completely filled
- `Inactive`: Order not yet active

**Example:**
```
Tokens: [Number.MAX_VALUE, 100, "Submitted", 50, 50, 150.25, 123456789, 0, 150.25, 1, "", 0]
Decoded: Order 100 is Submitted, 50/50 filled at $150.25
```

#### OPEN_ORDER (3)
**Purpose:** Open order details

**Complex structure with many fields.** See reference implementation for full decoder.

**Key Fields:**
- Order ID
- Contract details
- Order parameters (action, quantity, price, etc.)
- Order state (status, commission, etc.)

#### CURRENT_TIME (49)
**Purpose:** Server time

**Tokens:**
1. Version: Integer
2. Time: Integer (Unix timestamp in seconds)

**Example:**
```
Tokens: [1, 1736457890]
Decoded: Server time is 1736457890 (2025-01-09 12:31:30 UTC)
```

### Outgoing Messages (Client → Server)

#### REQ_POSITIONS (61)
**Purpose:** Request current positions

**Tokens:**
1. Version: Integer (1)

**Example:**
```
Tokens: [1]
Message: "1\0"
```

**Response:** Multiple POSITION (61) messages + POSITION_END (62)

#### REQ_ACCOUNT_DATA (6)
**Purpose:** Request account updates (deprecated, use REQ_ACCOUNT_SUMMARY)

**Tokens:**
1. Version: Integer (2)
2. Subscribe: Boolean (0 or 1)
3. Account Code: String (optional, for FA clients)

**Example:**
```
Tokens: [2, 1, ""]
Message: "2\01\0\0"
```

**Response:** Multiple ACCOUNT_VALUE (6) messages + ACCT_UPDATE_TIME (7) + ACCT_DOWNLOAD_END (8)

#### REQ_ACCOUNT_SUMMARY (62)
**Purpose:** Request account summary data

**Tokens:**
1. Version: Integer (1)
2. Request ID: Integer
3. Group: String (e.g., "All")
4. Tags: String (comma-separated tags, e.g., "AccountType,NetLiquidation,TotalCashValue")

**Example:**
```
Tokens: [1, 100, "All", "AccountType,NetLiquidation,TotalCashValue"]
Message: "1\0100\0All\0AccountType,NetLiquidation,TotalCashValue\0"
```

**Response:** Multiple ACCOUNT_SUMMARY (63) messages + ACCOUNT_SUMMARY_END (64)

#### REQ_OPEN_ORDERS (5)
**Purpose:** Request open orders

**Tokens:**
1. Version: Integer (1)

**Example:**
```
Tokens: [1]
Message: "1\0"
```

**Response:** Multiple OPEN_ORDER (3) messages + OPEN_ORDER_END (20)

#### PLACE_ORDER (3)
**Purpose:** Place new order

**Complex structure with many fields.** See reference implementation for full encoder.

**Key Fields:**
- Order ID
- Contract details
- Order parameters (action, totalQuantity, orderType, lmtPrice, etc.)

**Example (Simple Market Order):**
```
Order ID: 101
Contract: AAPL stock
Action: BUY
Quantity: 100
Order Type: MKT

Tokens: [3, ..., 101, ..., "BUY", 100, "MKT", ...]
```

#### CANCEL_ORDER (4)
**Purpose:** Cancel existing order

**Tokens:**
1. Version: Integer (1)
2. Order ID: Integer

**Example:**
```
Cancel order 100
Tokens: [1, 100]
Message: "1\0100\0"
```

**Response:** ORDER_STATUS (5) with status "Cancelled"

---

## Data Types

### Security Types (SecType)
- `STK`: Stock
- `OPT`: Option
- `FUT`: Future
- `IND`: Index
- `FOP`: Future Option
- `CASH`: Forex pair
- `BAG`: Combo/Basket
- `WAR`: Warrant
- `BOND`: Bond
- `COMMODITY`: Commodity
- `NEWS`: News
- `FUND`: Mutual fund

### Order Actions
- `BUY`: Buy
- `SELL`: Sell
- `SSHORT`: Short sell
- `SSHORTX`: Short sell exempt

### Order Types
- `MKT`: Market
- `LMT`: Limit
- `STP`: Stop
- `STP LMT`: Stop limit
- `TRAIL`: Trailing stop
- `TRAIL LMT`: Trailing stop limit
- `REL`: Relative (market)
- `VOL`: Volatility

### Time in Force (TIF)
- `DAY`: Day order
- `GTC`: Good Till Cancelled
- `IOC`: Immediate or Cancel
- `OPG`: At the Opening

---

## Connection State Machine

### States

#### DISCONNECTED
- Initial state
- No connection to server
- Can transition to CONNECTING

#### CONNECTING
- TCP connection in progress
- Handshake not yet complete
- Can transition to CONNECTED or DISCONNECTED

#### CONNECTED
- TCP connection established
- Handshake complete
- START_API sent
- Waiting for nextValidId
- Can transition to READY or DISCONNECTED

#### READY
- nextValidId received
- Connection fully initialized
- Can send API requests
- Can transition to DISCONNECTED

### State Transitions

```
DISCONNECTED ──connect()──> CONNECTING
CONNECTING ──handshake──> CONNECTED
CONNECTING ──error──> DISCONNECTED
CONNECTED ──nextValidId──> READY
READY ──disconnect──> DISCONNECTED
[Any State] ──error──> DISCONNECTED
```

### Critical Rules

1. **No API requests until READY state**
   - Must wait for nextValidId event
   - Sending requests earlier will be rejected

2. **Always check server version**
   - Different server versions support different features
   - Use MIN_SERVER_VER constants

3. **Handle disconnects gracefully**
   - Clean up resources
   - Reconnect if needed

4. **Respect rate limits**
   - Maximum 40 requests per second
   - Implement command buffer with throttling

---

## Error Handling

### Error Message Format

```
ERR_MSG (4) with tokens:
- Version: Integer
- Request ID: Integer (-1 if no request)
- Error Code: Integer
- Error Message: String
- Advanced Order Reject: JSON (optional)
```

### Common Error Codes

| Code | Description | Action |
|------|-------------|--------|
| -1 | No Valid ID | Informational, not an error |
| 200 | No security definition found | Check contract details |
| 201 | Order rejected | Check order parameters |
| 202 | Order cancelled | Order was cancelled |
| 203 | Security not allowed | Check trading permissions |
| 210 | Data farm not connected | Wait and retry |
| 211 | Data farm connection broken | Reconnect |
| 215 | Duplicate order ID | Use unique order IDs |
| 399 | Order rejected by risk | Check margin/risk limits |
| 502 | Couldn't connect to TWS | Check connection settings |
| 1100 | Connectivity lost | Reconnect |
| 1101 | Failed to send | Check network |

### Error Response Handling

```typescript
// Reference implementation
if (id === ErrorCode.NO_VALID_ID) {
  // Informational message
  this.emitInfo(msg, code);
} else {
  // Actual error
  this.emitError(msg, code, id, advancedOrderReject);
}
```

---

## Rate Limiting

### Limits
- Maximum: 40 requests per second
- Minimum interval: 25ms between requests

### Implementation
```typescript
const MAX_REQUESTS_PER_SEC = 40;
const MIN_INTERVAL = 1000 / MAX_REQUESTS_PER_SEC; // 25ms

// Command buffer with rate limiter
class CommandBuffer {
  private lastSendTime = 0;
  
  send(msg: Message) {
    const now = Date.now();
    const elapsed = now - this.lastSendTime;
    
    if (elapsed < MIN_INTERVAL) {
      // Wait
      setTimeout(() => this.send(msg), MIN_INTERVAL - elapsed);
      return;
    }
    
    // Send immediately
    this.socket.write(msg);
    this.lastSendTime = now;
  }
}
```

---

## Protocol Versioning

### Server Version Constants

```typescript
MIN_SERVER_VER = {
  PENDING_ORDERS: 9,
  MARKET_DATA_TYPE: 10,
  ...
  CME_TAGGING_FIELDS_IN_OPEN_ORDER: 176
}

MAX_SUPPORTED_SERVER_VERSION = 176;
MIN_SERVER_VER_SUPPORTED = 38;
```

### Version Checks

```typescript
// Before using a feature
if (this.serverVersion >= MIN_SERVER_VER.FEATURE_NAME) {
  // Feature is supported
  useFeature();
} else {
  // Feature not supported
  useAlternative();
}
```

---

## Testing Checklist

### Handshake Test
- [ ] Connect to TWS (localhost:7497)
- [ ] Send handshake message
- [ ] Receive server version
- [ ] Validate version compatibility
- [ ] Send START_API
- [ ] Receive nextValidId
- [ ] Mark connection as ready

### Position Test
- [ ] Send REQ_POSITIONS
- [ ] Receive POSITION messages
- [ ] Receive POSITION_END
- [ ] Parse position data correctly
- [ ] Verify position values

### Account Data Test
- [ ] Send REQ_ACCOUNT_SUMMARY
- [ ] Receive ACCOUNT_SUMMARY messages
- [ ] Receive ACCOUNT_SUMMARY_END
- [ ] Parse account data correctly
- [ ] Verify account values

### Order Test (Paper Trading Only!)
- [ ] Send PLACE_ORDER
- [ ] Receive ORDER_STATUS (Submitted)
- [ ] Receive ORDER_STATUS (Filled)
- [ ] Send REQ_OPEN_ORDERS
- [ ] Verify order appears in list
- [ ] Send CANCEL_ORDER
- [ ] Receive ORDER_STATUS (Cancelled)

---

## Security Considerations

1. **Never test with live account**
   - Always use paper trading (port 7497)
   - Live account port is 7496
   - Confirm account type before any order operations

2. **Validate all order parameters**
   - Check quantity, price, symbol
   - Verify order type is supported
   - Ensure account has sufficient funds

3. **Handle errors gracefully**
   - Log all error messages
   - Implement retry logic for transient errors
   - Alert user for critical errors

4. **Rate limiting is mandatory**
   - Exceeding rate limits may result in connection termination
   - Implement client-side throttling

---

## References

- Official IB API Documentation: https://interactivebrokers.github.io/tws-api/
- Reference TypeScript Implementation: https://github.com/stoqey/ib
- Protocol Discussion: https://groups.io/g/ib-api
- TWS/Gateway Download: https://www.interactivebrokers.com/en/trading/tws.php