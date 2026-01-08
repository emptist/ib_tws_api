# Reference Implementation Analysis

## Overview
This document analyzes the reference implementations to understand the correct IB TWS API protocol and identify issues in our Gleam implementation.

---

## TypeScript Implementation (reference/ib/)

### Architecture

#### 1. Core Components

**IBApi Class** (`reference/ib/src/api/api.ts`)
- Main public API interface
- Uses EventEmitter for event-driven architecture
- Delegates I/O to Controller
- MAX_SUPPORTED_SERVER_VERSION = MIN_SERVER_VER.CME_TAGGING_FIELDS_IN_OPEN_ORDER
- MIN_SERVER_VER_SUPPORTED = 38

**IBApiNext Class** (`reference/ib/src/api-next/api-next.ts`)
- RxJS Observable-based subscription architecture
- Wrapper around IBApi with modern async patterns
- Auto-reconnect and connection watchdog features
- Subscription registry for managing multiple requests

**Controller Class** (`reference/ib/src/core/io/controller.ts`)
- Dispatcher between public API and underlying I/O code
- CommandBuffer for command queuing and execution
- Rate limiter: 40 requests per second by default
- Encoder and Decoder instances for message processing
- **CRITICAL**: `controller.resume()` is called when `nextValidId` event is received
- Connection states: Disconnected, Disconnecting, Connecting, Connected

**Socket Class** (`reference/ib/src/core/io/socket.ts`)
- Low-level V100+ protocol communication
- Manages TCP socket connection
- Handles protocol handshake and message framing

**Encoder Class** (`reference/ib/src/core/io/encoder.ts`)
- Encodes API requests into token arrays
- 120 different message types (REQ_MKT_DATA, PLACE_ORDER, etc.)
- Version-aware encoding based on server version
- Handles contract encoding, order encoding, etc.

**Decoder Class** (`reference/ib/src/core/io/decoder.ts`)
- Decodes incoming messages into events
- 60+ different message types
- Message queue processing with boundary markers
- OrderDecoder helper class for complex order decoding

---

### Connection Flow (CRITICAL)

#### Step 1: Socket Connection
```typescript
socket.connect(host, port) {
  // Create TCP socket
  this.socket = net.createConnection({ host, port });
  
  // PAUSE controller during startup
  this.controller.pause();
}
```

#### Step 2: Send Handshake
```typescript
onConnect() {
  // Send "API\0" + 4-byte length + version string
  const version = "v" + MAX_SUPPORTED_SERVER_VERSION + "." + MIN_SERVER_VER_SUPPORTED;
  const msg = "API\0" + encodeLength(version.length) + version;
  this.socket.write(msg);
}
```

#### Step 3: Receive Server Response
```typescript
onData(data) {
  // Buffer incoming data
  // Parse V100 messages with 4-byte big-endian length prefix
  
  // First message is special - contains server version
  // Skip first 5 tokens: "API\0" + 4 length bytes
  const tokens = this.parseMessage(data);
  this.onServerVersion(tokens);
}
```

#### Step 4: Validate and Start API
```typescript
onServerVersion(tokens) {
  // Parse server version from first message
  // Validate version compatibility
  this.serverVersion = parseInt(tokens[0]);
  this.serverTime = tokens[1];
  
  // Send START_API message
  this.startAPI();
}

startAPI() {
  // START_API message format:
  // [OUT_MSG_ID.START_API, VERSION (2), clientId, optionalCapabilities]
  const tokens = [OUT_MSG_ID.START_API, 2, this.clientId, ""];
  this.controller.send(tokens);
  
  // Emit connected event
  this.emit('connected');
}
```

#### Step 5: Controller Resume (CRITICAL!)
```typescript
// In Controller:
onNextValidId(orderId) {
  // This is when connection is FULLY established
  this.resume();
  this.emit('nextValidId', orderId);
}
```

**KEY INSIGHT**: The controller is PAUSED during startup and only RESUMED when `nextValidId` is received. This prevents sending API requests before the connection is ready.

---

### Message Protocol Details

#### V100+ Protocol Format
```
[4-byte length prefix (big-endian)][message data][NULL terminator]
```

- Length prefix: 4 bytes, big-endian
- Message data: NULL-separated tokens
- NULL terminator: 0x00 byte at end

#### Handshake Format
```
Client -> Server: "API\0" + [4-byte length] + version_string
Server -> Client: [server_version, server_time, ...] (NULL-separated)
```

Example:
```
"API\0" + [0x00, 0x00, 0x00, 0x18] + "v176.38"
```

#### START_API Message Format
```
[71, 2, client_id, ""]  // 71 = OUT_MSG_ID.START_API
```

- Field 1: Message ID (71)
- Field 2: Version (2)
- Field 3: Client ID (integer)
- Field 4: Optional capabilities (empty string)

#### Message Encoding (Encoder)

All messages are encoded as NULL-separated token arrays:

```typescript
// Example: REQ_POSITIONS
reqPositions() {
  const version = 1;
  this.sendMsg(OUT_MSG_ID.REQ_POSITIONS, version);
}

// Results in tokens: [61, 1]
// Encoded as: "61\01\0"
```

**Token Encoding Rules**:
- Integers: String representation
- Doubles: String representation, empty string = 0 or undefined
- Booleans: "0" or "1"
- Strings: Direct value
- Empty/undefined values: Empty string ""

#### Message Decoding (Decoder)

```typescript
// Decoder maintains a queue of tokens
dataQueue: (string | undefined)[] = [];

// V100 protocol: queue includes boundary markers
enqueueMessage(tokens: string[]) {
  this.dataQueue.push(undefined); // start boundary
  this.dataQueue = this.dataQueue.concat(tokens);
  this.dataQueue.push(undefined); // end boundary
}

// Process messages
process() {
  while (true) {
    // Check for message boundary
    if (this.dataQueue[0] === undefined) {
      verifyMessageBoundary = true;
      this.dataQueue.shift();
    }
    
    // Read message ID
    const msgId = this.readInt();
    
    // Dispatch to decoder
    this.processMsg(msgId);
    
    // Verify all data processed if boundary marker present
    if (verifyMessageBoundary) {
      if (this.dataQueue[0] !== undefined) {
        // Error: unprocessed data left
      }
      this.drainQueue();
    }
  }
}
```

**Token Reading Methods**:
```typescript
readStr(): string {
  // Read next token from queue
  // Throws UnderrunError if empty
  return this.dataQueue.shift();
}

readInt(): number {
  const token = this.readStr();
  if (token === "") return 0;
  return parseInt(token, 10);
}

readDouble(): number | undefined {
  const token = this.readStr();
  if (token === "") return 0;
  const val = parseFloat(token);
  return val === Number.MAX_VALUE ? undefined : val;
}
```

---

### Key Findings vs Current Implementation

#### ✅ What We Got Right

1. **V100+ Protocol Handshake**: Correctly implemented "API\0" + length + version
2. **Big-endian encoding**: Using 4-byte big-endian length prefix
3. **NULL separators**: Correctly using NULL (0x00) as field separator
4. **Client ID message**: Format matches reference

#### ❌ What We Got Wrong

##### Issue 1: Missing Controller Pause/Resume Mechanism
**Problem**: Our implementation sends requests immediately after START_API
**Fix**: Need to implement a "ready" state that only allows requests after `nextValidId` is received

**Reference Flow**:
```
1. Connect -> Controller PAUSED
2. Send handshake
3. Receive server version
4. Send START_API
5. Wait for nextValidId event
6. Controller RESUMED -> Ready to send requests
```

**Current Flow**:
```
1. Connect
2. Send handshake
3. Send START_API
4. ❌ Try to send requests immediately (too early!)
```

##### Issue 2: Message Encoding Format
**Problem**: Our encoder creates message with length prefix for EVERY message
**Fix**: Length prefix is ONLY for V100 protocol on the wire, not in token arrays

**Reference**:
```typescript
// Encoder creates token arrays
const tokens = [OUT_MSG_ID.REQ_POSITIONS, 1];
this.sendMsg(tokens);

// Socket handles framing:
sendMsg(tokens) {
  const msg = tokens.join('\0') + '\0';
  const length = Buffer.byteLength(msg);
  this.socket.write(encodeLength(length) + msg);
}
```

**Current**: We're trying to encode length prefix in the message itself, which is wrong.

##### Issue 3: START_API Message Structure
**Problem**: Current implementation may have incorrect field ordering
**Fix**: Must match exact format: `[71, 2, clientId, ""]`

**Reference**:
```typescript
startAPI() {
  this.controller.send([
    OUT_MSG_ID.START_API,  // 71
    2,                    // VERSION
    this.clientId,
    ""                     // optional capabilities (empty)
  ]);
}
```

---

### Message Types Summary

#### Incoming Messages (Decoder handles 60+ types)

Key messages for our use case:
- `NEXT_VALID_ID` (9): Connection ready signal
- `ERR_MSG` (4): Error messages
- `ACCOUNT_VALUE` (6): Account data
- `PORTFOLIO_VALUE` (7): Portfolio positions
- `POSITION` (61): Current positions
- `ORDER_STATUS` (5): Order status updates
- `OPEN_ORDER` (3): Open order details
- `EXECUTION_DATA` (8): Execution reports
- `CURRENT_TIME` (49): Server time

#### Outgoing Messages (Encoder handles 120 types)

Key messages for our use case:
- `START_API` (71): Initialize API connection
- `REQ_POSITIONS` (61): Request positions
- `REQ_ACCOUNT_DATA` (6): Request account updates
- `REQ_ACCOUNT_SUMMARY` (62): Request account summary
- `REQ_OPEN_ORDERS` (5): Request open orders
- `PLACE_ORDER` (3): Place new order
- `CANCEL_ORDER` (4): Cancel order

---

### Rate Limiting

**Reference Implementation**:
```typescript
// Controller has rate limiter
const MAX_REQUESTS_PER_SEC = 40;
const MIN_INTERVAL_BETWEEN_REQUESTS = 1000 / MAX_REQUESTS_PER_SEC; // 25ms
```

**Our Implementation**: No rate limiting yet

**Priority**: Medium - can be added later, but important for production

---

### Error Handling

**Reference**:
```typescript
// Decoder emits errors through callback
emitError(errMsg: string, code: number, reqId?: number, advancedOrderReject?: unknown)

// Special handling for info messages (code = NO_VALID_ID)
if (id === ErrorCode.NO_VALID_ID) {
  this.emitInfo(msg, code);
} else {
  this.emitError(msg, code, id, advancedOrderReject);
}
```

**Our Implementation**: Basic error handling, needs improvement

---

## Comparison with Other Reference Implementations

### Rust Implementation
- Similar architecture to TypeScript
- Uses tokio for async I/O
- Byte-based protocol handling
- Strong typing for message structures

### Python Implementation (IBPy)
- Older implementation, less active
- Uses sockets directly
- Simpler architecture
- Good for understanding basics

### Go Implementation
- Goroutine-based concurrency
- Channel-based communication
- Clean separation of concerns

---

## Action Items for Gleam Implementation

### Immediate (Blocking Issues)

1. **Implement Connection Ready State**
   - Add `ready: Bool` field to connection state
   - Set to `False` on connect
   - Set to `True` only when `nextValidId` received
   - Block all API requests until `ready == True`

2. **Fix Message Encoding**
   - Remove length prefix from message content
   - Length prefix should only be added at socket send time
   - Encode messages as NULL-separated token strings

3. **Fix START_API Message**
   - Ensure exact format: `[71, 2, clientId, ""]`
   - Verify field order and types

### Short-term (Next Features)

4. **Implement Position Decoder**
   - Parse `POSITION` (61) messages
   - Extract: account, contract, position, avgCost

5. **Implement Account Value Decoder**
   - Parse `ACCOUNT_VALUE` (6) messages
   - Extract: key, value, currency, accountName

6. **Implement Order Status Decoder**
   - Parse `ORDER_STATUS` (5) messages
   - Extract: orderId, status, filled, remaining, etc.

7. **Implement Request Encoders**
   - `REQ_POSITIONS` (61)
   - `REQ_ACCOUNT_DATA` (6)
   - `REQ_ACCOUNT_SUMMARY` (62)
   - `REQ_OPEN_ORDERS` (5)

### Medium-term

8. **Add Rate Limiting**
   - Implement command buffer with rate limiter
   - 40 requests per second max

9. **Improve Error Handling**
   - Parse error codes and messages
   - Distinguish errors from info messages
   - Handle advanced order rejection data

10. **Add Reconnection Logic**
    - Auto-reconnect on disconnect
    - Exponential backoff
    - Connection watchdog

---

## Protocol Testing Checklist

### Handshake Testing
- [ ] Connect to TWS (port 7497)
- [ ] Send "API\0" + length + version
- [ ] Receive server version response
- [ ] Validate version compatibility
- [ ] Send START_API message
- [ ] Wait for nextValidId event
- [ ] Mark connection as ready

### Message Testing
- [ ] Send REQ_POSITIONS
- [ ] Receive POSITION messages
- [ ] Receive POSITION_END message
- [ ] Parse position data correctly
- [ ] Send REQ_ACCOUNT_DATA
- [ ] Receive ACCOUNT_VALUE messages
- [ ] Receive ACCT_DOWNLOAD_END message
- [ ] Parse account data correctly

---

## Notes

1. **Server Version Compatibility**
   - Current TWS/Gateway versions are around 176+
   - Our implementation should support versions 38+
   - Always check MIN_SERVER_VER before using features

2. **NULL Handling**
   - NULL (0x00) is the field separator
   - Empty strings represent missing/undefined values
   - Must preserve NULL separators in encoding

3. **Big-endian Encoding**
   - All multi-byte integers are big-endian
   - Length prefix: 4 bytes, big-endian
   - Client ID: 4 bytes, big-endian

4. **Message Boundaries**
   - V100 protocol uses explicit boundary markers (undefined in queue)
   - Ensures complete message processing
   - Detects protocol errors early

5. **Connection Lifecycle**
   - Connecting → Connected → Ready (nextValidId) → Active
   - Must wait for Ready state before sending API requests
   - Disconnect at any point requires cleanup

---

## References

- TypeScript: https://github.com/stoqey/ib
- Rust: https://github.com/stoqey/ib-rs
- Python: https://github.com/blampe/IBPy
- Go: https://github.com/gofinance/ib