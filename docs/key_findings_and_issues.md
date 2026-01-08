# Key Findings and Critical Issues

## Executive Summary

After analyzing the reference TypeScript implementation in detail, I've identified **3 critical issues** that are preventing our Gleam implementation from working correctly. These issues explain why TWS closes the connection after the handshake.

---

## Critical Issues (Must Fix Immediately)

### Issue #1: Missing Connection Ready State ⚠️ MOST CRITICAL

**Problem:** Our implementation sends API requests immediately after sending START_API, but TWS is not ready to accept requests yet.

**Reference Implementation Flow:**
```typescript
// 1. Connect and PAUSE controller
socket.connect() {
  this.controller.pause();  // ⬅️ CRITICAL!
}

// 2. Send handshake
sendHandshake() { ... }

// 3. Send START_API
startAPI() { ... }

// 4. WAIT for nextValidId event
onNextValidId(orderId) {
  this.controller.resume();  // ⬅️ ONLY NOW can we send requests!
  this.emit('nextValidId', orderId);
}
```

**Our Current Flow:**
```gleam
// 1. Connect
connect() { ... }

// 2. Send handshake
send_handshake() { ... }

// 3. Send START_API
send_start_api() { ... }

// 4. ❌ Try to send requests IMMEDIATELY (too early!)
```

**Why TWS Closes Connection:**
- TWS expects the client to wait for `nextValidId` event
- Sending requests before receiving `nextValidId` violates protocol
- TWS interprets this as a protocol violation and closes the connection

**Fix Required:**
1. Add `ready: Bool` field to connection state
2. Set `ready = False` on connect
3. Set `ready = True` only when `nextValidId` is received
4. Block all API requests until `ready == True`

**Impact:** BLOCKING - Cannot proceed without this fix

---

### Issue #2: Incorrect Message Encoding Format

**Problem:** Our encoder creates messages with length prefix in the content, when length prefix should only be added at socket send time.

**Reference Implementation:**
```typescript
// Encoder creates token arrays ONLY
const tokens = [OUT_MSG_ID.REQ_POSITIONS, 1];
this.sendMsg(tokens);

// Socket handles framing:
sendMsg(tokens) {
  const msg = tokens.join('\0') + '\0';  // NULL-separated tokens
  const length = Buffer.byteLength(msg);    // Calculate length
  this.socket.write(encodeLength(length) + msg);  // Add prefix HERE
}
```

**Our Current Implementation:**
```gleam
// We're trying to encode length prefix IN the message itself
encode_message(message_id, tokens) {
  let content = join_with_null(tokens)
  let length = byte_size(content)
  // ❌ WRONG: Length should NOT be in the message content
  return encode_length(length) ++ content ++ [0]
}
```

**Why This Is Wrong:**
- Length prefix is a V100+ protocol framing mechanism
- It should be added by the socket layer, not the encoder
- Encoder should only create NULL-separated token strings
- Socket layer handles the framing (length prefix + NULL terminator)

**Fix Required:**
1. Encoder should return: `tokens.join('\0') + '\0'` (NULL-separated string)
2. Socket layer should: Add 4-byte big-endian length prefix
3. Remove all length prefix logic from encoder

**Impact:** BLOCKING - Messages won't be understood by TWS

---

### Issue #3: START_API Message Structure

**Problem:** Current implementation may have incorrect field ordering or missing fields.

**Reference Implementation:**
```typescript
startAPI() {
  this.controller.send([
    OUT_MSG_ID.START_API,  // 71
    2,                    // VERSION (fixed value)
    this.clientId,          // Client ID (integer)
    ""                     // Optional capabilities (empty string)
  ]);
}
```

**Expected Token Array:** `[71, 2, clientId, ""]`

**Expected Message Data:** `"71\02\0{clientId}\0\0"`

**Our Implementation (Needs Verification):**
- Check that message ID is 71
- Check that version is 2 (not 1 or other)
- Check that we have 4 tokens total
- Check that last token is empty string

**Fix Required:**
1. Verify exact token format matches reference
2. Ensure all 4 tokens are present
3. Ensure correct types (int, int, int, string)

**Impact:** BLOCKING - Wrong format will cause handshake failure

---

## Additional Issues (Important but Not Blocking)

### Issue #4: No Rate Limiting

**Problem:** We don't implement the 40 requests/second rate limit.

**Reference Implementation:**
```typescript
const MAX_REQUESTS_PER_SEC = 40;
const MIN_INTERVAL = 1000 / MAX_REQUESTS_PER_SEC; // 25ms

class CommandBuffer {
  send(msg) {
    if (elapsed < MIN_INTERVAL) {
      setTimeout(() => this.send(msg), MIN_INTERVAL - elapsed);
      return;
    }
    this.socket.write(msg);
  }
}
```

**Impact:** 
- Medium priority for now
- Critical for production use
- TWS may disconnect or throttle if rate exceeded

**Fix:** Implement command buffer with rate limiter (can be added later)

---

### Issue #5: Incomplete Error Handling

**Problem:** Basic error handling, doesn't distinguish errors from informational messages.

**Reference Implementation:**
```typescript
if (id === ErrorCode.NO_VALID_ID) {
  // Informational message, not an error
  this.emitInfo(msg, code);
} else {
  // Actual error
  this.emitError(msg, code, id, advancedOrderReject);
}
```

**Impact:**
- Low priority for now
- Important for production reliability
- Helps debug connection issues

**Fix:** Implement proper error code handling (can be added later)

---

## Protocol Insights

### Handshake Response Format

**What TWS Sends After Our Handshake:**
```
NULL-separated tokens (no length prefix in handshake response)
Example: "176\01736457890\0"
```

**Decoded:**
```
Token 1: "176" (server version)
Token 2: "1736457890" (server time)
```

**Our Current Parser:**
- Should correctly parse this format
- Need to verify we're not expecting length prefix

### Connection Lifecycle

**Correct State Machine:**
```
DISCONNECTED ──connect()──> CONNECTING
CONNECTING ──handshake──> CONNECTED
CONNECTED ──nextValidId──> READY  ⬅️ CRITICAL!
READY ──requests──> ACTIVE
[Any State] ──error──> DISCONNECTED
```

**Our Current State Machine:**
```
DISCONNECTED ──connect()──> CONNECTED  ⬅️ MISSING READY STATE
CONNECTED ──requests──> ACTIVE  ⬅️ TOO EARLY!
```

### Message Boundaries

**V100+ Protocol:**
```
[4-byte length][message data][NULL terminator]
```

**Critical Rules:**
1. Length prefix is framing only (added by socket)
2. Message data is NULL-separated tokens
3. NULL terminator marks end of message
4. Each token is a string representation

---

## Immediate Action Plan

### Step 1: Fix Connection Ready State (P0 - CRITICAL)
```gleam
// In src/connection.gleam

pub type ConnectionState {
  Connecting
  Connected
  Ready  // NEW: Add this state
  Active
  Disconnected
}

// In connection record
pub type Connection {
  state: ConnectionState,
  ready: Bool,  // NEW: Track ready state
  // ... other fields
}

// In message handler
fn handle_next_valid_id(conn, order_id: Int) {
  conn.ready = True  // ⬅️ SET READY HERE
  conn.state = Ready
  // Emit event
}
```

### Step 2: Fix Message Encoding (P0 - CRITICAL)
```gleam
// In src/protocol.gleam

// Encoder should ONLY create NULL-separated string
pub fn encode_message_tokens(tokens: List(String)) -> String {
  let content = tokens
    |> list.map(fn(t) { t })
    |> string.join("\0")
  
  // Add NULL terminator
  content ++ "\0"
}

// Socket layer adds length prefix
pub fn send_with_framing(socket, message: String) {
  let length = byte_size(message)
  let prefix = encode_big_endian_length(length)
  socket_write(socket, prefix ++ message)
}
```

### Step 3: Verify START_API Format (P0 - CRITICAL)
```gleam
// In src/protocol.gleam

pub fn encode_start_api(client_id: Int) -> String {
  // Exact format: [71, 2, client_id, ""]
  let tokens = [
    int_to_string(71),  // START_API message ID
    int_to_string(2),   // Version
    int_to_string(client_id),
    ""                  // Optional capabilities (empty)
  ]
  
  encode_message_tokens(tokens)
}
```

### Step 4: Test Connection (P1)
```gleam
// In test file
let conn = connect("127.0.0.1", 7497, 1)
// Wait for ready state
wait_for_ready(conn)
// Now can send requests
request_positions(conn)
```

---

## Testing Strategy

### Phase 1: Connection Test
1. Connect to TWS (port 7497)
2. Send handshake
3. Receive server version
4. Send START_API
5. **Wait for nextValidId** (NEW!)
6. Verify connection is in READY state
7. Verify no disconnection

### Phase 2: Position Request Test
1. Send REQ_POSITIONS
2. Receive POSITION messages
3. Receive POSITION_END
4. Parse position data
5. Verify position values

### Phase 3: Account Data Test
1. Send REQ_ACCOUNT_SUMMARY
2. Receive ACCOUNT_SUMMARY messages
3. Receive ACCOUNT_SUMMARY_END
4. Parse account data
5. Verify account values

---

## Success Criteria

### Connection Success
- [ ] Handshake completes without error
- [ ] START_API accepted
- [ ] nextValidId received
- [ ] Connection remains open (no disconnect)
- [ ] Connection state transitions to READY

### Data Reception Success
- [ ] Can request positions
- [ ] Can request account data
- [ ] Can request open orders
- [ ] Messages are parsed correctly
- [ ] Data values are accurate

### API Functionality Success
- [ ] Can place orders (paper trading only!)
- [ ] Can cancel orders
- [ ] Order status updates received
- [ ] Execution reports received

---

## Root Cause Analysis

**Why TWS Closes Connection:**

The most likely cause is **Issue #1** - we're sending requests before TWS is ready.

**Timeline:**
1. We connect to TWS
2. We send handshake
3. TWS responds with server version
4. We send START_API
5. **We immediately try to send REQ_POSITIONS** (or other request)
6. TWS sees request before `nextValidId` event
7. TWS interprets this as protocol violation
8. TWS closes connection

**Evidence:**
- Reference implementation explicitly pauses controller until `nextValidId`
- This is a documented requirement in IB API
- Our logs show connection closes after handshake
- Timing matches this scenario perfectly

---

## Conclusion

The three critical issues identified explain all observed problems:

1. **Missing Ready State** - Most likely cause of disconnection
2. **Incorrect Encoding** - Would prevent message understanding
3. **START_API Format** - Would cause handshake failure

**Priority:** Fix Issue #1 first, then #2, then #3 in that order.

**Expected Outcome:** After these fixes, connection should remain open and we can successfully request and receive data.

---

## References

- Reference Implementation: `reference/ib/src/`
- Protocol Spec: `docs/protocol_specification.md`
- Analysis: `docs/reference_implementation_analysis.md`
- IB API Docs: https://interactivebrokers.github.io/tws-api/