# System-Level Review - IB TWS API Wrapper

**Date:** 2026-01-09  
**Status:** CRITICAL ISSUES IDENTIFIED  
**Root Cause:** Fundamental protocol implementation errors

---

## Executive Summary

After comprehensive review of the codebase, reference implementations, and documentation, I've identified **MULTIPLE CRITICAL PROTOCOL ERRORS** that explain why TWS consistently closes connections. These are not minor bugs - they are fundamental misunderstandings of the IB TWS API protocol.

**Key Finding:** We're sending raw binary data instead of properly formatted NULL-separated messages for ALL API communications after handshake.

---

## Critical Root Problems

### Problem 1: Client ID Message Format is COMPLETELY WRONG ❌

**Current Implementation (protocol.gleam):**
```gleam
pub fn client_id_message(client_id: Int) -> BitArray {
  int_to_four_bytes_big_endian(client_id)
}
```

**What this does:** Sends ONLY 4 bytes of raw binary client ID  
**What it should do:** Send START_API message with NULL-separated tokens

**Reference Implementation (TypeScript):**
```typescript
startAPI() {
  const tokens = [OUT_MSG_ID.START_API, 2, this.clientId, ""];
  this.controller.send(tokens);
  // Results in: "71\02\0<client_id>\0\0" with 4-byte length prefix
}
```

**Correct Format:**
```
[4-byte length][71\02\0<client_id>\0\0]
```
Where:
- 71 = START_API message ID
- 2 = version
- client_id = client identifier
- "" = optional capabilities (empty string)
- All fields NULL-separated (0x00)

**Impact:** TWS rejects connection immediately because it receives invalid message format.

---

### Problem 2: We Have Correct Code But Don't Use It ❌

**In message_encoder.gleam (CORRECT CODE EXISTS):**
```gleam
pub fn start_api_message(client_id: Int) -> String {
  let tokens = [
    int_to_string(71),     // START_API message ID
    int_to_string(2),      // Version (fixed at 2)
    int_to_string(client_id),
    "",                    // Optional capabilities (empty string)
  ]
  
  // Join with NULL and add final NULL terminator
  let result = string.join(tokens, "\u{0000}") <> "\u{0000}"
  result
}
```

**But tests use protocol.client_id_message() (WRONG):**
```gleam
let client_id_msg = protocol.client_id_message(config.client_id)
connection.send_bytes(conn, client_id_msg)
```

**Solution:** Use `message_encoder.start_api_message()` instead!

---

### Problem 3: All API Messages Missing Length Prefix ❌

**Reference Implementation:**
```typescript
sendMsg(tokens) {
  const msg = tokens.join('\0') + '\0';
  const length = Buffer.byteLength(msg);
  this.socket.write(encodeLength(length) + msg);
}
```

**Our Implementation:**
- `message_encoder.gleam` creates NULL-separated strings ✅
- BUT does NOT add 4-byte length prefix ❌
- `connection.send()` sends string directly without length prefix ❌

**Impact:** TWS cannot parse messages without length prefix.

---

### Problem 4: Connection State Machine Not Implemented ❌

**Reference Implementation Flow:**
```
1. Connect → Controller PAUSED
2. Send handshake → "API\0" + length + version
3. Receive server version
4. Send START_API → "71\02\0<client_id>\0\0"
5. Wait for nextValidId event
6. Controller RESUMED → Ready to send requests
7. Send API requests
```

**Our Implementation Flow:**
```
1. Connect
2. Send handshake ✅
3. Send client_id_message (WRONG FORMAT) ❌
4. Try to send requests immediately ❌
5. Connection closes ❌
```

**Critical Missing:**
- No "ready" state tracking
- No waiting for nextValidId event
- No blocking requests until ready

**Impact:** Sending requests before TWS is ready causes immediate disconnection.

---

### Problem 5: Handshake Version String May Be Wrong ❌

**Current Implementation:**
```gleam
let version_string = "v100..200"
```

**Reference Implementation:**
```typescript
const version = "v" + MAX_SUPPORTED_SERVER_VERSION + "." + MIN_SERVER_VER_SUPPORTED;
// Example: "v176.38"
```

**Difference:**
- We use: "v100..200" (range notation)
- Reference uses: "v176.38" (dot notation)

**Impact:** TWS may reject unsupported version format.

---

### Problem 6: Missing Message Queue and Buffer ❌

**Reference Implementation:**
```typescript
// Decoder maintains a queue of tokens
dataQueue: (string | undefined)[] = [];

// V100 protocol: queue includes boundary markers
enqueueMessage(tokens: string[]) {
  this.dataQueue.push(undefined); // start boundary
  this.dataQueue = this.dataQueue.concat(tokens);
  this.dataQueue.push(undefined); // end boundary
}
```

**Our Implementation:**
- No message queue
- No buffering of partial messages
- No handling of fragmented messages

**Impact:** Cannot handle real-world message fragmentation.

---

## Protocol Format Summary

### Correct V100+ Protocol:

#### Handshake (Client → Server):
```
"API\0" + [4-byte big-endian length] + "v<max>.<min>"
Example: "API\0" + [0x00,0x00,0x00,0x07] + "v176.38"
```

#### Server Response (Server → Client):
```
NULL-separated tokens (no length prefix for handshake response)
Example: "176\01736457890\0"
```

#### START_API Message (Client → Server):
```
[4-byte length][71\02\0<client_id>\0\0]
Example: [0x00,0x00,0x00,0x08] + "71\02\01234\0\0"
```

#### API Requests (Client → Server):
```
[4-byte length][message_id\0<token1>\0<token2>\0...\0]
Example: REQ_POSITIONS: [0x00,0x00,0x00,0x05] + "61\01\0"
```

#### Server Messages (Server → Client):
```
[4-byte length][message_code:16-bit][payload...]
Example: Error message with code 4
```

---

## Required Fixes (Priority Order)

### Priority 1: Fix START_API Message Format (CRITICAL)

**File:** `src/protocol.gleam`

**Remove this function:**
```gleam
pub fn client_id_message(client_id: Int) -> BitArray {
  int_to_four_bytes_big_endian(client_id)
}
```

**Add this function:**
```gleam
/// Create START_API message (sent after handshake response)
/// Format: "71\02\0<client_id>\0\0" with 4-byte length prefix
pub fn start_api_message_with_length(client_id: Int) -> BitArray {
  let tokens = [
    int.to_string(71),     // START_API message ID
    int.to_string(2),      // Version (fixed at 2)
    int.to_string(client_id),
    "",                    // Optional capabilities (empty string)
  ]
  
  let message_data = string.join(tokens, "\u{0000}") <> "\u{0000}"
  let length_bytes = int_to_four_bytes_big_endian(string.length(message_data))
  let message_bytes = bit_array.from_string(message_data)
  
  bit_array.concat([length_bytes, message_bytes])
}
```

### Priority 2: Fix All Message Encoding

**File:** `src/message_encoder.gleam`

**Update ALL encoder functions to add length prefix:**

```gleam
/// Create REQ_ACCOUNT_SUMMARY message with length prefix
pub fn request_account_summary_with_length(
  request_id: Int,
  group_code: String,
  tags: String,
) -> BitArray {
  let tokens = [
    int.to_string(6),      // REQ_ACCOUNT_SUMMARY message ID
    int.to_string(request_id),
    group_code,
    tags,
  ]
  
  let message_data = string.join(tokens, "\u{0000}") <> "\u{0000}"
  let length_bytes = int_to_four_bytes_big_endian(string.length(message_data))
  let message_bytes = bit_array.from_string(message_data)
  
  bit_array.concat([length_bytes, message_bytes])
}

/// Similar updates for:
/// - request_positions_with_length()
/// - request_open_orders_with_length()
/// - cancel_order_with_length()
/// - place_order_with_length()
```

### Priority 3: Implement Connection State Machine

**File:** `src/connection.gleam`

**Add state tracking:**
```gleam
pub fn set_ready_state(conn: Connection, ready: Bool) -> Connection {
  let new_state = ConnectionState(..conn.state, ready: ready)
  Connection(socket: conn.socket, state: new_state)
}

pub fn wait_for_next_valid_id(conn: Connection, timeout_ms: Int) -> Bool {
  // Wait until ready flag is set by nextValidId event
  let start = get_timestamp()
  wait_for_ready_loop(conn, timeout_ms, 0)
}
```

**Block requests until ready:**
```gleam
pub fn send_when_ready(
  conn: Connection,
  data: BitArray,
) -> Result(Nil, ConnectionError) {
  case conn.state.ready {
    True -> send_bytes(conn, data)
    False -> Error(ConnectionFailed(
      "Connection not ready - wait for nextValidId event",
    ))
  }
}
```

### Priority 4: Fix Handshake Version String

**File:** `src/protocol.gleam`

**Change from:**
```gleam
let version_string = "v100..200"
```

**To:**
```gleam
let version_string = "v176.38"
```

**Or make it configurable:**
```gleam
pub fn start_api_message(min_version: Int, max_version: Int) -> BitArray {
  let version_string = "v" <> int.to_string(max_version) <> "." <> int.to_string(min_version)
  // ... rest of implementation
}
```

### Priority 5: Update All Tests

**Files:** All test files in `test/`

**Change from:**
```gleam
let client_id_msg = protocol.client_id_message(config.client_id)
connection.send_bytes(conn, client_id_msg)
```

**To:**
```gleam
let start_api_msg = protocol.start_api_message_with_length(config.client_id)
connection.send_bytes(conn, start_api_msg)
```

**Add ready state handling:**
```gleam
// Wait for nextValidId
case connection.wait_for_next_valid_id(conn, 5000) {
  True -> {
    io.println("✅ Connection ready!")
    // Now send requests
  }
  False -> {
    io.println("❌ Connection not ready")
  }
}
```

---

## Testing Strategy After Fixes

### Step 1: Test Handshake Only
```bash
gleam run -m test_handshake_only
```
Expected: Receive server version, no disconnection

### Step 2: Test START_API Message
```bash
gleam run -m keep_alive_handshake_test
```
Expected: Receive nextValidId event, connection stays alive

### Step 3: Test Simple Request (REQ_POSITIONS)
```bash
gleam run -m real_account_data_test
```
Expected: Receive POSITION messages + POSITION_END

### Step 4: Test Multiple Requests
```bash
gleam run -m comprehensive_test
```
Expected: All requests succeed, data received

---

## Files Requiring Changes

### Critical Changes:
1. `src/protocol.gleam` - Fix client_id_message, fix version string
2. `src/message_encoder.gleam` - Add length prefix to all messages
3. `src/connection.gleam` - Implement state machine
4. `test/keep_alive_handshake_test.gleam` - Use correct message format
5. `test/real_account_data_test.gleam` - Use correct message format
6. `test/proper_handshake_test.gleam` - Use correct message format

### Secondary Changes:
7. `test/test_handshake_only.gleam` - Verify handshake only
8. `test/detect_ports.gleam` - Keep as is (port detection works)
9. `test/check_port.gleam` - Keep as is (port check works)

---

## Risk Assessment

**Current Risk Level:** CRITICAL  
**Risk:** Every test fails immediately, TWS rejects all connections  
**Impact:** Cannot proceed with any development until fixed

**After Fixes Risk Level:** MODERATE  
**Risk:** May discover additional protocol issues during testing  
**Impact:** Can proceed with incremental development

---

## Lessons Learned

1. **Always verify against reference implementation** - We had correct code but didn't use it
2. **Protocol format is sacred** - Any deviation causes immediate rejection
3. **State machine is critical** - Cannot send requests until ready
4. **Length prefix is mandatory** - Every API message needs it
5. **NULL separators are required** - All tokens must be NULL-separated

---

## Next Steps

1. ✅ **COMPLETED:** System-level review
2. **NEXT:** Implement all Priority 1-4 fixes
3. **THEN:** Test with corrected implementation
4. **FINALLY:** Proceed with incremental feature development

---

## References

- IB TWS API Documentation: https://interactivebrokers.github.io/tws-api/
- Reference TypeScript Implementation: https://github.com/stoqey/ib
- Protocol Specification: `docs/protocol_specification.md`
- Reference Analysis: `docs/reference_implementation_analysis.md`