# Technical Notes - IB TWS API Wrapper (Gleam/JavaScript)

This document records detailed technical issues encountered during development and lessons learned for future Erlang target implementation.

## Critical Protocol Discoveries

### 1. Client ID Message Must Be Separate

**Issue**: Initially attempted to include client ID as part of the handshake message.

**Discovery**: The IB TWS API V100+ protocol requires the client ID to be sent as a **separate message** AFTER receiving the server's initial response, not as part of the handshake itself.

**Protocol Sequence**:
1. Client sends: `API\0` + 4-byte length + version string (e.g., "v0..200")
2. Server responds: `VERSION<timestamp> EST` (e.g., "20020260107 08:02:02 EST")
3. Client sends: 4-byte client ID integer (big-endian)

**Code Reference**: 
- `src/protocol.gleam` - `start_api_message()` creates handshake
- `src/protocol.gleam` - `client_id_message()` creates separate client ID message
- `test/improved_handshake_test.gleam` - Shows correct sequence

### 2. Asynchronous Data Reception Pattern

**Issue**: Using `receive()` after `sleep()` doesn't work reliably because data arrives asynchronously via event handlers.

**Discovery**: In the JavaScript runtime (Node.js), socket data arrives through event callbacks. The `sleep()` function returns a Promise but doesn't block the Gleam code execution flow.

**Current Workaround**: 
- Event handlers in `connection.gleam` automatically log received data
- Data is stored in `ConnectionState.received_data` list
- `receive()` retrieves most recent data from this list
- However, timing issues mean data may arrive after `receive()` is called

**Future Solution Needed**:
- Implement proper callback-based message processing
- Use message handler callbacks instead of polling with `receive()`
- Consider implementing a message queue that callbacks can process

**Code Reference**:
- `src/connection.gleam:77-82` - DataEvent handler stores data
- `src/connection.gleam:134-139` - `receive()` retrieves stored data
- `src/connection_ffi.mjs` - `sleep()` returns Promise (non-blocking)

## Type System Challenges

### 3. Type Mismatches in Message Handler

**Issue**: Compilation errors due to incorrect type assumptions for IB TWS message fields.

**Examples**:
- `filled`: Expected `Float`, actual `Int`
- `remaining`: Expected `Int`, actual `Float`  
- `avg_fill`: Expected `Float`, actual `Int`
- `account` in position handler: Expected `String`, actual `Int`

**Resolution**: 
- Updated type definitions in `MessageHandler` to match actual IB TWS protocol
- Used appropriate conversion functions (`int.to_string()`, `float.to_string()`)
- Created proper type definitions based on IB API documentation

**Lesson**: Always verify actual protocol field types against IB API documentation, not assumptions.

**Code Reference**:
- `src/message_handler.gleam:15` - `on_order_status` type definition
- `src/message_handler.gleam:16` - `on_position` type definition
- `src/message_handler.gleam:63-76` - Handler implementations

## Connection Issues

### 4. ECONNREFUSED When TWS Not Running

**Issue**: Connection fails with `Error: connect ECONNREFUSED 127.0.0.1:7497` when TWS is not running or not configured to accept API connections.

**Discovery**: 
- TWS must be running before attempting connection
- API connections must be enabled in TWS: File → Global Configuration → API → Settings
- "Enable ActiveX and Socket Clients" must be checked
- "Socket Port" must match the port used (7497 for paper, 7496 for live)
- "Allow connections from localhost" must be checked

**Error Handling**:
- Connection errors are properly captured and logged
- User receives clear error messages

**Code Reference**:
- `src/connection.gleam:84-86` - ErrorEvent handler
- `test/improved_handshake_test.gleam:107-113` - Error message formatting

## Handshake Protocol Details

### 5. Version String Format

**Issue**: Uncertainty about correct version string format.

**Discovery**: Multiple valid formats:
- Single version: `"v100"`
- Version range: `"v0..200"` (min..max)
- Current working format: `"v0..200"` (supports all versions from 0 to 200)

**Protocol Implementation**:
```
API\0 (4 bytes)
+ 4-byte big-endian length of version string
+ version string (UTF-8 encoded)
```

**Example**: "v0..200"
- Length: 7 bytes
- Bytes: 118, 48, 46, 46, 50, 48, 48 ('v', '0', '.', '.', '2', '0', '0')

**Code Reference**:
- `src/protocol.gleam:45-88` - `start_api_message()` implementation
- Debug output shows exact byte composition

### 6. Big-Endian Byte Encoding

**Issue**: Multi-byte integers must be big-endian (network byte order).

**Discovery**: IB TWS protocol uses big-endian encoding for all multi-byte integers.

**Implementation**:
```gleam
fn int_to_four_bytes_big_endian(value: Int) -> BitArray {
  let b3 = value / 16_777_216  // Most significant byte
  let temp = value / 65_536
  let b2 = temp % 256
  let temp2 = value / 256
  let b1 = temp2 % 256
  let b0 = value % 256         // Least significant byte
  <<b3:8, b2:8, b1:8, b0:8>>
}
```

**Code Reference**:
- `src/protocol.gleam:102-110` - `int_to_four_bytes_big_endian()`

## JavaScript-Specific Issues

### 7. Non-Blocking Sleep

**Issue**: `sleep()` function doesn't block execution in Gleam/JavaScript runtime.

**Discovery**: The FFI `sleep()` function returns a JavaScript Promise, but Gleam doesn't automatically await it. This means:
- Code after `sleep()` executes immediately
- Data may arrive after `receive()` is called
- Event handlers continue running in background

**Current Status**: This is a known limitation of the current implementation.

**Future Solutions**:
1. Implement proper async/await pattern using Gleam's promise support
2. Use event-driven architecture where callbacks handle all message processing
3. Consider using Gleam's `gleam/javascript/promise` module for proper async handling

**Code Reference**:
- `src/connection_ffi.mjs:14-18` - `sleep()` implementation
- `test/improved_handshake_test.gleam:44` - Usage that doesn't block

### 8. BitArray to Buffer Conversion

**Issue**: Sending BitArray data requires conversion to Node.js Buffer.

**Discovery**: Gleam BitArray has a `rawBuffer` property containing the underlying Uint8Array.

**Implementation**:
```javascript
export function send_bytes(socket, data) {
  // data is a Gleam BitArray with rawBuffer property
  const buffer = Buffer.from(data.rawBuffer);
  const success = socket.write(buffer);
  return success;
}
```

**Code Reference**:
- `src/connection_ffi.mjs:21-26` - `send_bytes()` implementation

## Testing Strategy

### 9. Paper vs Live Account Testing

**Discovery**: 
- Paper trading uses port 7497
- Live trading uses port 7496
- Never test buy/sell operations on live account
- Client IDs must be unique per connection

**Safety Measures**:
- Separate test files for paper and live accounts
- Clear warnings in live account test code
- Client ID: 1 for testing (can be any positive integer)

**Code Reference**:
- `test/ib_tws_api_test.gleam` - Paper trading test (port 7497)
- `test/live_account_test.gleam` - Live account test (port 7496)
- `test/improved_handshake_test.gleam` - Improved test (port 7497)

## Lessons for Erlang Target

### Key Takeaways for Erlang Implementation:

1. **Protocol Understanding**: The IB TWS protocol is the same regardless of runtime. The handshake sequence, message formats, and byte encoding rules are identical.

2. **Async Pattern**: Erlang's actor model is naturally suited for this:
   - Use GenServer for connection management
   - Messages arrive as Erlang messages (not callbacks)
   - Pattern matching on message types is idiomatic

3. **Type System**: 
   - Erlang is dynamically typed, so no type mismatches
   - However, consider using Dialyzer specs for documentation
   - Type specifications help catch errors at compile time

4. **Blocking Sleep**: 
   - Erlang's `timer:sleep/1` actually blocks the process
   - This makes polling approaches more viable
   - But still prefer message-driven architecture

5. **Byte Handling**:
   - Erlang's binaries are similar to BitArray
   - Use `<<>>` syntax for binary construction
   - Big-endian is default: `<<Value:32>>`

6. **TCP Socket**:
   - Use `gen_tcp` module for TCP connections
   - `{active, true}` for message-driven reception
   - `{active, false}` for polling with `gen_tcp:recv/2,3`

7. **Error Handling**:
   - Use `try/catch` for socket operations
   - Pattern matching on `ok`/`error` tuples
   - Supervision trees for fault tolerance

8. **Testing**:
   - Can use paper trading account for all tests
   - Live account tests should be clearly marked
   - Consider property-based testing for message parsing

## Current Status

### Working Features:
- ✓ TCP connection establishment
- ✓ V100+ handshake protocol implementation
- ✓ Server response parsing
- ✓ Client ID message sending
- ✓ Basic message handler framework
- ✓ Type-safe message definitions

### Known Limitations:
- ⚠️ Asynchronous data reception not fully handled
- ⚠️ No message queue implementation
- ⚠️ Limited message parsing (only server time)
- ⚠️ Sleep doesn't block (JavaScript runtime)

### Next Steps:
1. Implement proper event-driven message handling
2. Add message queue for async message processing
3. Implement message parsing for common IB TWS messages
4. Add market data request functionality
5. Add order placement (paper trading only)

## Port Detection Implementation

### 10. Automatic Port Detection for IB TWS API

**Issue**: Users switch between paper trading (port 7497) during the day and live trading (port 7496) at night. The library needs to automatically detect which port is available.

**Discovery**:
- The nc (netcat) command can be used to check if a port is listening
- `nc -z -w <timeout> <host> <port>` exits with code 0 if port is available, non-zero otherwise
- Using `execSync` from Node.js child_process allows synchronous port checking
- This approach works reliably with ES modules and doesn't block the event loop

**Implementation**:
```javascript
// src/connection_ffi.mjs
import { execSync } from 'child_process';

export function check_port_available(host, port, timeout = 1) {
    try {
        const result = execSync(`nc -z -w ${timeout} ${host} ${port} 2>&1`, {
            encoding: 'utf8'
        });
        return result.includes('succeeded');
    } catch (error) {
        return false;
    }
}

export function detect_ib_tws_port(host = "127.0.0.1", timeout = 1) {
    console.log(`[Port Detection] Starting detection for ${host} with timeout ${timeout}s`);
    
    const paper_port = 7497;
    const paper_available = check_port_available(host, paper_port, timeout);
    if (paper_available) {
        console.log(`[Port Detection] Port ${paper_port} (Paper Trading) is available`);
        return paper_port;
    }
    
    const live_port = 7496;
    const live_available = check_port_available(host, live_port, timeout);
    if (live_available) {
        console.log(`[Port Detection] Port ${live_port} (Live Trading) is available`);
        return live_port;
    }
    
    console.log(`[Port Detection] Neither port 7496 nor 7497 is available on ${host}`);
    return 0;
}
```

**Gleam API**:
```gleam
@external(javascript, "./connection_ffi.mjs", "detect_ib_tws_port")
pub fn detect_ib_tws_port(host: String, timeout: Int) -> Int

/// Create connection config with automatic port detection
pub fn config_auto_detect(
  host: String,
  client_id: Int,
  timeout: Int,
) -> Result(ConnectionConfig, String) {
  let detected_port = detect_ib_tws_port(host, timeout)
  case detected_port {
    0 -> Error("No IB TWS server detected on ports 7496 or 7497")
    port -> Ok(ConnectionConfig(host: host, port: port, client_id: client_id))
  }
}
```

**Usage Example**:
```gleam
case connection.config_auto_detect("127.0.0.1", client_id, 1) {
  Ok(config) -> {
    io.println("✓ Connected to port: " <> int.to_string(config.port))
    // Proceed with connection
  }
  Error(err) -> {
    io.println("❌ " <> err)
    // Handle error - neither port is available
  }
}
```

**Technical Decisions**:
1. **Return Type**: Returns `Int` where `0` means "not found", any other value is the port number
   - Simpler than returning `Option(Int)` or `Result(Int, String)`
   - Avoids Gleam FFI marshaling issues with complex types
   - Easy to pattern match: `case port { 0 -> ...; _ -> ... }`

2. **Detection Order**: Checks paper trading port (7497) first, then live trading port (7496)
   - Prioritizes safer paper trading environment
   - Matches typical development workflow

3. **Timeout Parameter**: Uses seconds (not milliseconds) for nc command
   - Default timeout of 1 second is reasonable for local connections
   - Can be increased for slower networks

4. **Logging**: Console.log statements provide debugging information
   - Shows which port is being checked
   - Indicates which port was detected
   - Helps troubleshoot connection issues

**Testing**:
- Created `test/auto_port_detection_test.gleam` to verify functionality
- Test successfully detected port 7496 (live trading) and connected to it
- Demonstrates full handshake flow with auto-detected port

**Code Reference**:
- `src/connection_ffi.mjs:7-37` - Port detection implementation
- `src/connection.gleam:108-134` - Gleam API for port detection
- `test/auto_port_detection_test.gleam` - Complete test demonstrating usage

## References

- IB TWS API Documentation: https://interactivebrokers.github.io/tws-api/
- IB API Version Numbers: API_VersionNum.txt (in IB documentation)
- Gleam Documentation: https://gleam.run/
- Gleam JavaScript FFI: https://gleam.run/writing-javascript-ffi/
- nc (netcat) manual: https://linux.die.net/man/1/nc