# Gleam TWS API Wrapper Development - Conversation Summary

## Project Overview

This document summarizes the development progress of a Gleam wrapper for Interactive Brokers' TWS (Trader Workstation) API. The goal is to enable Gleam applications to communicate with TWS through TCP socket connections using Interactive Brokers' proprietary binary protocol.

## Technical Foundation

### TWS API Architecture

- **Live Trading Port**: 7496
- **Paper Trading Port**: 7497
- **Protocol**: Binary message protocol over TCP sockets
- **Byte Order**: Network byte order (big-endian)
- **Message Format**: 4-byte length prefix followed by message payload
- **Handshake**: Client sends API version + client ID, TWS responds with server version

### Client ID Generation

The recommended approach for generating unique client IDs is timestamp-based:

```python
client_id = int(time.time() * 1000) % 1000000
if client_id == 0:
    client_id = 1
```

This ensures uniqueness without requiring centralized coordination.

### API Version

Version 76 is used in the implementation, representing a balance between compatibility and available features.

## Reference Implementation Analysis

### ibOptions Project Structure

The ibOptions Python project served as the primary reference implementation:

- **ib_agent_async.py**: Demonstrates async connection patterns using reusable IB instances
- **ib_settings.py**: Contains TWS connection parameters and configuration
- **reference/info_ib_ticker.py**: Shows example message structures for market data

### Key Patterns Observed

1. **Reusable IB Instance Strategy**: Create IB() object once and reuse across operations
2. **Async/Await Patterns**: Non-blocking connection with proper timeout handling
3. **Connection Lifecycle**: Connect → Handshake → API Requests → Process → Disconnect

## Implementation Status

### Completed Work

1. **TCP Socket Infrastructure**: Implemented in Gleam using Erlang gen_tcp via FFI
2. **Protocol Module**: Message encoding/decoding functionality in protocol.gleam
3. **Test Harness**: test_socket_simple.gleam for connection testing with detailed logging
4. **Python Verification**: test_ib_async.py to validate ib_async connection patterns

### Current Implementation Files

- `src/ib_tws_api/socket.gleam`: Low-level TCP connection management
- `src/ib_tws_api/protocol.gleam`: Binary message encoding/decoding
- `src/ib_tws_api/client.gleam`: High-level client API
- `src/ib_tws_api/types.gleam`: Domain type definitions
- `src/test_socket_simple.gleam`: Primary test harness with handshake implementation
- `test_ib_async.py`: Python verification script
- `native/src/socket_helper.erl`: Erlang FFI layer for socket operations

## Technical Challenges

### Outstanding Issue: TWS Handshake Response

**Problem**: TWS accepts TCP connections but does not respond to handshake messages.

**Investigated Approaches**:
1. Tested both message formats (with/without length prefix)
2. Tried different client ID values (static and timestamp-based)
3. Tested both ports 7496 and 7497
4. Verified TWS is listening using `lsof -i :7496`

**Status**: Issue remains unresolved. Further investigation needed into:
- Exact byte-level handshake format requirements
- TWS API settings configuration
- Network traffic analysis with Wireshark

### Resolved Issues

1. **Timeout Command**: Removed external `timeout` dependency, using Gleam's built-in timeout
2. **Module Not Found**: Identified ib_async installation issue in test environment
3. **Unused Function**: Identified unused `socket_connect` in socket.gleam

## Architecture Decisions

### Module Organization

```
src/
├── ib_tws_api/
│   ├── socket.gleam      # TCP connection management
│   ├── protocol.gleam    # Message encoding/decoding
│   ├── client.gleam      # High-level client API
│   ├── types.gleam       # Domain type definitions
│   ├── test_connection.gleam
│   └── test_real_integration.gleam
├── test_socket_simple.gleam  # Primary test harness
└── [other test files]
```

### Error Handling

- Uses Gleam's Result type for operations that can fail
- Comprehensive logging throughout connection process
- Pattern matching for exhaustive error handling

### Type Safety

- Custom types model domain accurately
- Static type checking at compile time
- Algebraic data types for message types and error conditions

## Code Examples

### Socket Connection (Gleam)

```gleam
case socket.connect_socket("127.0.0.1", 7496) {
  Ok(socket) -> {
    // Send handshake
    let handshake = create_handshake_message(server_version, client_id)
    socket.send(socket, handshake)
  }
  Error(reason) -> {
    io.println("Connection failed: " <> error_to_string(reason))
  }
}
```

### Handshake Message Creation

```gleam
let handshake = <<
  server_version:big-unsigned-int-32,
  client_id:big-unsigned-int-32,
  // Additional fields based on API version
>>
```

### Python Reference (ib_async)

```python
async def connect_to_ib():
    ib = IB()
    client_id = int(time.time() * 1000) % 1000000
    await ib.connectAsync('127.0.0.1', 7496, clientId=client_id, timeout=10)
```

## Next Steps

### Immediate Priority

1. **Resolve Handshake Issue**
   - Examine official IB API source code for exact handshake format
   - Verify TWS API settings enable connections
   - Analyze network traffic with Wireshark
   - Test with known-working client implementation

### Short-term Tasks

2. **Protocol Implementation**
   - Complete message encoding/decoding for all message types
   - Implement market data request/response handling
   - Add order placement and management messages
   - Implement account update subscriptions

3. **Error Handling Improvements**
   - Add connection retry logic
   - Implement reconnection after disconnection
   - Add proper timeout handling for all operations

### Medium-term Tasks

4. **Testing and Validation**
   - Create unit tests for protocol encoding
   - Implement integration tests with TWS
   - Add property-based testing for message encoding

5. **Documentation**
   - Document public API
   - Add usage examples
   - Create architecture documentation

## Key Takeaways

1. **Protocol Sensitivity**: TWS API requires exact byte-level formatting; even small deviations cause silent failures

2. **Client ID Uniqueness**: Timestamp-based generation is essential for distributed systems

3. **TWS Configuration**: Must explicitly enable API connections in TWS settings

4. **Reference Value**: The ibOptions project provided invaluable patterns for async communication

5. **Debugging Approach**: Systematic investigation from basic connectivity to protocol format was essential

## Commands Used During Development

```bash
# Check if TWS is listening on port
lsof -i :7496

# Run Gleam tests
gleam test

# Build project
gleam build

# Run Python test script
python test_ib_async.py
```

## Important Notes

- **Public Repository**: No account information should be included in code or logs
- **Gleam/ERlang**: The implementation leverages Gleam's ability to interoperate with Erlang for socket operations
- **Async Model**: Gleam's actor model provides natural fit for the async communication pattern required by TWS API

## Conclusion

The project has established the foundational infrastructure for Gleam TWS API integration. The primary remaining challenge is resolving the handshake issue to establish successful communication with TWS. Once this is resolved, the path to implementing the full protocol becomes clear, drawing on the patterns observed in the reference implementation and the protocol specifications.
