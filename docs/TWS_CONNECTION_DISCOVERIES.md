# TWS Connection Discoveries

## Date: 2026-01-09

## Protocol Diagnostic Results

### Port Availability
✅ **Port 7496 (Live Trading)**: OPEN - TWS is listening
✅ **Port 7497 (Paper Trading)**: OPEN - TWS is listening

### Connection Behavior

**Observation**: TWS is actively closing connections with `ECONNRESET` error immediately after handshake.

**Timeline**:
1. Socket connects successfully to TWS
2. Socket ready event fires
3. We send handshake message (API\0 + 4-byte length + version string "v100.200")
4. **TWS immediately resets connection (ECONNRESET)**

### Handshake Message Sent
```
API\0 (4 bytes)
+ 4-byte big-endian length (8 bytes for "v100.200")
+ version string "v100.200" (8 bytes)
= Total 16 bytes
```

### Analysis

The ECONNRESET error indicates TWS is actively rejecting our handshake message. Possible causes:

1. **Protocol Version Mismatch**: TWS may not support the version string format we're using
2. **Handshake Format Error**: The handshake format may be incorrect
3. **TWS Configuration**: TWS may have restrictive API settings
4. **Authentication**: TWS may require additional authentication before accepting connection

### Next Steps

1. **Research correct TWS handshake protocol** - Check IB TWS API documentation for exact handshake format
2. **Test different version strings** - Try "v100" instead of "v100.200"
3. **Check TWS API settings** - Verify TWS API configuration allows our client
4. **Implement proper event-driven receive** - Use callbacks instead of polling

### Fact-Discovery Progress

✅ **Confirmed**: TWS is running and listening on both ports
✅ **Confirmed**: TCP connection establishment works
✅ **Confirmed**: Handshake message is being sent
✅ **Discovered**: TWS actively rejects current handshake format
❌ **Pending**: Need to discover correct handshake protocol

## Test Files Created

1. `test/check_tws_port.gleam` - Port availability checker
2. `test/protocol_diagnostic.gleam` - Minimal handshake test
3. `test/event_driven_test.gleam` - Full protocol sequence test
4. `test/receive_and_decode_test.gleam` - Response receiver test

## Key Files Modified

- `src/connection_ffi.mjs` - Added `check_port()` function
- `test/ib_tws_api_test.gleam` - Fact-discovery test suite (8 passing tests)