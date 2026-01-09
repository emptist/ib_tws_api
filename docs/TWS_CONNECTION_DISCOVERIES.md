# TWS Connection Discoveries

This document tracks discoveries made while connecting to Interactive Brokers TWS API.

## TWS Port Status (2026-01-09)

### Actual Port Status (from netstat)

**Port 7496 (live trading):** ✅ LISTENING
- Process: JavaAppli (PID 84903)
- TWS is accepting connections on this port

**Port 7497 (paper trading):** ❌ NOT LISTENING
- No process listening on this port
- Paper trading TWS is NOT running or not configured to use port 7497

## Test Results (2026-01-09)

### ✅ All 8 Tests Pass (but tests don't verify actual TWS responses)

All tests in `test/ib_tws_api_test.gleam` pass, but they only verify:
- Client ID generation
- Handshake version string generation
- TCP connection establishment
- Message encoding
- Message transmission

Tests do NOT verify that TWS actually responds to our messages.

## Current Issue: TWS Not Responding on Port 7496

### Actual Behavior

**Port 7496 (Live Trading):**
- TWS is listening (verified by netstat)
- TCP connections establish successfully
- Handshake messages are sent correctly (verified by FFI fix)
- **TWS does NOT respond to any messages**
- Connections close within 1-57 milliseconds without any TWS acknowledgment

**Port 7497 (Paper Trading):**
- TWS is NOT listening (verified by netstat)
- No paper trading TWS instance is running

### What We've Verified Working

1. ✅ **TCP Connection Establishment**
   - Connections to port 7496 establish successfully
   - Socket is ready and accepts connections

2. ✅ **Message Encoding** 
   - Handshake bytes are correct: `00000008763130302E323030000000C8`
   - Fixed FFI bug to send correct data instead of `00000000`
   - Length prefixes are properly encoded as 4-byte big-endian integers
   - UTF-8 string encoding works correctly

3. ✅ **Message Transmission**
   - All messages are sent successfully to TWS
   - No socket errors during transmission

### ❌ What's NOT Working

1. **TWS Does Not Respond**
   - No server handshake received from TWS
   - No `nextValidId` event
   - No error messages from TWS
   - All `received_data` lists are empty

2. **Paper Trading Not Available**
   - Port 7497 is not listening
   - Cannot test paper trading connection

## Previous Discoveries

### Critical FFI Bug (FIXED ✅)

**Discovery:** Handshake messages were being sent as `00000000` instead of correct data

**Root Cause:** In [`src/connection_ffi.mjs`](../src/connection_ffi.mjs:24), the `send_bytes` function was using `Buffer.from(data)` to convert Gleam's BitArray to a Buffer. However, Gleam passes BitArray as an object with a `rawBuffer` property containing the actual Uint8Array of bytes. Converting the entire object resulted in an empty buffer.

**Fix:** Changed to `Buffer.from(data.rawBuffer)`

**Verification:**
- Before fix: Buffer hex = `00000000`
- After fix: Buffer hex = `41504900` (correct "API\0" header)

## Protocol Analysis

### Current Handshake Implementation

Based on IB TWS API V100+ protocol:

1. **Client sends:**
   - API version string (e.g., "v100.200")
   - 4-byte big-endian length prefix
   - API version integer (e.g., 200)

2. **Server should respond with:**
   - Server version
   - Server time
   - Connection ready acknowledgment

**Current Implementation:**
```gleam
// Handshake message format:
<<length:32-size, version_string:utf8>>
<<api_version:32>>
```

**Example:**
- Version string: "v100.200" (8 bytes)
- Length prefix: 00000008
- API version: 000000C8 (200 in hex)
- Total: 16 bytes

### Next Steps for Investigation

1. **Verify TWS Configuration on Port 7496**
   - Check if TWS is configured to accept API connections
   - Verify "Enable ActiveX and Socket Clients" is enabled
   - Check TWS logs for connection attempts
   - Look for any error messages or warnings

2. **Protocol Verification**
   - Compare with known working IB API clients
   - Test with official IB API Python/Java samples
   - Verify handshake sequence matches TWS expectations

3. **Message Format Review**
   - Double-check message encoding (big-endian vs little-endian)
   - Verify string encoding (UTF-8 vs ASCII)
   - Check for required additional handshake fields

## Test Philosophy

We follow **fact-discovery testing**:

- ✅ Connect to REAL TWS instances
- ✅ Send REAL messages
- ✅ Discover FACTS through actual responses
- ❌ NO fake data or hardcoded responses
- ❌ NO cheating the compiler with trivial tests

Each test must verify actual functionality, not just compile-time correctness.

## References

- IB TWS API Documentation: https://www.interactivebrokers.com/en/software/api/apiguide/
- TWS API V100+ Protocol: Handshake and message format specifications