# JavaScript FFI Async Issue

## Problem

Gleam's `@external(javascript, ...)` decorator does not automatically await Promises. When a JavaScript function returns a Promise, Gleam treats the Promise object itself as the return value, not the resolved value.

## Evidence

1. Error shows "Nil" when inspecting the Promise object
2. Connection succeeds (we see "[Node.js] Connected successfully" log)
3. The success happens AFTER the error is reported
4. This indicates Gleam is pattern matching on the Promise before it resolves

## Root Cause

In `src/ib_tws_api/socket.gleam`:

```gleam
@external(javascript, "../ffi/socket_ffi.mjs", "connect")
fn tcp_connect(host: String, port: Int) -> Result(Socket, Dynamic)
```

The JavaScript function returns a Promise:
```javascript
export function connect(host, port, callback) {
  return new Promise((resolve, reject) => {
    // async connection logic
  });
}
```

Gleam treats the Promise object as a value, not awaiting it.

## Attempts to Fix

1. ✗ Added resolved flag to prevent multiple resolutions
2. ✗ Changed to callback pattern - Gleam doesn't support this either
3. ✗ Tried Promise return type - Gleam doesn't have Promise type
4. ✗ Synchronous wrapper - Node.js doesn't support synchronous TCP

## Solutions

### Option 1: Use Erlang Target (Recommended)

Erlang has native async support through `gen_tcp` and `gen_server`. This is the proper way to handle async operations in Gleam.

**Action Items**:
1. Switch focus to `tryerl` branch for Erlang target
2. Implement Erlang FFI using gen_tcp
3. Test with live TWS instance
4. Verify all functionality works

### Option 2: Use JavaScript with Different Approach

Research if Gleam has async/await support or a different FFI pattern for async operations.

**Research Needed**:
- Check Gleam documentation for async FFI
- Look at how other Gleam projects handle async
- Check if there's a gleam_otp or similar package for async

### Option 3: Use JavaScript with Node.js Worker Threads

Use Node.js Worker Threads to run async operations synchronously from Gleam's perspective.

**Complexity**: High
**Feasibility**: Unknown

## Current Status

- ✅ Protocol implementation complete and tested (22 tests passing)
- ✅ Type system working correctly
- ✅ JavaScript FFI functions implemented correctly
- ✅ Socket connection works at JavaScript level
- ❌ Gleam JavaScript FFI cannot handle async Promises
- ⏸️ Blocked on async/await support in Gleam JavaScript FFI

## Recommendation

**Proceed with Erlang target implementation**. Erlang has native async support and is the intended platform for this type of I/O operation.

## Next Steps

1. Create `tryerl` branch from master
2. Implement Erlang FFI for socket operations
3. Test connection with live TWS instance
4. Verify all protocol operations work
5. Document any differences between JavaScript and Erlang implementations

## Files Affected

- `src/ib_tws_api/socket.gleam` - FFI declarations
- `native/ib_tws_api/ffi/socket_ffi.mjs` - JavaScript implementation
- `src/ib_tws_api/test_simple_connection.gleam` - Test file

## References

- Gleam Documentation: https://gleam.run/
- Gleam JavaScript FFI: https://tour.gleam.run/#/javascript-interop
- Node.js net module: https://nodejs.org/api/net.html