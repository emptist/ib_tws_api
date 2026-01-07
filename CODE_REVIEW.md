# Code Review and Refactoring Recommendations

## Overview
This document provides a comprehensive review of the IB TWS API codebase and identifies opportunities for refactoring and improvements.

## Code Quality Assessment

### ✅ Strengths
1. **Clean module organization** - Clear separation of concerns (connection, protocol, market_data, orders, account_data)
2. **Type safety** - Strong use of Gleam's type system for safety (AccountType, OrderType, etc.)
3. **Documentation** - Good documentation with examples and technical notes
4. **Safety features** - Trading safety with AccountType system
5. **Comprehensive testing** - Multiple test files covering different scenarios
6. **FFI integration** - Clean integration with Node.js for JavaScript-specific functionality

### ⚠️ Areas for Improvement

## 1. Connection Module (connection.gleam)

### Issue 1.1: Non-functional `on_data()` function
**Location:** `src/connection.gleam:201-214`

**Problem:** The `on_data()` function doesn't actually work - it just prints a warning. The callback mechanism is only functional when passed to `connect_with_callback()`.

**Current Code:**
```gleam
pub fn on_data(
  _conn: Connection,
  _callback: DataCallback,
) -> Result(Nil, ConnectionError) {
  io.println(
    "[Connection] Warning: on_data() called after connect(). "
    <> "Callback will only work for future connections.",
  )
  Ok(Nil)
}
```

**Recommendation:** Either:
- **Option A:** Remove this function entirely since it doesn't work
- **Option B:** Implement proper state updating to allow callback registration after connection
- **Option C:** Update documentation to clearly state this function is deprecated

**Priority:** Medium - This could confuse users who expect it to work

---

### Issue 1.2: ConnectionState.received_data is a List but only stores one item
**Location:** `src/connection.gleam:10-18`

**Problem:** `received_data` is typed as `List(String)` but the implementation only keeps the most recent data at the front of the list. The old data is never cleared or used.

**Current Code:**
```gleam
ConnectionState(
  connected: Bool,
  received_data: List(String),  // List but only stores most recent
  error: option.Option(String),
  should_close: Bool,
  on_data_callback: option.Option(DataCallback),
)
```

**Recommendation:** Change to `received_data: option.Option(String)` to match actual usage.

**Priority:** Low - Doesn't affect functionality, just type accuracy

---

### Issue 1.3: Debug logging mixed with production code
**Location:** Throughout `connection.gleam`

**Problem:** Timestamp logging and debug output are always printed, even in production use.

**Examples:**
```gleam
let timestamp = get_timestamp()
io.println("[Connection " <> timestamp <> "] Socket ready")
```

**Recommendation:** Add a `debug_mode` parameter to ConnectionConfig to control logging output.

**Priority:** Medium - Would make the library more production-ready

---

## 2. Protocol Module (protocol.gleam)

### Issue 2.1: Incomplete `parse_message()` function
**Location:** `src/protocol.gleam:158-171`

**Problem:** The `parse_message()` function is incomplete and always returns `Ok(StartApi)` regardless of input.

**Current Code:**
```gleam
pub fn parse_message(data: BitArray) -> Result(MessageCode, String) {
  let size = bit_array.byte_size(data)
  case size {
    0 -> Error("Empty message")
    _ -> {
      // Check for known message patterns
      // This is a simplified parser - will be expanded as needed
      Ok(StartApi)  // Always returns StartApi!
    }
  }
}
```

**Recommendation:** Either:
- **Option A:** Implement proper message parsing based on message code (first 2 bytes)
- **Option B:** Remove this function until it's properly implemented
- **Option C:** Add `TODO` comment explaining it's not yet implemented

**Priority:** High - This function is misleading and doesn't work

---

### Issue 2.2: Debug logging in `start_api_message()`
**Location:** `src/protocol.gleam:71-90`

**Problem:** The function prints debug information every time it's called.

**Recommendation:** Add a `debug` parameter or use a logging configuration.

**Priority:** Low - Useful for development but should be optional in production

---

### Issue 2.3: Unused MessageCode and Message types
**Location:** `src/protocol.gleam:11-35`

**Problem:** `MessageCode` and `Message` types are defined but not fully utilized in the codebase.

**Recommendation:** Either implement proper message handling using these types, or move them to a separate `message_types.gleam` module for future use.

**Priority:** Low - Not causing issues but incomplete

---

## 3. Market Data Module (market_data.gleam)

### Issue 3.1: Hardcoded API version
**Location:** `src/market_data.gleam:27-40`

**Problem:** The `request_market_data()` function hardcodes version to 9, which may not be correct for all API versions.

**Current Code:**
```gleam
<<
  1:16,
  9:32,  // Hardcoded version
  ticker_id:32,
  ...
>>
```

**Recommendation:** Add a `version` parameter with a sensible default.

**Priority:** Medium - May cause issues with different TWS versions

---

## 4. Orders Module (orders.gleam)

### Issue 4.1: Unused `OrderSide` type
**Location:** `src/orders.gleam:15-19`

**Problem:** `OrderSide` type is defined but never used. Only `OrderAction` is used.

**Current Code:**
```gleam
pub type OrderSide {
  Buy
  Sell
}

pub type OrderAction {
  BuyAction
  SellAction
  ShortAction
}
```

**Recommendation:** Remove the unused `OrderSide` type to avoid confusion.

**Priority:** Low - Just code cleanup

---

### Issue 4.2: Hardcoded API version
**Location:** `src/orders.gleam:80-94`

**Problem:** The `place_order()` function hardcodes version to 45.

**Current Code:**
```gleam
<<
  5:16,
  45:32,  // Hardcoded version
  order_id:32,
  ...
>>
```

**Recommendation:** Add a `version` parameter with a sensible default.

**Priority:** Medium - May cause issues with different TWS versions

---

## 5. Account Data Module (account_data.gleam)

**Assessment:** This module is well-structured and doesn't have significant issues. The code is clean, well-documented, and follows good practices.

**Minor suggestions:**
- Consider adding validation for `req_id` to ensure it's positive
- The `common_account_tags()` function is useful and well-designed

---

## 6. FFI Module (connection_ffi.mjs)

### Issue 6.1: Assumption about BitArray.rawBuffer
**Location:** `src/connection_ffi.mjs:33`

**Problem:** The `send_bytes()` function assumes `data.rawBuffer` exists. This might not be the correct property name in all Gleam versions or JavaScript runtimes.

**Current Code:**
```javascript
const bytes = data.rawBuffer;
```

**Recommendation:** Add error handling and check for the correct property name. Test with different Gleam versions.

**Priority:** High - Could cause runtime errors

---

### Issue 6.2: Verbose port detection logging
**Location:** `src/connection_ffi.mjs:103-134`

**Problem:** The `detect_ib_tws_port()` function logs every step, which may be too verbose for production use.

**Recommendation:** Add a `verbose` parameter to control logging output.

**Priority:** Low - Useful for debugging but should be optional

---

## General Recommendations

### 1. Add Configuration Module
**Recommendation:** Create a `config.gleam` module to centralize configuration options like:
- API versions
- Default timeouts
- Debug mode settings
- Logging configuration

**Priority:** Medium - Would improve maintainability

---

### 2. Improve Error Handling
**Recommendation:** Add more specific error types:
- `ProtocolError` for protocol-level issues
- `AuthenticationError` for client ID issues
- `TimeoutError` for connection timeouts
- `ValidationError` for invalid parameters

**Priority:** Medium - Would improve debugging

---

### 3. Add Logging Module
**Recommendation:** Create a `logger.gleam` module to provide:
- Configurable log levels (debug, info, warn, error)
- Consistent log formatting
- Optional timestamp inclusion

**Priority:** Low - Would improve code quality

---

### 4. Add Integration Tests
**Recommendation:** Create integration tests that:
- Test the full connection flow
- Test message sending and receiving
- Test error scenarios
- Test with both paper and live trading accounts (read-only)

**Priority:** Medium - Would improve reliability

---

### 5. Document API Version Compatibility
**Recommendation:** Create a document (`API_VERSIONS.md`) that:
- Lists which API versions have been tested
- Documents any known compatibility issues
- Provides upgrade paths when TWS updates

**Priority:** Low - Would help users

---

## Refactoring Priority Summary

### High Priority (Should Fix Soon)
1. Fix incomplete `parse_message()` function in protocol.gleam
2. Verify `send_bytes()` FFI implementation for BitArray access

### Medium Priority (Should Consider)
3. Add debug mode configuration to control logging
4. Make API versions configurable instead of hardcoded
5. Remove or fix non-functional `on_data()` function
6. Improve error handling with specific error types

### Low Priority (Nice to Have)
7. Remove unused `OrderSide` type
8. Add logging module
9. Create configuration module
10. Add integration tests
11. Document API version compatibility
12. Fix `ConnectionState.received_data` type

---

## Conclusion

The codebase is in good shape with solid foundations. The main areas for improvement are:

1. **Completing incomplete features** (like `parse_message()`)
2. **Adding configurability** (debug mode, API versions)
3. **Improving robustness** (error handling, validation)
4. **Code cleanup** (remove unused code, fix types)

None of these issues are critical - the code works as intended. The recommendations above are for making the library more maintainable, production-ready, and user-friendly.

## Next Steps

1. Review these recommendations with the team
2. Prioritize based on user needs and development goals
3. Implement high-priority fixes first
4. Gradually address medium and low priority items
5. Update documentation as changes are made