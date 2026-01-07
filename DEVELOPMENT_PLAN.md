# IB TWS API Wrapper - Development Plan

## Overview
Building a Gleam language wrapper for the Interactive Brokers TWS API, targeting JavaScript. The development follows a bottom-up approach, implementing features only when naturally needed.

**Note**: This plan is continuously updated based on actual development progress and discoveries. See [`TECHNICAL_NOTES.md`](TECHNICAL_NOTES.md) for detailed technical issues and lessons learned.

## Project Goals
1. Create a minimal viable connection to IB TWS API
2. Receive raw data from the API
3. Gradually add features as needed
4. Maintain working functionality at every step
5. Consistent documentation and git commits
6. Document all issues for future Erlang target implementation

## Configuration
- **Paper Trading Port**: 7497 (for development and testing)
- **Live Trading Port**: 7496 (never use for buy/sell operations during development)
- **Protocol**: TCP socket-based communication
- **Target**: JavaScript (Node.js runtime)
- **Future Target**: Erlang (using lessons learned from JavaScript implementation)

## Development Phases

### Phase 1: Foundation ✅ COMPLETED
**Goal**: Establish basic project structure and minimal connectivity

#### Step 1: Project Setup ✅
- [x] Create development plan document
- [x] Update gleam.toml to target JavaScript
- [x] Set up project structure with proper directories
- [x] Create initial documentation

#### Step 2: Minimal TCP Connection ✅
- [x] Implement basic TCP socket connection using node_socket_client
- [x] Connect to IB TWS API on port 7497
- [x] Receive raw data stream via event handlers
- [x] Create test to verify connection and print raw data
- [x] Implement JavaScript FFI for Node.js integration
- [x] Commit with message: "feat: implement minimal TCP connection to IB TWS API"

**Success Criteria**: Can connect to TWS API and receive/print raw bytes

#### Step 2.5: Automatic Port Switching with Trading Safety ✅
- [x] Add `AccountType` type with three variants for different safety levels:
  - `PaperTrading`: port 7497, trading allowed (development)
  - `LiveTradingReadOnly`: port 7496, NO TRADING (development safety)
  - `LiveTrading`: port 7496, trading allowed (production only)
- [x] Implement `is_trading_allowed()` function to check trading permissions
- [x] Implement `get_port_for_account_type()` helper function
- [x] Implement `config_with_account_type()` for automatic port detection
- [x] Keep `config()` function for explicit port specification
- [x] Create comprehensive test to verify port switching and safety features
- [x] Update all test files to use appropriate account types
- [x] Commit with message: "feat: add type-level trading safety with three account types"

#### Step 2.6: Automatic Port Detection ✅
- [x] **Discovery**: Users switch between paper trading (7497) and live trading (7496) throughout the day
- [x] Implement `detect_ib_tws_port()` function using nc (netcat) command
- [x] Implement `config_auto_detect()` convenience function for easy usage
- [x] Use execSync from Node.js child_process for synchronous port checking
- [x] Return Int type (0 = not found, other = port number) to avoid FFI marshaling issues
- [x] Check paper trading port (7497) first, then live trading port (7496)
- [x] Add comprehensive logging for debugging port detection process
- [x] Create test/auto_port_detection_test.gleam to verify functionality
- [x] Test successfully detects port 7496 (live trading) and connects to it
- [x] Update README with automatic port detection usage examples
- [x] Update TECHNICAL_NOTES.md with implementation details
- [x] Commit with message: "feat: add automatic port detection for IB TWS API"

**Success Criteria**:
- ✓ Automatically select correct port based on account type
- ✓ Type-level safety prevents accidental trading on live account during development
- ✓ Clear distinction between development and production modes
- ✓ Automatically detect which IB TWS port (7496 or 7497) is available
- ✓ Handle case when neither port is available with clear error message
- ✓ Useful for switching between paper trading (day) and live trading (night)

### Phase 2: Protocol Handshake ✅ COMPLETED
**Goal**: Implement the initial handshake protocol

#### Step 3: Research IB TWS Handshake Protocol ✅
- [x] Study IB TWS API V100+ protocol documentation
- [x] Understand handshake message format: "API\0" + 4-byte length + version string
- [x] Document protocol requirements

#### Step 4: Implement Correct V100+ Handshake ✅
- [x] Implement `start_api_message()` function with correct format
- [x] Implement `int_to_four_bytes_big_endian()` for proper byte encoding
- [x] Create `client_id_message()` for separate client ID transmission
- [x] Implement `parse_server_response()` to parse "VERSION<timestamp> EST" format
- [x] Add debug logging for byte-level inspection

#### Step 5: Test Handshake and Verify Server Response ✅
- [x] Create test that sends handshake and receives server response
- [x] Verify server response format: "20020260107 08:02:02 EST"
- [x] Parse version number and timestamp correctly
- [x] Confirm handshake works with paper trading account

#### Step 6: Parse and Display Server Response Data ✅
- [x] Implement server response parsing
- [x] Display version and timestamp in test output
- [x] Handle parsing errors gracefully

#### Step 7: Commit Working Handshake Implementation ✅
- [x] Commit with message: "feat: implement IB TWS V100+ handshake protocol"

#### Step 8: Fix Client ID Message ✅
- [x] **Discovery**: Client ID must be sent as SEPARATE message after server response
- [x] Not part of handshake message itself
- [x] Update test to send client ID after receiving server response
- [x] Document this critical protocol requirement

#### Step 9: Fix Message Handler Type Errors ✅
- [x] **Discovery**: Type mismatches in message handler due to incorrect assumptions
- [x] Fixed `filled`, `avg_fill`, `remaining` types in `on_order_status`
- [x] Fixed `account` type in `on_position` handler
- [x] Updated type definitions to match actual IB TWS protocol
- [x] Commit with message: "Fix type errors in message_handler.gleam"

**Success Criteria**: Successful handshake, receive server time, send client ID as separate message

### Phase 3: Async Message Handling 🔄 IN PROGRESS
**Goal**: Implement proper asynchronous message processing

#### Step 10: Test Handshake on Live Account (Port 7496) ✅ COMPLETED
- [x] Run handshake test on live account (port 7496)
- [x] Use `LiveTradingReadOnly` type for safety (trading blocked)
- [x] Verify handshake message format: "API\0" + length + version
- [x] Confirm server response received: "20020260107 08:50:31 EST"
- [x] Parse version (200) and timestamp correctly
- [x] **Discovery**: Data arrives asynchronously via event handlers
- [x] Data appears AFTER test timeout completes (async timing issue)
- [x] Handshake works correctly on both paper (7497) and live (7496) accounts
- [x] Commit with message: "test: verify handshake on live account port 7496"

**Success Criteria**:
- ✓ Handshake works on live account (port 7496)
- ✓ Server response received and parsed correctly
- ✓ Version: 200, Timestamp: 20260107 08:50:31 EST
- ✓ Trading blocked by LiveTradingReadOnly type (development safety)
- ✓ Connection closes gracefully

#### Step 11: Implement Proper Async Message Handling with Event Callbacks ⏳
- [ ] **Current Issue**: `sleep()` doesn't block in JavaScript runtime
- [ ] Data arrives asynchronously via event handlers
- [ ] `receive()` polls stored data but timing issues occur
- [ ] **Solution Needed**: Implement message queue with callback processing
- [ ] Use event-driven architecture instead of polling
- [ ] Consider using `gleam/javascript/promise` for proper async handling
- [ ] Create improved test that properly handles async messages
- [ ] Update documentation on async patterns
- [ ] Commit with message: "feat: implement async message handling"

#### Step 12: Implement Message Parsing for Common Messages ⏳
- [ ] Parse error messages (message code 4)
- [ ] Parse tick price messages (message code 1)
- [ ] Parse tick size messages (message code 2)
- [ ] Parse order status messages (message code 9)
- [ ] Parse position messages (message code 61)
- [ ] Create tests for each message type
- [ ] Update message handler to dispatch parsed messages
- [ ] Commit with message: "feat: add message parsing for common message types"

#### Step 13: Add Support for Market Data Requests ⏳
- [ ] Implement `req_mkt_data()` function
- [ ] Create Contract type definition
- [ ] Handle market data callbacks via message handler
- [ ] Create test that subscribes to a stock and prints data
- [ ] Parse tick price and tick size messages
- [ ] Commit with message: "feat: implement market data subscription"

**Success Criteria**: Can subscribe to a stock and receive quotes via event handlers

### Phase 4: Order Management (Paper Trading Only)
**Goal**: Implement order placement and management

#### Step 14: Add Order Placement Functionality ⏳
- [ ] Define Order type with all required fields
- [ ] Implement `place_order()` function (paper trading only)
- [ ] Handle order status callbacks
- [ ] Implement order cancellation
- [ ] Create tests for order operations (paper account only)
- [ ] **WARNING**: Never test on live account (port 7496)
- [ ] Commit with message: "feat: implement order management"

#### Step 15: Add Position and Account Data Retrieval ⏳
- [ ] Implement account summary request
- [ ] Parse account data messages
- [ ] Implement portfolio positions request
- [ ] Handle position data callbacks
- [ ] Create tests for account information
- [ ] Commit with message: "feat: implement account information retrieval"

**Success Criteria**: Can place/cancel orders on paper account, retrieve positions

### Phase 5: Advanced Features
**Goal**: Add remaining API features as needed

#### Step 15: Historical Data ⏳
- [ ] Implement historical data requests
- [ ] Parse historical data responses
- [ ] Commit with message: "feat: add historical data support"

#### Step 16: Real-time Bars ⏳
- [ ] Implement real-time bar subscriptions
- [ ] Commit with message: "feat: add real-time bar support"

### Phase 6: Documentation and Examples
**Goal**: Complete documentation and usage examples

#### Step 17: Document All Implemented Features ⏳
- [ ] Update README with current capabilities
- [ ] Document API usage with examples
- [ ] Add troubleshooting section
- [ ] Document configuration requirements
- [ ] Commit with message: "docs: update documentation"

#### Step 18: Create Comprehensive Examples ⏳
- [ ] Create example: Basic connection and handshake
- [ ] Create example: Market data subscription
- [ ] Create example: Order placement (paper trading)
- [ ] Create example: Account information retrieval
- [ ] Commit with message: "docs: add usage examples"

**Success Criteria**: Complete documentation with working examples

## Development Principles

1. **Bottom-Up Development**: Only implement features when naturally needed
2. **Working Code**: Every commit must have working, tested functionality
3. **Incremental**: Build on previous work without breaking existing features
4. **Documentation First**: Update docs as features are added
5. **Git Discipline**: Commit frequently with clear messages
6. **Test Coverage**: Every feature must have tests
7. **Safety First**: Never test buy/sell on live account (port 7496)
8. **Document Issues**: Record all technical issues for future reference

## Testing Strategy

- Unit tests for all pure functions
- Integration tests for API communication (paper account only)
- Manual testing with TWS Gateway running
- Separate test files for paper (7497) and live (7496) accounts
- Never automate tests on live account
- Use clear warnings in live account test code

## File Structure

```
ib_tws_api/
├── src/
│   ├── ib_tws_api.gleam          # Main module (placeholder)
│   ├── connection.gleam           # TCP connection handling
│   ├── connection_ffi.mjs         # JavaScript FFI for Node.js
│   ├── protocol.gleam             # Message protocol implementation
│   ├── message_handler.gleam      # Message processing and callbacks
│   ├── types.gleam                # Core type definitions (future)
│   ├── market_data.gleam          # Market data operations (future)
│   ├── orders.gleam               # Order management (future)
│   └── account.gleam              # Account information (future)
├── test/
│   ├── ib_tws_api_test.gleam      # Original paper trading test
│   ├── paper_account_test.gleam     # Paper trading test (automatic port 7497)
│   ├── live_account_test.gleam      # Live account test (automatic port 7496) - NO BUY/SELL
│   ├── auto_port_test.gleam         # Test automatic port switching
│   ├── improved_handshake_test.gleam  # Improved async test
│   └── diagnostic_test.gleam        # Diagnostic/testing utilities
├── examples/
│   └── (to be created as needed)
├── DEVELOPMENT_PLAN.md            # This file
├── TECHNICAL_NOTES.md            # Detailed technical issues and lessons
├── README.md                      # Project documentation
└── gleam.toml                     # Project configuration
```

## Dependencies

Current dependencies:
- gleam_stdlib (standard library)
- gleeunit (testing)
- node_socket_client (TCP socket bindings for JavaScript target)

Potential future additions (as needed):
- gleam/javascript for additional JavaScript interop
- JSON parsing libraries (if needed)

## Critical Technical Discoveries

### 1. Client ID Must Be Separate Message
The client ID must be sent as a separate message AFTER receiving the server's initial response, not as part of the handshake itself. See [`TECHNICAL_NOTES.md`](TECHNICAL_NOTES.md#1-client-id-message-must-be-separate).

### 2. Asynchronous Data Reception Pattern
In JavaScript runtime, socket data arrives through event callbacks. The `sleep()` function returns a Promise but doesn't block execution. Event-driven architecture is required. See [`TECHNICAL_NOTES.md`](TECHNICAL_NOTES.md#2-asynchronous-data-reception-pattern).

### 3. Type System Challenges
IB TWS message fields have specific types that must match the actual protocol. Always verify against IB API documentation. See [`TECHNICAL_NOTES.md`](TECHNICAL_NOTES.md#3-type-system-challenges).

### 4. Big-Endian Byte Encoding
All multi-byte integers must be big-endian (network byte order). See [`TECHNICAL_NOTES.md`](TECHNICAL_NOTES.md#6-big-endian-byte-encoding).

## Lessons for Erlang Target

Key takeaways for future Erlang implementation:
- Protocol understanding is identical across runtimes
- Erlang's actor model is naturally suited for this
- Use GenServer for connection management
- Type specifications (Dialyzer) help catch errors
- Erlang's `timer:sleep/1` actually blocks
- Binary handling is similar to BitArray
- Use `gen_tcp` module for TCP connections

See [`TECHNICAL_NOTES.md`](TECHNICAL_NOTES.md#lessons-for-erlang-target) for detailed guidance.

## Current Status

### Working Features:
- ✅ TCP connection establishment
- ✅ V100+ handshake protocol implementation
- ✅ Server response parsing
- ✅ Client ID message sending
- ✅ Basic message handler framework
- ✅ Type-safe message definitions
- ✅ Comprehensive technical documentation
- ✅ Automatic port switching based on account type
- ✅ Automatic port detection (7496 or 7497) using nc command
- ✅ Type-level trading safety with three account modes
- ✅ `is_trading_allowed()` function for runtime safety checks
- ✅ Separate test files for paper and live accounts
- ✅ Handshake verified on both paper (7497) and live (7496) accounts
- ✅ Server response format confirmed: "VERSION<timestamp> EST"
- ✅ Message parsing for error, tick price, tick size, order status, position, and account summary messages
- ✅ Market data subscription functionality
- ✅ Order placement (paper trading only) with safety checks
- ✅ Position and account data retrieval
- ✅ 25 account summary tags implemented
- ✅ 4 comprehensive examples created

### Known Limitations:
- ⚠️ Asynchronous data reception uses callbacks (working but could be improved)
- ⚠️ Limited message types implemented (error, tick price, tick size, order status, position, account summary)
- ⚠️ No message queue for buffering (callbacks handle messages directly)
- ⚠️ Sleep doesn't block (JavaScript runtime) - use event-driven patterns

### Next Immediate Steps:
1. ✅ **COMPLETED**: Test handshake on live account (port 7496) - SUCCESS!
2. ✅ **COMPLETED**: Implement proper event-driven message handling with callbacks
3. ✅ **COMPLETED**: Implement message parsing for common IB TWS messages
4. ✅ **COMPLETED**: Add market data request functionality
5. ✅ **COMPLETED**: Add order placement (paper trading only)
6. ✅ **COMPLETED**: Add position and account data retrieval
7. ✅ **COMPLETED**: Document all implemented features
8. ✅ **COMPLETED**: Create comprehensive examples
9. ✅ **COMPLETED**: Add automatic port detection
10. Add historical data requests
11. Add real-time bar subscriptions
12. Add more order types and advanced order features
13. Improve error handling and resilience

## Notes

- Use `config_auto_detect()` for automatic port detection (recommended for switching between paper/live)
- Use `config_with_account_type()` for automatic port selection based on account type
- Use `config()` only when custom port is needed
- PaperTrading automatically uses port 7497 (for development)
- LiveTrading automatically uses port 7496 (never test buy/sell)
- Automatic port detection checks paper trading port (7497) first, then live trading port (7496)
- Port detection uses nc (netcat) command with 1-second timeout by default
- The IB TWS API documentation will be the primary reference
- Protocol version will be tracked in connection module
- All technical issues are documented in TECHNICAL_NOTES.md
- This plan is updated as development progresses

## References

- IB TWS API Documentation: https://interactivebrokers.github.io/tws-api/
- IB API Version Numbers: API_VersionNum.txt (in IB documentation)
- Gleam Documentation: https://gleam.run/
- Gleam JavaScript FFI: https://gleam.run/writing-javascript-ffi/
- Technical Notes: [`TECHNICAL_NOTES.md`](TECHNICAL_NOTES.md)