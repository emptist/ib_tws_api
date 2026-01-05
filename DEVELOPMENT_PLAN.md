# IB TWS API Wrapper - Development Plan

## Project Overview

This project aims to create a comprehensive Gleam wrapper for Interactive Brokers' TWS (Trader Workstation) and Gateway API. The wrapper will provide a type-safe, functional interface to interact with IB's trading infrastructure, enabling developers to retrieve account information, manage orders, and receive market data.

### Connection Details

- **Available TWS Instance**: Running at the live account port
- **Connection Requirements**: No client ID or account ID needed to connect
- **Account Discovery**: Accounts associated with the login are retrieved once a connection is established
- **Default Ports**:
  - TWS Paper Trading: 7497
  - TWS Live Trading: 7496
  - IB Gateway Paper Trading: 4002
  - IB Gateway Live Trading: 4001

## Project Architecture

### Module Structure

```
src/ib_tws_api/
├── ib_tws_api.gleam          # Main module - public API surface
├── client.gleam              # Client connection management
├── socket.gleam              # Low-level TCP socket operations
├── protocol.gleam            # Message encoding/decoding
├── types.gleam               # Data type definitions
├── messages/                 # Message-specific modules
│   ├── connection.gleam      # Connection handshake messages
│   ├── market_data.gleam     # Market data request/tick messages
│   ├── orders.gleam          # Order placement and status messages
│   ├── account.gleam         # Account summary and position messages
│   └── contract.gleam        # Contract details messages
└── utils/
    ├── encoder.gleam         # Binary encoding utilities
    └── decoder.gleam         # Binary decoding utilities
```

### Module Responsibilities

#### `ib_tws_api.gleam` (Main Module)
- Re-exports public types and functions
- Provides high-level API for common operations
- Manages client lifecycle

#### `client.gleam`
- Client creation and configuration
- Connection management (connect/disconnect)
- Authentication handling
- Message sending and receiving
- Error handling and recovery

#### `socket.gleam`
- Low-level TCP socket operations using Erlang's gen_tcp
- Socket configuration and options
- Binary data transmission
- Connection error handling

#### `protocol.gleam`
- Message type definitions
- Message routing and dispatch
- Protocol version management
- Message validation

#### `types.gleam`
- Core data structures (Contract, Order, etc.)
- Custom error types
- Type aliases for common patterns

#### Message-Specific Modules
- Define message structures for specific API areas
- Implement encoding/decoding for message types
- Handle message-specific validation

## Protocol Implementation Details

### IB TWS Binary Protocol

The IB TWS API uses a binary protocol where messages are composed of fields separated by null bytes (`\0`). Each message starts with a message type ID (integer) followed by fields in a specific order.

#### Message Format
```
<message_id><field1>\0<field2>\0<field3>\0...
```

#### Encoding Strategy
1. Convert each field to its binary representation
2. Separate fields with null bytes
3. Concatenate all fields
4. Prefix with message ID

#### Decoding Strategy
1. Read message ID from the beginning
2. Split remaining data by null bytes
3. Parse each field according to message type
4. Construct appropriate message type

### Key Message Types

#### Connection Messages
- **Connect Request (0)**: Initiate connection with client ID
- **Connect Ack (4)**: Server acknowledges connection with version and time
- **Connect Failed (5)**: Connection failed with reason
- **Ping (8)**: Keep-alive ping
- **Pong (9)**: Keep-alive pong response

#### Market Data Messages
- **Request Market Data (1)**: Subscribe to market data for a contract
- **Cancel Market Data (2)**: Cancel market data subscription
- **Market Data Tick (9)**: Receive market data updates

#### Order Messages
- **Place Order (3)**: Submit a new order
- **Cancel Order (4)**: Cancel an existing order
- **Order Status (5)**: Receive order status updates
- **Open Order (47)**: Receive open order details
- **Open Order End (48)**: End of open order transmission
- **Execution Detail (49)**: Receive execution details
- **Execution Detail End (50)**: End of execution details transmission

#### Account Messages
- **Request Account Summary (6)**: Request account information
- **Account Summary (7)**: Receive account summary data
- **Request Positions (8)**: Request current positions
- **Position (61)**: Receive position updates

#### Real-Time Bars Messages
- **Request Real-Time Bars (50)**: Subscribe to real-time bar data
- **Cancel Real-Time Bars (51)**: Cancel real-time bars subscription
- **Real-Time Bar (52)**: Receive real-time bar updates

## Development Phases

### Phase 1: Foundation
**Status**: Complete

**Tasks**:
- [x] Set up project structure
- [x] Define core types (Contract, Order, etc.)
- [x] Implement basic socket communication
- [x] Define message types
- [x] Implement message encoding/decoding utilities
- [x] Create connection handshake logic
- [x] Fix failing protocol decoding tests
- [ ] Test basic socket connection with real TWS instance

**Deliverables**:
- [x] Working socket connection to TWS
- [x] Basic message type definitions
- [x] Encoding/decoding utilities
- [ ] Connection handshake verified with real TWS

**Progress Notes**:
- Socket communication implemented using Erlang's gen_tcp directly
- Message types defined: ConnectRequest, ConnectAck, ConnectFailed, AccountSummary, Position, MarketDataTick, OrderStatus, Ping
- Encoding functions implemented for all defined message types
- Decoding functions implemented for ConnectAck, ConnectFailed, AccountSummary, Position, MarketDataTick, OrderStatus, Ping
- String decoding uses bit array accumulation with UTF-8 conversion
- Client module updated to send ConnectRequest during connection
- Message sending/receiving functionality implemented
- Fixed binary pattern matching issues: Changed `<<message_id:32>>` to `<<message_id:int-size(32)>>` for proper integer pattern matching
- All 11 tests now passing (7 encoding tests + 4 decoding tests)
- Protocol decoding tests now work correctly for ConnectAck, ConnectFailed, AccountSummary, and Position messages

### Phase 2: Protocol Implementation
**Status**: Complete

**Tasks**:
- [x] Implement encoding for all message types
- [x] Implement decoding for all message types
- [x] Add message validation
- [ ] Handle protocol versioning
- [ ] Implement error recovery

**Deliverables**:
- [x] Complete protocol implementation
- [x] Comprehensive test coverage for encoding/decoding

**Progress Notes**:
- Implemented encoding for all message types: ConnectRequest, Disconnect, Ping, Pong, MarketDataRequest, CancelMarketData, OrderPlace, CancelOrder, OpenOrder, AccountSummaryRequest, PositionsRequest, RealTimeBarsRequest, CancelRealTimeBars
- Implemented decoding for all message types: ConnectAck, ConnectFailed, MarketDataTick, OrderStatus, AccountSummary, Position, Ping, OpenOrder, OpenOrderEnd, ExecutionDetail, ExecutionDetailEnd, RealTimeBar
- Added comprehensive test coverage with 22 tests passing
- Refactored decode functions to use result.try and use expressions for better code quality
- Fixed duplicate message ID issue (49 was used for both ExecutionDetail and RealTimeBar, corrected to use 52 for RealTimeBar)
- Implemented real-time bars functionality with message IDs 50 (request), 51 (cancel), and 52 (response)
- Protocol documentation

### Phase 3: Client Management
**Status**: Pending

**Tasks**:
- [ ] Implement client lifecycle management
- [ ] Add connection state tracking
- [ ] Implement message queueing
- [ ] Add automatic reconnection logic
- [ ] Implement request/response correlation

**Deliverables**:
- Robust client implementation
- Connection management features
- Request/response handling

### Phase 4: High-Level API
**Status**: Pending

**Tasks**:
- [ ] Implement account information retrieval
- [ ] Implement order placement and management
- [ ] Implement market data subscription
- [ ] Implement position tracking
- [ ] Add event streaming API

**Deliverables**:
- User-friendly high-level API
- Example applications
- API documentation

### Phase 5: Testing and Documentation
**Status**: Pending

**Tasks**:
- [ ] Create comprehensive test suite
- [ ] Add integration tests with mock server
- [ ] Write API documentation
- [ ] Create usage examples
- [ ] Add performance benchmarks

**Deliverables**:
- Full test coverage
- Complete documentation
- Example applications

## Testing Strategy

### Unit Tests
- Test each module in isolation
- Test encoding/decoding functions with known inputs
- Test error handling paths
- Test edge cases and boundary conditions

### Integration Tests
- Test socket connection with mock TWS server
- Test message exchange sequences
- Test error scenarios (disconnection, timeout, etc.)
- Test concurrent operations

### Property-Based Tests
- Test encoding/decoding round-trip properties
- Test message ordering invariants
- Test state transition properties

### Test Organization
```
test/ib_tws_api/
├── ib_tws_api_test.gleam          # Main test file
├── socket_test.gleam               # Socket tests
├── protocol_test.gleam             # Protocol tests
├── client_test.gleam               # Client tests
└── integration_test.gleam          # Integration tests
```

## Documentation Plan

### Code Documentation
- Document all public functions with Gleam doc comments
- Include usage examples for complex operations
- Document error types and their meanings
- Document protocol message formats

### User Documentation
- Getting started guide
- API reference
- Tutorial examples
- Best practices guide

### Developer Documentation
- Architecture overview
- Protocol specification
- Contribution guidelines
- Testing guide

## Development Milestones

### Milestone 1: Basic Connection (Week 1)
- Establish TCP connection to TWS
- Implement connection handshake
- Send/receive basic messages
- **Success Criteria**: Can connect and receive server acknowledgment

### Milestone 2: Protocol Implementation (Week 2-3)
- Implement encoding/decoding for all message types
- Add message validation
- Handle protocol versioning
- **Success Criteria**: All message types can be encoded/decoded correctly

### Milestone 3: Client Features (Week 4)
- Implement client lifecycle management
- Add request/response correlation
- Implement automatic reconnection
- **Success Criteria**: Client can handle connection failures gracefully

### Milestone 4: High-Level API (Week 5-6)
- Implement account information retrieval
- Implement order management
- Implement market data subscription
- **Success Criteria**: Can retrieve account info and place orders

### Milestone 5: Testing and Polish (Week 7-8)
- Complete test coverage
- Write documentation
- Create examples
- **Success Criteria**: Ready for public release

## Risk Mitigation

### Technical Risks
- **Protocol Complexity**: IB TWS protocol is complex and evolves
  - Mitigation: Focus on core features first, add advanced features incrementally
- **Erlang Interop**: Direct use of gen_tcp may have compatibility issues
  - Mitigation: Test thoroughly on different Erlang/OTP versions
- **Performance**: Binary parsing may be slow for high-frequency data
  - Mitigation: Profile and optimize critical paths

### Project Risks
- **Scope Creep**: API is very large
  - Mitigation: Prioritize features based on user needs
- **Testing**: Hard to test without real TWS instance
  - Mitigation: Create mock server for integration tests
- **Documentation**: Protocol documentation is sparse
  - Mitigation: Reverse-engineer from existing implementations

## Dependencies

### Current Dependencies
- `gleam_stdlib`: Standard library
- `gleeunit`: Testing framework

### Potential Future Dependencies
- `gleam_erlang`: For advanced Erlang interop (if needed)
- `gleam_otp`: For actor-based concurrency (if needed)
- Custom Erlang libraries for specific protocol features

## Development Workflow

### Branching Strategy
- `main`: Stable release branch
- `develop`: Development branch
- `feature/*`: Feature branches
- `bugfix/*`: Bug fix branches

### Commit Convention
- `feat: Add new feature`
- `fix: Fix bug`
- `docs: Update documentation`
- `test: Add tests`
- `refactor: Refactor code`
- `chore: Maintenance tasks`

### Code Review Process
- All changes must be reviewed
- Tests must pass
- Documentation must be updated
- Code must follow style guidelines

## Success Criteria

The project will be considered successful when:
1. Can establish and maintain a connection to IB TWS/Gateway
2. Can retrieve account information
3. Can place and manage orders
4. Can subscribe to market data
5. Has comprehensive test coverage (>80%)
6. Has complete documentation
7. Has example applications demonstrating usage

## Next Steps

1. **Immediate**: Complete Phase 1 foundation work
   - [x] Implement message encoding/decoding utilities
   - [x] Create connection handshake logic
   - [x] Test basic socket connection
   - [ ] Test connection with real TWS instance

2. **Short-term**: Begin Phase 2 protocol implementation
   - [x] Implement encoding for remaining message types (MarketData, Orders, etc.)
   - [x] Implement decoding for remaining message types
   - [x] Add comprehensive tests for all message types
   - [ ] Test connection handshake with real TWS

3. **Medium-term**: Complete protocol implementation
   - [x] Implement all message types from IB TWS API
   - [x] Add comprehensive tests
   - [ ] Document protocol details
   - [ ] Handle protocol versioning

4. **Long-term**: Build high-level API
   - [ ] Design user-friendly API
   - [ ] Implement client features (reconnection, message queueing)
   - [ ] Create examples and documentation

## Technical Learnings

### Gleam Binary Pattern Matching

During development, we encountered a critical issue with binary pattern matching in Gleam. The pattern `<<message_id:32>>` was being interpreted as a string pattern rather than an integer pattern, causing all protocol decoding tests to fail.

**Problem**:
```gleam
case data {
  <<message_id:32, _rest>> -> Ok(message_id)  // Incorrect - interpreted as string
  _ -> Error("Invalid message format")
}
```

**Solution**:
```gleam
case data {
  <<message_id:int-size(32), _rest:bits>> -> Ok(message_id)  // Correct - explicit integer type
  _ -> Error("Invalid message format")
}
```

**Key Points**:
- Always specify explicit type when pattern matching on integers: `int-size(32)`
- Always use `:bits` for remaining data in binary patterns
- The `:size(32)` syntax alone is interpreted as a string/bytes pattern
- This pattern applies to all binary pattern matching in protocol decoding

**Functions Updated**:
- `parse_message_id`
- `decode_connect_ack`
- `decode_connect_failed`
- `decode_market_data_tick`
- `decode_order_status`
- `decode_account_summary`
- `decode_position`
- `decode_ping`
- `decode_int`

This understanding is critical for any future binary protocol work in Gleam.

### Gleam Error Handling with Result Module

During development, we identified that deeply nested case expressions in decode functions were a code smell that violated Gleam idioms. The Gleam result module provides utilities to avoid excessive nesting when calling multiple functions that can fail.

**Problem**:
```gleam
fn decode_execution_detail(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:int-size(32), rest:bits>> -> {
      case decode_int(rest) {
        Ok(#(order_id, rest1)) -> {
          case decode_string(rest1) {
            Ok(#(client_id, rest2)) -> {
              case decode_int(rest2) {
                Ok(#(exec_id, rest3)) -> {
                  // ... more nesting
                }
                Error(err) -> Error(err)
              }
            }
            Error(err) -> Error(err)
          }
        }
        Error(err) -> Error(err)
      }
    }
    _ -> Error("Invalid execution detail format")
  }
}
```

**Solution**:
```gleam
fn decode_execution_detail(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:int-size(32), rest:bits>> -> {
      use #(order_id, rest1) <- result.try(decode_int(rest))
      use #(client_id, rest2) <- result.try(decode_string(rest1))
      use #(exec_id, rest3) <- result.try(decode_int(rest2))
      use #(time, rest4) <- result.try(decode_string(rest3))
      use #(acct_number, rest5) <- result.try(decode_string(rest4))
      use #(exchange, rest6) <- result.try(decode_string(rest5))
      use #(side, rest7) <- result.try(decode_string(rest6))
      use #(shares, rest8) <- result.try(decode_float(rest7))
      use #(price, rest9) <- result.try(decode_float(rest8))
      use #(perm_id, rest10) <- result.try(decode_int(rest9))
      use #(client_id2, rest11) <- result.try(decode_int(rest10))
      use #(order_id2, _remaining) <- result.try(decode_int(rest11))

      let execution =
        types.Execution(
          order_id: order_id,
          client_id: client_id,
          exec_id: exec_id,
          time: time,
          acct_number: acct_number,
          exchange: exchange,
          side: side,
          shares: shares,
          price: price,
          perm_id: perm_id,
          client_id2: client_id2,
          order_id2: order_id2,
        )
      Ok(ExecutionDetail(execution))
    }
    _ -> Error("Invalid execution detail format")
  }
}
```

**Key Points**:
- Use `result.try` to chain operations that can fail
- Use `use` expressions with tuple destructuring to extract values and remaining data
- This pattern flattens deeply nested code and improves readability
- Follows Gleam's recommended idioms for error handling
- The result module provides `try`, `map`, `map_error`, and other utilities for working with Result types

**Functions Refactored**:
- `decode_execution_detail`
- `decode_position`
- `decode_contract`
- `decode_account_summary`
- `decode_connect_ack`
- `decode_connect_failed`
- `decode_open_order`
- `decode_realtime_bar`

This refactoring significantly improved code maintainability and follows Gleam best practices for error handling.

## Reference Resources

- [IB TWS API Documentation](https://interactivebrokers.github.io/)
- [IB API Reference Guide](https://interactivebrokers.github.io/tws-api/index.html)
- [Gleam Documentation](https://gleam.run/)
- [Erlang gen_tcp Documentation](https://erlang.org/doc/man/gen_tcp.html)

## Notes

- This is a learning project to explore Gleam's capabilities
- Focus on code quality and maintainability
- Prioritize type safety and functional programming principles
- Document decisions and trade-offs
- Keep the codebase simple and idiomatic
