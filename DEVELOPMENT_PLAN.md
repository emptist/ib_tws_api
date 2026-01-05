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

#### Account Messages
- **Request Account Summary (6)**: Request account information
- **Account Summary (7)**: Receive account summary data
- **Request Positions (8)**: Request current positions
- **Position (9)**: Receive position updates

## Development Phases

### Phase 1: Foundation
**Status**: Mostly Complete

**Tasks**:
- [x] Set up project structure
- [x] Define core types (Contract, Order, etc.)
- [x] Implement basic socket communication
- [x] Define message types
- [x] Implement message encoding/decoding utilities
- [x] Create connection handshake logic
- [ ] Fix failing protocol decoding tests
- [ ] Test basic socket connection with real TWS instance

**Deliverables**:
- [x] Working socket connection to TWS
- [x] Basic message type definitions
- [x] Encoding/decoding utilities
- [ ] Connection handshake verified with real TWS

**Progress Notes**:
- Socket communication implemented using Erlang's gen_tcp directly
- Message types defined: ConnectRequest, ConnectAck, ConnectFailed, AccountSummary, Position
- Encoding functions implemented for all defined message types
- Decoding functions implemented for ConnectAck, ConnectFailed, AccountSummary, Position
- String decoding uses bit array accumulation with UTF-8 conversion
- Client module updated to send ConnectRequest during connection
- Message sending/receiving functionality implemented
- Four decoding tests currently failing (ConnectAck, ConnectFailed, AccountSummary, Position) - need debugging

### Phase 2: Protocol Implementation
**Status**: Pending

**Tasks**:
- [ ] Implement encoding for all message types
- [ ] Implement decoding for all message types
- [ ] Add message validation
- [ ] Handle protocol versioning
- [ ] Implement error recovery

**Deliverables**:
- Complete protocol implementation
- Comprehensive test coverage for encoding/decoding
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
   - Implement message encoding/decoding utilities
   - Create connection handshake logic
   - Test basic socket connection

2. **Short-term**: Begin Phase 2 protocol implementation
   - Implement encoding for connection messages
   - Implement decoding for connection messages
   - Test connection handshake

3. **Medium-term**: Complete protocol implementation
   - Implement all message types
   - Add comprehensive tests
   - Document protocol details

4. **Long-term**: Build high-level API
   - Design user-friendly API
   - Implement client features
   - Create examples and documentation

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
