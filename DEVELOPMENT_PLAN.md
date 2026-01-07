# IB TWS API Wrapper - Development Plan

## Overview
Building a Gleam language wrapper for the Interactive Brokers TWS API, targeting JavaScript. The development will follow a bottom-up approach, implementing features only when naturally needed.

## Project Goals
1. Create a minimal viable connection to IB TWS API
2. Receive raw data from the API
3. Gradually add features as needed
4. Maintain working functionality at every step
5. Consistent documentation and git commits

## Configuration
- **Paper Trading Port**: 7497 (for development and testing)
- **Live Trading Port**: 7496 (never use for buy/sell operations during development)
- **Protocol**: TCP socket-based communication
- **Target**: JavaScript (Node.js runtime)

## Development Phases

### Phase 1: Foundation (Current)
**Goal**: Establish basic project structure and minimal connectivity

#### Step 1: Project Setup
- [x] Create development plan document
- [x] Update gleam.toml to target JavaScript
- [x] Set up project structure with proper directories
- [x] Create initial documentation

#### Step 2: Minimal TCP Connection
- [ ] Implement basic TCP socket connection
- [ ] Connect to IB TWS API on port 7497
- [ ] Receive raw data stream
- [ ] Create test to verify connection and print raw data
- [ ] Commit with message: "feat: implement minimal TCP connection to IB TWS API"

**Success Criteria**: Can connect to TWS API and receive/print raw bytes

### Phase 2: Protocol Handshake
**Goal**: Implement the initial handshake protocol

#### Step 3: Message Protocol Basics
- [ ] Implement message framing (length-prefixed messages)
- [ ] Create types for message codes
- [ ] Implement handshake sequence:
  - Send API version
  - Send client ID
  - Receive server time
- [ ] Create tests for handshake
- [ ] Commit with message: "feat: implement IB TWS protocol handshake"

**Success Criteria**: Successful handshake and receive server time

### Phase 3: Basic Data Types
**Goal**: Define core data structures needed for API communication

#### Step 4: Core Type Definitions
- [ ] Define Contract type (symbol, security type, exchange, etc.)
- [ ] Define Order type (as needed)
- [ ] Define Market Data types (as needed)
- [ ] Define Error types
- [ ] Create tests for type constructors
- [ ] Commit with message: "feat: add core data type definitions"

**Success Criteria**: All basic types defined and tested

### Phase 4: Market Data (Minimal)
**Goal**: Request and receive market data

#### Step 5: Market Data Subscription
- [ ] Implement market data request function
- [ ] Handle market data callbacks
- [ ] Parse market data messages
- [ ] Create test that subscribes to a stock and prints data
- [ ] Commit with message: "feat: implement market data subscription"

**Success Criteria**: Can subscribe to a stock and receive quotes

### Phase 5: Order Management
**Goal**: Implement order placement and management (paper trading only)

#### Step 6: Order Operations
- [ ] Implement order placement (paper trading only)
- [ ] Implement order status callbacks
- [ ] Implement order cancellation
- [ ] Create tests for order operations (paper account only)
- [ ] Commit with message: "feat: implement order management"

**Success Criteria**: Can place/cancel orders on paper account

### Phase 6: Account Information
**Goal**: Retrieve account details

#### Step 7: Account Data
- [ ] Implement account summary request
- [ ] Parse account data messages
- [ ] Implement portfolio positions request
- [ ] Create tests for account information
- [ ] Commit with message: "feat: implement account information retrieval"

**Success Criteria**: Can retrieve account balance and positions

### Phase 7: Advanced Features
**Goal**: Add remaining API features as needed

#### Step 8: Historical Data
- [ ] Implement historical data requests
- [ ] Parse historical data responses
- [ ] Commit with message: "feat: add historical data support"

#### Step 9: Real-time Bars
- [ ] Implement real-time bar subscriptions
- [ ] Commit with message: "feat: add real-time bar support"

#### Step 10: News and Research
- [ ] Implement news bulletins (if needed)
- [ ] Commit with message: "feat: add news support"

### Phase 8: Error Handling and Resilience
**Goal**: Robust error handling and connection management

#### Step 11: Error Handling
- [ ] Implement comprehensive error types
- [ ] Add connection retry logic
- [ ] Add timeout handling
- [ ] Commit with message: "feat: improve error handling and resilience"

### Phase 9: Documentation and Examples
**Goal**: Complete documentation and usage examples

#### Step 12: Documentation
- [ ] Complete API documentation
- [ ] Add usage examples
- [ ] Create getting started guide
- [ ] Commit with message: "docs: complete documentation"

## Development Principles

1. **Bottom-Up Development**: Only implement features when naturally needed
2. **Working Code**: Every commit must have working, tested functionality
3. **Incremental**: Build on previous work without breaking existing features
4. **Documentation First**: Update docs as features are added
5. **Git Discipline**: Commit frequently with clear messages
6. **Test Coverage**: Every feature must have tests
7. **Safety First**: Never test buy/sell on live account (port 7496)

## Testing Strategy

- Unit tests for all pure functions
- Integration tests for API communication (paper account only)
- Manual testing with TWS Gateway running
- Never automate tests on live account

## File Structure

```
ib_tws_api/
├── src/
│   ├── ib_tws_api.gleam          # Main module
│   ├── connection.gleam           # TCP connection handling
│   ├── protocol.gleam             # Message protocol
│   ├── types.gleam                # Core type definitions
│   ├── market_data.gleam          # Market data operations
│   ├── orders.gleam               # Order management
│   └── account.gleam              # Account information
├── test/
│   ├── ib_tws_api_test.gleam      # Main test file
│   ├── connection_test.gleam      # Connection tests
│   ├── protocol_test.gleam        # Protocol tests
│   └── integration_test.gleam     # Integration tests (paper only)
├── examples/
│   ├── basic_connection.gleam    # Example: Connect and receive data
│   └── market_data.gleam          # Example: Subscribe to quotes
├── DEVELOPMENT_PLAN.md            # This file
├── README.md                      # Project documentation
└── gleam.toml                     # Project configuration
```

## Dependencies

Current dependencies:
- gleam_stdlib (standard library)
- gleeunit (testing)

Potential future additions (as needed):
- gleam_erlang or gleam_javascript for FFI
- Socket libraries for TCP communication
- JSON parsing libraries (if needed)

## Notes

- Always use port 7497 (paper trading) for development
- Port 7496 (live trading) should only be used for final deployment
- The IB TWS API documentation will be the primary reference
- Protocol version will be tracked in connection module