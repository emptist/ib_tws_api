# ib_tws_api

[![Package Version](https://img.shields.io/hexpm/v/ib_tws_api)](https://hex.pm/packages/ib_tws_api)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/ib_tws_api/)

A Gleam wrapper for Interactive Brokers' TWS (Trader Workstation) and Gateway API, providing a type-safe, functional interface to interact with IB's trading infrastructure.

## Features

- ✅ Type-safe protocol implementation for IB TWS API
- ✅ Support for JavaScript target (Node.js)
- ✅ Comprehensive message encoding/decoding
- ✅ Socket-based communication with TWS/Gateway
- ✅ Account information retrieval
- ✅ Order placement and management
- ✅ Market data subscription
- ✅ Position tracking
- ✅ Real-time bar data

## Installation

```sh
gleam add ib_tws_api
```

## Quick Start

```gleam
import ib_tws_api
import ib_tws_api/protocol

pub fn main() {
  // Create a client
  let client = ib_tws_api.new_client("127.0.0.1", 7497, 1)

  // Connect to TWS
  case ib_tws_api.connect(client) {
    Ok(connected) -> {
      io.println("Connected successfully!")
      
      // Request account summary
      let req = protocol.AccountSummaryRequest(1, "All", [
        "TotalCashBalance",
        "NetLiquidation",
      ])
      
      case ib_tws_api.send_message(connected, req) {
        Ok(_) -> io.println("Request sent")
        Error(err) -> io.println("Error: " <> inspect_error(err))
      }
      
      // Disconnect when done
      ib_tws_api.disconnect(connected)
    }
    Error(err) -> {
      io.println("Failed to connect: " <> inspect_error(err))
    }
  }
}
```

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

## API Reference

### Client Management

```gleam
// Create a new client
let client = ib_tws_api.new_client(host: "127.0.0.1", port: 7497, client_id: 1)

// Connect to TWS/Gateway
let result = ib_tws_api.connect(client)

// Check connection status
let connected = ib_tws_api.is_connected(client)

// Disconnect from TWS/Gateway
let result = ib_tws_api.disconnect(client)
```

### Sending Messages

```gleam
// Send a message to TWS
let result = ib_tws_api.send_message(client, message)
```

### Receiving Messages

```gleam
// Receive a message from TWS
let result = ib_tws_api.receive_message(client, timeout_ms)
```

### Message Types

The library supports the following message types:

- `ConnectRequest` - Initiate connection
- `ConnectAck` - Connection acknowledgment
- `ConnectFailed` - Connection failure
- `MarketDataRequest` - Subscribe to market data
- `CancelMarketData` - Cancel market data subscription
- `MarketDataTick` - Receive market data updates
- `OrderPlace` - Place an order
- `CancelOrder` - Cancel an order
- `OrderStatus` - Receive order status updates
- `OpenOrder` - Receive open order details
- `ExecutionDetail` - Receive execution details
- `AccountSummaryRequest` - Request account information
- `AccountSummary` - Receive account summary data
- `PositionsRequest` - Request current positions
- `Position` - Receive position updates
- `RealTimeBarsRequest` - Subscribe to real-time bars
- `RealTimeBar` - Receive real-time bar updates
- `CancelRealTimeBars` - Cancel real-time bars subscription
- `Ping` - Keep-alive ping
- `Pong` - Keep-alive pong response

## Connection Details

### Default Ports

- **TWS Paper Trading**: 7497
- **TWS Live Trading**: 7496
- **IB Gateway Paper Trading**: 4002
- **IB Gateway Live Trading**: 4001

### Connection Requirements

- No API key or account ID needed for connection
- Accounts associated with the login are retrieved automatically after connection
- Client ID should be unique per connection (use incrementing integers)

## Testing

The project includes comprehensive unit tests covering:

- Protocol encoding/decoding
- Client lifecycle management
- Message type handling
- Binary data parsing

Run tests with:

```sh
gleam test
```

## Current Status

### JavaScript Target (tryjs branch)
- ✅ Socket implementation working
- ✅ Protocol encoding/decoding working
- ✅ All 22 unit tests passing
- ✅ Clean project structure
- ⏳ Real TWS integration testing needed
- ⏳ Message buffering for continuous stream needed

### Erlang Target
- ⏳ To be implemented after tryjs branch merge

## Project Structure

```
src/ib_tws_api/
├── client.gleam          # Client connection management
├── socket.gleam           # Low-level TCP socket operations
├── protocol.gleam         # Message encoding/decoding
├── types.gleam            # Data type definitions
└── test_connection.gleam # Integration test examples
```

## Development Roadmap

### Phase 1: Foundation ✅
- [x] Set up project structure
- [x] Define core types
- [x] Implement basic socket communication
- [x] Define message types
- [x] Implement message encoding/decoding utilities

### Phase 2: Protocol Implementation ✅
- [x] Implement encoding for all message types
- [x] Implement decoding for all message types
- [x] Add message validation
- [x] Fix byte order issues (little-endian)

### Phase 3: JavaScript Target (tryjs branch) ✅
- [x] Fix JavaScript FFI implementation
- [x] Fix socket external references
- [x] Fix protocol encoding (byte order)
- [x] Fix client type errors
- [x] Clean up duplicate/obsolete files
- [x] All tests passing

### Phase 4: Real TWS Integration (In Progress)
- [ ] Test connection to real TWS instance
- [ ] Verify account discovery mechanism
- [ ] Test account summary retrieval
- [ ] Test open orders retrieval
- [ ] Test positions retrieval
- [ ] Test market data subscription
- [ ] Test order placement (paper trading)
- [ ] Test order cancellation
- [ ] Test execution detail retrieval

### Phase 5: Client Features (Pending)
- [ ] Implement client lifecycle management
- [ ] Add connection state tracking
- [ ] Implement message queueing
- [ ] Add automatic reconnection logic
- [ ] Implement request/response correlation

### Phase 6: High-Level API (Pending)
- [ ] Implement account information retrieval API
- [ ] Implement order management API
- [ ] Implement market data subscription API
- [ ] Implement position tracking API
- [ ] Add event streaming API

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

This project is licensed under the Apache-2.0 license.

## Acknowledgments

- [IB TWS API Documentation](https://interactivebrokers.github.io/)
- [Gleam Programming Language](https://gleam.run/)
