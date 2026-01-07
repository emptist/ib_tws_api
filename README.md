# IB TWS API Wrapper for Gleam

A Gleam language wrapper for the Interactive Brokers Trader Workstation (TWS) API, targeting JavaScript.

## Overview

This library provides a Gleam interface to interact with the IB TWS API for paper trading and live trading operations. It follows a bottom-up development approach, implementing features incrementally as they are naturally needed.

## Status

🚧 **Under Active Development** - This project is in early development stages.

Currently implemented:
- ✅ Project structure and planning
- 🚧 Minimal TCP connection (in progress)

## Installation

```sh
gleam add ib_tws_api
```

## Quick Start

### Basic Connection (Paper Trading)

```gleam
import ib_tws_api
import gleam/io

pub fn main() {
  // Connect to paper trading account on port 7497
  let result = ib_tws_api.connect("127.0.0.1", 7497)
  
  case result {
    Ok(connection) -> {
      io.println("Connected to IB TWS API successfully!")
      // Connection is now ready to use
    }
    Error(reason) -> {
      io.println("Failed to connect: " <> reason)
    }
  }
}
```

## Configuration

The IB TWS API requires the TWS or IB Gateway application to be running with API connections enabled.

### Paper Trading (Development)
- **Port**: 7497
- **Use for**: Development, testing, and learning
- **Safety**: No real money at risk

### Live Trading (Production)
- **Port**: 7496
- **Use for**: Real trading operations
- **Warning**: Real money involved

## Development

### Running Tests

```sh
gleam test
```

### Building

```sh
gleam build
```

### Running Examples

```sh
gleam run --module examples/basic_connection
```

## Project Structure

```
src/
├── ib_tws_api.gleam          # Main public API
├── connection.gleam           # TCP connection handling
├── protocol.gleam             # Message protocol implementation
├── types.gleam                # Core type definitions
├── market_data.gleam          # Market data operations
├── orders.gleam               # Order management
└── account.gleam              # Account information
```

## Development Plan

See [`DEVELOPMENT_PLAN.md`](DEVELOPMENT_PLAN.md) for the complete development roadmap and implementation strategy.

## API Coverage

The library is being developed incrementally. Currently planned features:

- [x] Project setup and planning
- [ ] Basic TCP connection
- [ ] Protocol handshake
- [ ] Market data subscription
- [ ] Order management (paper trading)
- [ ] Account information
- [ ] Historical data
- [ ] Real-time bars
- [ ] Error handling and resilience

## Safety Notes

⚠️ **Important Safety Guidelines**:

1. **Always use paper trading (port 7497) for development and testing**
2. **Never automate buy/sell operations on live account (port 7496)**
3. **Test thoroughly on paper account before using live trading**
4. **The library is in early development - use at your own risk**

## Contributing

This is an early-stage project. Contributions are welcome once the basic functionality is established.

## License

Apache-2.0

## Disclaimer

This software is provided as-is for educational and development purposes. Trading involves significant risk of loss. Always test thoroughly with paper trading before using real money.

## Resources

- [IB TWS API Documentation](https://interactivebrokers.github.io/)
- [Gleam Language](https://gleam.run/)
- [Development Plan](DEVELOPMENT_PLAN.md)
