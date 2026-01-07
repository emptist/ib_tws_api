# IB TWS API Wrapper for Gleam

A Gleam language wrapper for the Interactive Brokers Trader Workstation (TWS) API, targeting JavaScript.

## Overview

This library provides a Gleam interface to interact with the IB TWS API for paper trading and live trading operations. It follows a bottom-up development approach, implementing features incrementally as they are naturally needed.

## Status

🚧 **Under Active Development** - This project is in early development stages.

Currently implemented:
- ✅ Project structure and planning

## Installation

```sh
gleam add ib_tws_api
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

## License

Apache-2.0

## Disclaimer

This software is provided as-is for educational and development purposes. Trading involves significant risk of loss. Always test thoroughly with paper trading before using real money.

## Resources

- [IB TWS API Documentation](https://www.interactivebrokers.com/campus/ibkr-api-page/twsapi-doc/)
- [IB TWS API GitHub](https://github.com/InteractiveBrokers/tws-api)
- [Gleam Language](https://gleam.run/)
- [Development Plan](DEVELOPMENT_PLAN.md)
