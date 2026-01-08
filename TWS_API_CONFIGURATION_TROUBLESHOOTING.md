# IB TWS API Configuration Troubleshooting Guide

## Current Status Analysis

Based on our diagnostic tests:
- ✅ Connection to TWS on port 7497 is working
- ✅ API handshake (V100 protocol) is successful  
- ✅ Client ID transmission works
- ❌ Account data is NOT being received from server

## Configuration Issues Detected

The server accepts connections but does not respond with account data. This indicates:

1. **API Socket Clients are not enabled** in TWS settings
2. **Localhost connections may be blocked** 
3. **Client ID may need to be explicitly allowed**
4. **TWS may need to be restarted** after configuration changes

## Step-by-Step Fix Instructions

### 1. Enable Socket Clients in TWS

1. Open Interactive Brokers TWS
2. Go to `Settings` (top menu) → `API` → `Settings`
3. Check the box: `Enable ActiveX and Socket Clients`
4. Verify `Socket port` is set to `7497` (paper trading) or `7496` (live trading)
5. Check `Allow connections from localhost only`
6. Click `OK` to save changes

### 2. Configure Client Permissions

1. Go to `Settings` → `API` → `Configure`
2. Add your client ID (try 0, 1, 2, 3) with appropriate permissions:
   - Read-Only API: Basic market data and account info
   - Trading API: Full trading capabilities
   - Market Data: Real-time quotes
3. Set IP restrictions if needed (localhost: 127.0.0.1)

### 3. Restart TWS

**CRITICAL STEP**: After making configuration changes, you MUST:
- Completely close TWS
- Wait 30 seconds
- Restart TWS
- Wait for TWS to fully load and connect to IB servers

### 4. Test Different Client IDs

Try different client IDs in your test:

```gleam
// Test with client ID 0
connection.connect(127.0.0.1, 7497, 0)

// Test with client ID 1  
connection.connect(127.0.0.1, 7497, 1)

// Test with client ID 2
connection.connect(127.0.0.1, 7497, 2)

// Test with client ID 3
connection.connect(127.0.0.1, 7497, 3)
```

## Verification Test

Run this comprehensive test after configuration:

```bash
gleam run -m comprehensive_connection_test
```

## Expected Successful Response

When properly configured, you should see:
- Account list messages (`managed_accounts`)
- Account summary data  
- Portfolio positions
- Open orders
- Market data subscriptions

## Common Error Messages

- `Could not connect to TWS`: Check TWS is running and port is correct
- `API handshake failed`: Version mismatch, check TWS version
- `No account data received`: API not enabled or permissions incorrect
- `Connection timeout`: Firewall blocking or TWS not responding

## Live vs Paper Trading Ports

- **Paper Trading**: Port `7497` (use for testing)
- **Live Trading**: Port `7496` (use with real money accounts)

## Security Notes

- Never enable API on public networks
- Use localhost-only connections for security
- Set appropriate API permissions for your needs
- Regularly review API access logs in TWS

## Next Steps After Configuration

1. Run comprehensive connection test
2. Verify account data is received
3. Test market data subscriptions
4. Test order placement (paper trading only)
5. Implement full trading system