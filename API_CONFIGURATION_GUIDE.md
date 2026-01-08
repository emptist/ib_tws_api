# IB TWS API Configuration Guide

## Current Status: ✅ Connection Working, ❌ No Account Data

Our diagnostics confirm that:
1. ✅ IB TWS is running on port 7497 (Paper Trading)
2. ✅ Connection to IB TWS is successful
3. ✅ API handshake is working
4. ✅ Client ID transmission is working
5. ❌ Server is not responding with account data (positions, balances, orders)

## Root Cause
The IB TWS API socket is accepting connections but not responding with account data. This indicates that the API is not fully configured or enabled within the Interactive Brokers TWS application.

## Required Configuration Steps

### 1. Enable API Socket Clients
**In IB TWS:**
1. Go to `Settings` → `API` → `Settings`
2. Check `Enable ActiveX and Socket Clients`
3. Set `Socket port` to `7497` (for paper trading)
4. Check `Allow connections from localhost only` (recommended for security)
5. Click `OK` to save

### 2. Restart IB TWS
After making these changes, **restart IB TWS completely** for the changes to take effect.

### 3. Check Client ID Availability
Try different Client IDs in case ID 999 is already in use:
- 1, 2, 3, etc.
- Our system will automatically detect available ports

### 4. Verify Account Permissions
Ensure your paper trading account has API access permissions enabled.

## Connection Details
- **Host:** 127.0.0.1 (localhost)
- **Port:** 7497 (Paper Trading)
- **Client ID:** 999 (try others if needed)

## Testing Your Configuration

After making the configuration changes, run our diagnostic test:

```bash
gleam run -m simple_diagnostic
```

If successful, you should see account data responses from the server.

## Next Steps After Configuration

Once the API is properly configured, the following features will work:
- ✅ Account balance and position queries
- ✅ Real-time market data
- ✅ Order placement and management
- ✅ Portfolio updates
- ✅ Historical data requests

## Troubleshooting Common Issues

### If connection fails:
1. Verify IB TWS is running
2. Check firewall settings
3. Ensure port 7497 is not blocked

### If no account data is received:
1. Double-check API settings in TWS
2. Restart IB TWS after configuration changes
3. Try a different Client ID
4. Verify account has trading permissions

### If you see "Client ID already in use":
1. Use `connection.generate_client_id()` for automatic ID generation
2. Or manually specify a different Client ID (1, 2, 3, etc.)

## Security Notes
- Always use `Allow connections from localhost only` in production
- Paper trading port (7497) allows simulated trading without real money
- Live trading port (7496) uses real money - use with extreme caution
- Never share your Client ID or port configuration

## API Version Compatibility
- Current implementation supports API versions v100 to v200
- Automatically negotiates version with IB TWS server
- Handles both paper trading (7497) and live trading (7496) ports