"""
Test IB connection using ib_async library to verify TWS is working
and compare the connection process with our Gleam implementation.
"""

import asyncio
from ib_async import *
import time

async def test_connection():
    print("=== IB Connection Test (using ib_async) ===")
    
    # Create IB client
    ib = IB()
    
    # Connect to TWS (paper trading on port 7497)
    print("\nConnecting to 127.0.0.1:7497...")
    await ib.connectAsync('127.0.0.1', 7497, clientId=999999)
    
    print(f"✓ Connected! Server version: {ib.serverVersion()}")
    print(f"  Connection time: {ib.twsConnectionTime()}")
    
    # Wait a bit for connection to stabilize
    await asyncio.sleep(1)
    
    # Get account summary
    print("\nRequesting account summary...")
    ib.reqAccountSummary(999, "All", "$LEDGER")
    await asyncio.sleep(1)
    
    # Display account summary
    print("\nAccount Summary:")
    for summary in ib.accountSummary():
        print(f"  {summary.account}: {summary.tag} = {summary.value}")
    
    # Get positions
    print("\nRequesting positions...")
    ib.reqPositions()
    await asyncio.sleep(1)
    
    print("\nPositions:")
    for position in ib.positions():
        print(f"  {position.contract.symbol}: {position.position}")
    
    # Disconnect
    print("\nDisconnecting...")
    await ib.disconnectAsync()
    print("✓ Disconnected")
    
    print("\n=== Test Complete ===")

if __name__ == "__main__":
    asyncio.run(test_connection())