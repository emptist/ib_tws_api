#!/usr/bin/env python3
"""
Simple test to verify ib_async connection works
"""
import asyncio
import sys
sys.path.insert(0, '/Users/jk/gits/hub/ibOptions')

from ib_async import IB

async def test_connection():
    ib = IB()
    try:
        # Use a timestamp-based client ID like ib_async does
        import time
        client_id = int(time.time() * 1000) % 1000000
        if client_id == 0:
            client_id = 1
        
        print(f"Connecting to IB with client_id={client_id}...")
        
        # Connect with a short timeout
        await ib.connectAsync('127.0.0.1', 7497, clientId=client_id, timeout=10)
        
        print(f"Connected! Connection ID: {ib.connection.clientId}")
        print(f"Server version: {ib.serverVersion()}")
        print(f"TWS connection time: {ib.twsConnectionTime()}")
        
        # Just disconnect after successful connection
        ib.disconnect()
        print("Disconnected successfully")
        
    except Exception as e:
        print(f"Error: {type(e).__name__}: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    asyncio.run(test_connection())
