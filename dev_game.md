# Development Test Game
## Slogan
"No results, no fun, no use, no stop!"

## Purpose
This game is designed to test the functionality of the IB TWS API implementation and help identify any bugs or issues in the current codebase. By completing the following tasks, you'll verify the API's ability to handle various common operations.

## Tasks

### Task 1: Account Information
- Retrieve a list of all available accounts

### Task 2: Account Details
- For each account, fetch:
  - Current positions (stocks, options, etc.)
  - Available funds
  - Account summary information

### Task 3: Open Orders Check
- For each account, check if there are any open orders
- List details of all open orders if found

### Task 4: Sell Order Execution
- Select one account that has stock positions
- Execute a sell order for 100 shares of a stock from that account

### Task 5: Buy Order Execution
- Select one account
- Place a buy order for 100 shares of SLV (iShares Silver Trust) at the current market price
- Configure the order with the following parameters:
  - Allow out-of-market-time execution
  - Set order duration to "Good Till Canceled" (GTC)

### Task 6: Order Cancellation
- Identify one of the pending orders you've placed
- Successfully cancel the order

### Task 7: Logging
- Ensure all operations and their results are thoroughly logged
- Include timestamps, operation types, account IDs, and any error messages
- Logs should be detailed enough for thorough investigation of any issues

### Task 8: Game Completion
- Once all tasks are completed successfully
- Document the total time taken to complete the game
- Reflect on any challenges faced and how they were overcome
- Write a account_summary_and_trading_log.md file to document the game process and results

## Instructions
1. Complete each task in sequence
2. Document any issues or unexpected behavior encountered
3. Verify the expected results after each operation
4. Ensure all operations are properly logged

By successfully completing this game, you'll help identify and resolve any bugs or problems in the current implementation. Good luck and have fun! Remember our slogan: "No results, no fun, no use, no stop!" Don't stop until you have fun!