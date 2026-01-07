import connection
import execution_reports
import fundamental_data
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import news_and_research

/// Phase 9 Integration Test
/// 
/// This example tests all Phase 9 features with real data from IB TWS API:
/// - Execution and Commission Reports
/// - News and Research Features
/// - Fundamental Data
///
/// IMPORTANT: Run this with paper trading account (port 7497)
/// DO NOT run with live account for trading operations
pub fn main() {
  io.println("========================================")
  io.println("Phase 9 Integration Test")
  io.println("Testing with Real IB TWS API Data")
  io.println("========================================")

  // Connect to IB TWS API (paper trading)
  let config = connection.config("127.0.0.1", 7497, 1001)

  io.println("\n1. Connecting to IB TWS API...")
  io.println("   Host: " <> config.host)
  io.println("   Port: " <> int.to_string(config.port))
  io.println("   Client ID: " <> int.to_string(config.client_id))

  case connection.connect(config) {
    Ok(conn) -> {
      io.println("   ✓ Connected successfully!")

      // Test all Phase 9 features
      test_execution_reports(conn)
      test_news_and_research(conn)
      test_fundamental_data(conn)

      // Close connection
      io.println("\n10. Closing connection...")
      case connection.close(conn) {
        Ok(_) -> io.println("   ✓ Connection closed")
        Error(_) -> io.println("   ✗ Error closing connection")
      }
    }
    Error(_) -> {
      io.println("   ✗ Connection failed")
      io.println("\nPlease ensure:")
      io.println("   - IB TWS is running")
      io.println("   - API connections are enabled in TWS settings")
      io.println("   - Port 7497 is available (paper trading)")
    }
  }

  io.println("\n========================================")
  io.println("Integration Test Complete")
  io.println("========================================")
}

/// Test Execution and Commission Reports
fn test_execution_reports(conn: connection.Connection) {
  io.println("\n2. Testing Execution and Commission Reports")
  io.println("   -----------------------------------")

  // Request executions
  let exec_msg = execution_reports.request_executions()

  io.println("   Requesting executions...")
  case connection.send_bytes(conn, exec_msg) {
    Ok(_) -> {
      io.println("   ✓ Execution request sent")
      io.println("   Note: Execute some orders in TWS to see execution data")
    }
    Error(_) -> io.println("   ✗ Error sending request")
  }

  // Request commissions
  let commission_msg = execution_reports.request_commission_report()
  io.println("   Requesting commission reports...")
  case connection.send_bytes(conn, commission_msg) {
    Ok(_) -> io.println("   ✓ Commission report request sent")
    Error(_) -> io.println("   ✗ Error sending request")
  }
}

/// Test News and Research Features
fn test_news_and_research(conn: connection.Connection) {
  io.println("\n3. Testing News and Research Features")
  io.println("   -----------------------------------")

  // Request news bulletins
  let bulletin_msg = news_and_research.request_news_bulletins(True)

  io.println("   Requesting news bulletins...")
  case connection.send_bytes(conn, bulletin_msg) {
    Ok(_) -> {
      io.println("   ✓ News bulletin request sent")
      io.println("   Note: News bulletins will be received via callbacks")
    }
    Error(_) -> io.println("   ✗ Error sending request")
  }

  // Request historical news (example: AAPL)
  let hist_news_msg =
    news_and_research.request_historical_news(
      3002,
      12_345,
      "BZ",
      "20240101 00:00:00",
      "",
      10,
    )

  io.println("   Requesting historical news for AAPL...")
  case connection.send_bytes(conn, hist_news_msg) {
    Ok(_) -> io.println("   ✓ Historical news request sent")
    Error(_) -> io.println("   ✗ Error sending request")
  }
}

/// Test Fundamental Data
fn test_fundamental_data(conn: connection.Connection) {
  io.println("\n4. Testing Fundamental Data")
  io.println("   -----------------------------------")

  // Request fundamental data for AAPL
  let fundamental_msg = fundamental_data.request_fundamental_data(4001, 12_345)

  io.println("   Requesting fundamental data...")
  io.println("   Contract ID: " <> int.to_string(12_345))
  case connection.send_bytes(conn, fundamental_msg) {
    Ok(_) -> {
      io.println("   ✓ Fundamental data request sent")
      io.println("   Note: Fundamental data will include:")
      io.println("     - EPS and PE ratio")
      io.println("     - Dividend yield")
      io.println("     - Revenue data")
      io.println("     - Balance sheet data")
    }
    Error(_) -> io.println("   ✗ Error sending request")
  }

  // Demonstrate financial calculations
  io.println("\n5. Demonstrating Financial Calculations")
  io.println("   -----------------------------------")

  // Calculate PE ratio
  let price = 150.0
  let eps = 6.0
  case fundamental_data.calculate_pe_ratio(price, eps) {
    Ok(pe) -> {
      io.println("   PE Ratio Calculation:")
      io.println("     Price: $" <> float.to_string(price))
      io.println("     EPS: $" <> float.to_string(eps))
      io.println("     PE Ratio: " <> float.to_string(pe))
    }
    Error(e) -> io.println("   ✗ Error: " <> e)
  }

  // Calculate dividend yield
  let dividend = 0.96
  case fundamental_data.calculate_dividend_yield(dividend, price) {
    Ok(yield) -> {
      io.println("\n   Dividend Yield Calculation:")
      io.println("     Annual Dividend: $" <> float.to_string(dividend))
      io.println("     Price: $" <> float.to_string(price))
      io.println("     Dividend Yield: " <> float.to_string(yield) <> "%")
    }
    Error(e) -> io.println("   ✗ Error: " <> e)
  }

  // Calculate debt-to-equity
  let debt = 100_000.0
  let equity = 200_000.0
  case fundamental_data.calculate_debt_to_equity(debt, equity) {
    Ok(dte) -> {
      io.println("\n   Debt-to-Equity Calculation:")
      io.println("     Total Debt: $" <> float.to_string(debt))
      io.println("     Shareholder Equity: $" <> float.to_string(equity))
      io.println("     Debt-to-Equity: " <> float.to_string(dte))
    }
    Error(e) -> io.println("   ✗ Error: " <> e)
  }

  // Calculate current ratio
  let current_assets = 300_000.0
  let current_liabilities = 120_000.0
  case
    fundamental_data.calculate_current_ratio(
      current_assets,
      current_liabilities,
    )
  {
    Ok(cr) -> {
      io.println("\n   Current Ratio Calculation:")
      io.println("     Current Assets: $" <> float.to_string(current_assets))
      io.println(
        "     Current Liabilities: $" <> float.to_string(current_liabilities),
      )
      io.println("     Current Ratio: " <> float.to_string(cr))
    }
    Error(e) -> io.println("   ✗ Error: " <> e)
  }
}
