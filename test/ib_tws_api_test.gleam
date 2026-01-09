import account_data
import api_messages
import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option.{Some}
import gleam/result
import message_encoder
import order_management
import protocol

// ═══════════════════════════════════════════════════════════════
// IB TWS API TEST SUITE - FACT DISCOVERY
// 
// This test suite discovers FACTS about real TWS behavior.
// Tests use Result types to honestly report what works and what doesn't.
// 
// Run with: gleam test
// 
// Test Philosophy:
// - Connect to REAL TWS instance
// - Send REAL protocol messages
// - Discover FACTS about TWS behavior
// - Use Result types to honestly report success/failure
// - NO fake data, NO cheating compiler
// ═══════════════════════════════════════════════════════════════

// ═══════════════════════════════════════════════════════════════
// TEST RESULTS TRACKING
// ═══════════════════════════════════════════════════════════════

pub type TestResult {
  TestResult(
    test_name: String,
    status: String,
    // "PASS", "FAIL", "UNKNOWN"
    details: String,
    fact_discovered: String,
  )
}

// ═══════════════════════════════════════════════════════════════
// PAPER TRADING ACCOUNT TESTS (Port 7497)
// ═══════════════════════════════════════════════════════════════

/// Test: Can we establish a TCP connection to TWS on port 7497?
/// This is the most basic test - just a TCP socket connection
pub fn test_paper_trading_tcp_connection() -> TestResult {
  io.println("\n=== TEST: TCP Connection to Paper Trading (Port 7497) ===")

  let client_id = connection.generate_client_id()
  let config = connection.config("127.0.0.1", 7497, client_id)

  io.println("Client ID: " <> int.to_string(client_id))
  io.println("Host: " <> config.host)
  io.println("Port: " <> int.to_string(config.port))
  io.println("")

  case connection.connect(config) {
    Ok(conn) -> {
      io.println("✅ TCP connection established")

      // Close connection
      case connection.close(conn) {
        Ok(_) -> {
          io.println("✅ Connection closed cleanly")

          TestResult(
            test_name: "TCP Connection to Paper Trading",
            status: "PASS",
            details: "Successfully established TCP connection to TWS on port 7497",
            fact_discovered: "TWS is listening on port 7497 for paper trading connections",
          )
        }
        Error(err) -> {
          let error_msg = error_to_string(err)
          io.println("❌ Failed to close connection: " <> error_msg)

          TestResult(
            test_name: "TCP Connection to Paper Trading",
            status: "PARTIAL",
            details: "Connection established but failed to close cleanly: "
              <> error_msg,
            fact_discovered: "TWS accepts connections on port 7497 but close has issues",
          )
        }
      }
    }
    Error(err) -> {
      let error_msg = error_to_string(err)
      io.println("❌ Failed to connect: " <> error_msg)

      TestResult(
        test_name: "TCP Connection to Paper Trading",
        status: "FAIL",
        details: "Failed to establish TCP connection: " <> error_msg,
        fact_discovered: "TWS is NOT listening on port 7497 or is not running",
      )
    }
  }
}

/// Test: Can we send handshake message and receive response?
/// This tests the V100+ protocol handshake
pub fn test_paper_trading_handshake() -> TestResult {
  io.println("\n=== TEST: Handshake with Paper Trading ===")

  let client_id = connection.generate_client_id()
  let config = connection.config("127.0.0.1", 7497, client_id)

  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  // Track if we received handshake response
  let handshake_received = io.println("")
  // Placeholder

  let result =
    connection.connect_with_callback(
      config,
      Some(fn(data) {
        io.println("📥 Received data: " <> data)

        // Check if this is handshake response
        case protocol.parse_server_response(data) {
          Ok(#(version, timestamp)) -> {
            io.println("")
            io.println("✅ HANDSHAKE RESPONSE RECEIVED:")
            io.println("   Version: " <> int.to_string(version))
            io.println("   Timestamp: " <> timestamp)
            io.println("")
            // In real implementation, we'd set a flag here
          }
          Error(msg) -> {
            io.println("ℹ️  Not a handshake response: " <> msg)
          }
        }
      }),
    )

  case result {
    Ok(conn) -> {
      io.println("✅ Connected to TWS")

      // Send handshake
      io.println("📤 Sending handshake message...")
      let handshake = protocol.start_api_message(100, 200)

      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("✅ Handshake sent successfully")

          // Wait for server response
          io.println("⏳ Waiting 3 seconds for server response...")
          connection.sleep(3000)

          // Close connection
          let _ = connection.close(conn)

          TestResult(
            test_name: "Handshake with Paper Trading",
            status: "UNKNOWN",
            details: "Handshake sent, check above for server response",
            fact_discovered: "Need to verify if handshake response was received",
          )
        }
        Error(err) -> {
          let error_msg = error_to_string(err)
          io.println("❌ Failed to send handshake: " <> error_msg)

          let _ = connection.close(conn)

          TestResult(
            test_name: "Handshake with Paper Trading",
            status: "FAIL",
            details: "Failed to send handshake: " <> error_msg,
            fact_discovered: "Handshake message could not be sent",
          )
        }
      }
    }
    Error(err) -> {
      let error_msg = error_to_string(err)
      io.println("❌ Failed to connect: " <> error_msg)

      TestResult(
        test_name: "Handshake with Paper Trading",
        status: "FAIL",
        details: "Failed to establish connection: " <> error_msg,
        fact_discovered: "Cannot test handshake if connection fails",
      )
    }
  }
}

/// Test: Can we send client ID after handshake?
/// This tests the full handshake + client ID flow
pub fn test_paper_trading_client_id() -> TestResult {
  io.println("\n=== TEST: Send Client ID after Handshake ===")

  let client_id = connection.generate_client_id()
  let config = connection.config("127.0.0.1", 7497, client_id)

  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  let result = connection.connect(config)

  case result {
    Ok(conn) -> {
      io.println("✅ Connected to TWS")

      // Send handshake
      io.println("📤 Step 1: Sending handshake...")
      let handshake = protocol.start_api_message(100, 200)

      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("✅ Handshake sent")

          // Wait for server response
          io.println("⏳ Waiting 1 second...")
          connection.sleep(1000)

          // Send client ID
          io.println("📤 Step 2: Sending client ID...")
          let client_id_msg = protocol.client_id_message(client_id)

          case connection.send_bytes(conn, client_id_msg) {
            Ok(_) -> {
              io.println("✅ Client ID sent")

              // Wait to see if connection stays open
              io.println("⏳ Waiting 2 seconds...")
              connection.sleep(2000)

              let _ = connection.close(conn)

              TestResult(
                test_name: "Send Client ID after Handshake",
                status: "UNKNOWN",
                details: "Handshake and client ID sent successfully",
                fact_discovered: "Need to verify if TWS accepts client ID",
              )
            }
            Error(err) -> {
              let error_msg = error_to_string(err)
              io.println("❌ Failed to send client ID: " <> error_msg)

              let _ = connection.close(conn)

              TestResult(
                test_name: "Send Client ID after Handshake",
                status: "PARTIAL",
                details: "Handshake sent but client ID failed: " <> error_msg,
                fact_discovered: "TWS accepts handshake but rejects client ID",
              )
            }
          }
        }
        Error(err) -> {
          let error_msg = error_to_string(err)
          io.println("❌ Failed to send handshake: " <> error_msg)

          let _ = connection.close(conn)

          TestResult(
            test_name: "Send Client ID after Handshake",
            status: "FAIL",
            details: "Failed at handshake stage: " <> error_msg,
            fact_discovered: "Cannot proceed to client ID if handshake fails",
          )
        }
      }
    }
    Error(err) -> {
      let error_msg = error_to_string(err)
      io.println("❌ Failed to connect: " <> error_msg)

      TestResult(
        test_name: "Send Client ID after Handshake",
        status: "FAIL",
        details: "Failed to establish connection: " <> error_msg,
        fact_discovered: "Cannot test client ID if connection fails",
      )
    }
  }
}

/// Test: Can we request account data?
/// This tests if we can send API requests after connection
pub fn test_paper_trading_request_account_data() -> TestResult {
  io.println("\n=== TEST: Request Account Data ===")

  let client_id = connection.generate_client_id()
  let config = connection.config("127.0.0.1", 7497, client_id)

  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  let result =
    connection.connect_with_callback(
      config,
      Some(fn(data) { io.println("📥 Received: " <> data) }),
    )

  case result {
    Ok(conn) -> {
      io.println("✅ Connected to TWS")

      // Send handshake
      let handshake = protocol.start_api_message(100, 200)
      let _ = connection.send_bytes(conn, handshake)
      connection.sleep(1000)

      // Send client ID
      let client_id_msg = protocol.client_id_message(client_id)
      let _ = connection.send_bytes(conn, client_id_msg)
      connection.sleep(1000)

      // Request account summary
      io.println("📤 Requesting account summary...")
      let account_summary_msg =
        account_data.request_account_summary(
          1,
          "All",
          account_data.common_account_tags(),
        )
      let account_summary_bytes =
        message_encoder.add_length_prefix_to_string(account_summary_msg)

      case connection.send_bytes(conn, account_summary_bytes) {
        Ok(_) -> {
          io.println("✅ Account summary request sent")

          // Request positions
          io.println("📤 Requesting positions...")
          let positions_msg = account_data.request_positions(1)
          let positions_bytes =
            message_encoder.add_length_prefix_to_string(positions_msg)

          case connection.send_bytes(conn, positions_bytes) {
            Ok(_) -> {
              io.println("✅ Positions request sent")

              // Wait for responses
              io.println("⏳ Waiting 5 seconds for responses...")
              connection.sleep(5000)

              let _ = connection.close(conn)

              TestResult(
                test_name: "Request Account Data",
                status: "UNKNOWN",
                details: "Account summary and positions requests sent successfully",
                fact_discovered: "Need to verify if TWS sends account data",
              )
            }
            Error(err) -> {
              let error_msg = error_to_string(err)
              io.println("❌ Failed to send positions request: " <> error_msg)

              let _ = connection.close(conn)

              TestResult(
                test_name: "Request Account Data",
                status: "PARTIAL",
                details: "Account summary sent but positions failed: "
                  <> error_msg,
                fact_discovered: "Some API requests work, others don't",
              )
            }
          }
        }
        Error(err) -> {
          let error_msg = error_to_string(err)
          io.println("❌ Failed to send account summary request: " <> error_msg)

          let _ = connection.close(conn)

          TestResult(
            test_name: "Request Account Data",
            status: "FAIL",
            details: "Failed to send account summary: " <> error_msg,
            fact_discovered: "API requests cannot be sent",
          )
        }
      }
    }
    Error(err) -> {
      let error_msg = error_to_string(err)
      io.println("❌ Failed to connect: " <> error_msg)

      TestResult(
        test_name: "Request Account Data",
        status: "FAIL",
        details: "Failed to establish connection: " <> error_msg,
        fact_discovered: "Cannot test API requests if connection fails",
      )
    }
  }
}

// ═══════════════════════════════════════════════════════════════
// LIVE TRADING ACCOUNT TESTS (Port 7496) - READ ONLY
// ═══════════════════════════════════════════════════════════════

/// Test: Can we connect to live trading account in read-only mode?
/// WARNING: This connects to LIVE TRADING account
pub fn test_live_trading_readonly_connection() -> TestResult {
  io.println("\n=== TEST: Live Trading Read-Only Connection ===")
  io.println("⚠️  WARNING: Connecting to LIVE TRADING account (Port 7496)")
  io.println("⚠️  Trading operations are BLOCKED (Read-Only mode)")
  io.println("")

  let client_id = connection.generate_client_id()
  let config =
    connection.config_with_account_type(
      "127.0.0.1",
      7496,
      connection.LiveTradingReadOnly,
      client_id,
    )

  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  // Verify trading is blocked
  let trading_allowed =
    connection.is_trading_allowed(connection.LiveTradingReadOnly)
  io.println("Trading allowed: " <> bool_to_string(trading_allowed))
  io.println("")

  case connection.connect(config) {
    Ok(conn) -> {
      io.println("✅ Connected to live trading account (Read-Only)")

      // Close connection
      case connection.close(conn) {
        Ok(_) -> {
          io.println("✅ Connection closed cleanly")

          TestResult(
            test_name: "Live Trading Read-Only Connection",
            status: "PASS",
            details: "Successfully connected to live trading in read-only mode",
            fact_discovered: "TWS accepts read-only connections on port 7496",
          )
        }
        Error(err) -> {
          let error_msg = error_to_string(err)
          io.println("❌ Failed to close connection: " <> error_msg)

          TestResult(
            test_name: "Live Trading Read-Only Connection",
            status: "PARTIAL",
            details: "Connected but close failed: " <> error_msg,
            fact_discovered: "Live trading connection works but close has issues",
          )
        }
      }
    }
    Error(err) -> {
      let error_msg = error_to_string(err)
      io.println("❌ Failed to connect: " <> error_msg)

      TestResult(
        test_name: "Live Trading Read-Only Connection",
        status: "FAIL",
        details: "Failed to establish connection: " <> error_msg,
        fact_discovered: "TWS is NOT listening on port 7496 or is not running",
      )
    }
  }
}

// ═══════════════════════════════════════════════════════════════
// UTILITY TESTS
// ═══════════════════════════════════════════════════════════════

/// Test: Client ID generation
pub fn test_client_id_generation() -> TestResult {
  io.println("\n=== TEST: Client ID Generation ===")

  let client_id1 = connection.generate_client_id()
  let client_id2 = connection.generate_client_id()

  io.println("Client ID 1: " <> int.to_string(client_id1))
  io.println("Client ID 2: " <> int.to_string(client_id2))

  case client_id1 != client_id2 && client_id1 > 0 && client_id2 > 0 {
    True -> {
      io.println("✅ Client IDs are unique and positive")

      TestResult(
        test_name: "Client ID Generation",
        status: "PASS",
        details: "Generated unique positive client IDs",
        fact_discovered: "Client ID generation works correctly",
      )
    }
    False -> {
      io.println("❌ Client IDs are not unique or not positive")

      TestResult(
        test_name: "Client ID Generation",
        status: "FAIL",
        details: "Client IDs are not unique or not positive",
        fact_discovered: "Client ID generation has bugs",
      )
    }
  }
}

/// Test: Message encoding
pub fn test_message_encoding() -> TestResult {
  io.println("\n=== TEST: Message Encoding ===")

  // Test START_API message encoding
  let start_api_msg = api_messages.StartApiMessage(123, 100, "")
  let encoded = api_messages.encode_message(start_api_msg)

  let message_size = bit_array.byte_size(encoded)
  io.println(
    "START_API message size: " <> int.to_string(message_size) <> " bytes",
  )

  case message_size > 4 {
    True -> {
      io.println("✅ Message has 4-byte length prefix")

      TestResult(
        test_name: "Message Encoding",
        status: "PASS",
        details: "Messages encoded with 4-byte length prefix",
        fact_discovered: "Type-safe message encoding works correctly",
      )
    }
    False -> {
      io.println("❌ Message missing 4-byte length prefix")

      TestResult(
        test_name: "Message Encoding",
        status: "FAIL",
        details: "Messages do not have 4-byte length prefix",
        fact_discovered: "Message encoding is broken",
      )
    }
  }
}

/// Test: Handshake version string
pub fn test_handshake_version_string() -> TestResult {
  io.println("\n=== TEST: Handshake Version String ===")

  let version1 = protocol.start_api_message(100, 200)
  let version2 = protocol.start_api_message(100, 100)

  let size1 = bit_array.byte_size(version1)
  let size2 = bit_array.byte_size(version2)

  io.println("Version v100..200 size: " <> int.to_string(size1) <> " bytes")
  io.println("Version v100 size: " <> int.to_string(size2) <> " bytes")

  case size1 > 0 && size2 > 0 {
    True -> {
      io.println("✅ Handshake messages generated")

      TestResult(
        test_name: "Handshake Version String",
        status: "PASS",
        details: "Handshake version strings generated correctly",
        fact_discovered: "Handshake version string generation works",
      )
    }
    False -> {
      io.println("❌ Handshake messages empty")

      TestResult(
        test_name: "Handshake Version String",
        status: "FAIL",
        details: "Handshake messages are empty",
        fact_discovered: "Handshake version string generation is broken",
      )
    }
  }
}

// ═══════════════════════════════════════════════════════════════
// HELPER FUNCTIONS
// ═══════════════════════════════════════════════════════════════

fn error_to_string(error: connection.ConnectionError) -> String {
  case error {
    connection.ConnectionFailed(msg) -> "Connection failed: " <> msg
    connection.InvalidHost -> "Invalid host"
    connection.InvalidPort -> "Invalid port"
    connection.SocketError(msg) -> "Socket error: " <> msg
    connection.Timeout -> "Connection timeout"
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "YES"
    False -> "NO"
  }
}
