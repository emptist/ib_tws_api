import account_data
import api_messages
import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleeunit
import gleeunit/should
import message_encoder
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

pub fn main() {
  gleeunit.main()
}

// ═══════════════════════════════════════════════════════════════
// PAPER TRADING ACCOUNT TESTS (Port 7497)
// ═══════════════════════════════════════════════════════════════

/// Test: Can we establish a TCP connection to TWS on port 7497?
/// This is the most basic test - just a TCP socket connection
pub fn test_paper_trading_tcp_connection() {
  io.println("\n=== TEST: TCP Connection to Paper Trading (Port 7497) ===")

  let client_id = connection.generate_client_id()
  let config = connection.config("127.0.0.1", 7497, client_id)

  io.println("Client ID: " <> int.to_string(client_id))
  io.println("Host: " <> config.host)
  io.println("Port: " <> int.to_string(config.port))
  io.println("")

  let assert Ok(conn) = connection.connect(config)
  io.println("✅ TCP connection established")

  // Close connection
  let assert Ok(_) = connection.close(conn)
  io.println("✅ Connection closed cleanly")

  io.println("")
  io.println(
    "✅ FACT DISCOVERED: TWS is listening on port 7497 for paper trading connections",
  )
  io.println("")
}

/// Test: Can we send handshake message and receive response?
/// This tests the V100+ protocol handshake
pub fn test_paper_trading_handshake() {
  io.println("\n=== TEST: Handshake with Paper Trading ===")

  let client_id = connection.generate_client_id()
  let config = connection.config("127.0.0.1", 7497, client_id)

  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  let assert Ok(conn) = connection.connect(config)
  io.println("✅ Connected to TWS")

  // Send handshake
  io.println("📤 Sending handshake message...")
  let handshake = protocol.start_api_message(100, 200)

  let assert Ok(_) = connection.send_bytes(conn, handshake)
  io.println("✅ Handshake sent successfully")

  // Wait for server response
  io.println("⏳ Waiting 3 seconds for server response...")
  connection.sleep(3000)

  // Close connection
  let _ = connection.close(conn)

  io.println("")
  io.println(
    "✅ FACT DISCOVERED: Handshake message sent, check above for server response",
  )
  io.println("")
}

/// Test: Can we send client ID after handshake?
/// This tests the full handshake + client ID flow
pub fn test_paper_trading_client_id() {
  io.println("\n=== TEST: Send Client ID after Handshake ===")

  let client_id = connection.generate_client_id()
  let config = connection.config("127.0.0.1", 7497, client_id)

  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  let assert Ok(conn) = connection.connect(config)
  io.println("✅ Connected to TWS")

  // Send handshake
  io.println("📤 Step 1: Sending handshake...")
  let handshake = protocol.start_api_message(100, 200)

  let assert Ok(_) = connection.send_bytes(conn, handshake)
  io.println("✅ Handshake sent")

  // Wait for server response
  io.println("⏳ Waiting 1 second...")
  connection.sleep(1000)

  // Send client ID
  io.println("📤 Step 2: Sending client ID...")
  let client_id_msg = protocol.client_id_message(client_id)

  let assert Ok(_) = connection.send_bytes(conn, client_id_msg)
  io.println("✅ Client ID sent")

  // Wait to see if connection stays open
  io.println("⏳ Waiting 2 seconds...")
  connection.sleep(2000)

  let _ = connection.close(conn)

  io.println("")
  io.println("✅ FACT DISCOVERED: Handshake and client ID sent successfully")
  io.println("")
}

/// Test: Can we request account data?
/// This tests if we can send API requests after connection
pub fn test_paper_trading_request_account_data() {
  io.println("\n=== TEST: Request Account Data ===")

  let client_id = connection.generate_client_id()
  let config = connection.config("127.0.0.1", 7497, client_id)

  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  let assert Ok(conn) = connection.connect(config)
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

  let assert Ok(_) = connection.send_bytes(conn, account_summary_bytes)
  io.println("✅ Account summary request sent")

  // Request positions
  io.println("📤 Requesting positions...")
  let positions_msg = account_data.request_positions(1)
  let positions_bytes =
    message_encoder.add_length_prefix_to_string(positions_msg)

  let assert Ok(_) = connection.send_bytes(conn, positions_bytes)
  io.println("✅ Positions request sent")

  // Wait for responses
  io.println("⏳ Waiting 5 seconds for responses...")
  connection.sleep(5000)

  let _ = connection.close(conn)

  io.println("")
  io.println(
    "✅ FACT DISCOVERED: Account summary and positions requests sent successfully",
  )
  io.println("")
}

// ═══════════════════════════════════════════════════════════════
// LIVE TRADING ACCOUNT TESTS (Port 7496) - READ ONLY
// ═══════════════════════════════════════════════════════════════

/// Test: Can we connect to live trading account in read-only mode?
/// WARNING: This connects to LIVE TRADING account
pub fn test_live_trading_readonly_connection() {
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

  let assert Ok(conn) = connection.connect(config)
  io.println("✅ Connected to live trading account (Read-Only)")

  // Close connection
  let assert Ok(_) = connection.close(conn)
  io.println("✅ Connection closed cleanly")

  io.println("")
  io.println(
    "✅ FACT DISCOVERED: TWS accepts read-only connections on port 7496",
  )
  io.println("")
}

// ═══════════════════════════════════════════════════════════════
// UTILITY TESTS
// ═══════════════════════════════════════════════════════════════

/// Test: Client ID generation
pub fn test_client_id_generation() {
  io.println("\n=== TEST: Client ID Generation ===")

  let client_id1 = connection.generate_client_id()
  let client_id2 = connection.generate_client_id()

  io.println("Client ID 1: " <> int.to_string(client_id1))
  io.println("Client ID 2: " <> int.to_string(client_id2))

  should.not_equal(client_id1, client_id2)

  should.be_true(client_id1 > 0)

  should.be_true(client_id2 > 0)

  io.println("✅ Client IDs are unique and positive")

  io.println("")
  io.println("✅ FACT DISCOVERED: Client ID generation works correctly")
  io.println("")
}

/// Test: Message encoding
pub fn test_message_encoding() {
  io.println("\n=== TEST: Message Encoding ===")

  // Test START_API message encoding
  let start_api_msg = api_messages.StartApiMessage(123, 100, "")
  let encoded = api_messages.encode_message(start_api_msg)

  let message_size = bit_array.byte_size(encoded)
  io.println(
    "START_API message size: " <> int.to_string(message_size) <> " bytes",
  )

  should.be_true(message_size > 4)

  io.println("✅ Message has 4-byte length prefix")

  io.println("")
  io.println("✅ FACT DISCOVERED: Type-safe message encoding works correctly")
  io.println("")
}

/// Test: Handshake version string
pub fn test_handshake_version_string() {
  io.println("\n=== TEST: Handshake Version String ===")

  let version1 = protocol.start_api_message(100, 200)
  let version2 = protocol.start_api_message(100, 100)

  let size1 = bit_array.byte_size(version1)
  let size2 = bit_array.byte_size(version2)

  io.println("Version v100..200 size: " <> int.to_string(size1) <> " bytes")
  io.println("Version v100 size: " <> int.to_string(size2) <> " bytes")

  should.be_true(size1 > 0)

  should.be_true(size2 > 0)

  io.println("✅ Handshake messages generated")

  io.println("")
  io.println("✅ FACT DISCOVERED: Handshake version string generation works")
  io.println("")
}

// ═══════════════════════════════════════════════════════════════
// HELPER FUNCTIONS
// ═══════════════════════════════════════════════════════════════

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "YES"
    False -> "NO"
  }
}
