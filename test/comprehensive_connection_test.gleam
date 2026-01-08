import account_data
import account_message_handler
import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option.{Some}
import message_encoder
import protocol_fixed as protocol

/// Comprehensive test to debug connection and account data issues
pub fn main() {
  io.println("==========================================================")
  io.println("           COMPREHENSIVE CONNECTION TEST                 ")
  io.println("==========================================================")
  io.println("Mission: Debug and fix account data response issues")

  let config = connection.config("127.0.0.1", 7497, 999)
  let account_handler = account_message_handler.default_account_handler()

  let data_callback = fn(data: String) {
    let timestamp = connection.get_timestamp()
    io.println("\n🔵 [" <> timestamp <> "] RAW SERVER DATA RECEIVED:")
    io.println("🔵 " <> data)
    io.println("🔵 [" <> timestamp <> "] END DATA\n")

    // Process all messages through the account handler
    account_message_handler.process_account_message(data, account_handler)

    // Check if this is managed accounts response
    let message_type = account_message_handler.detect_message_type(data)
    case message_type {
      "managed_accounts" ->
        account_message_handler.answer_question_1(data, account_handler)
      "timestamp_200" ->
        io.println("✅ Session timestamp received - connection established!")
      "account_update" -> io.println("✅ Account update received!")
      _ -> io.println("ℹ️  Received message type: " <> message_type)
    }
  }

  case connection.connect_with_callback(config, Some(data_callback)) {
    Ok(conn) -> {
      io.println("✅ CONNECTION ESTABLISHED!")
      io.println("Client ID: " <> int.to_string(config.client_id))

      // Send handshake (START API message)
      io.println("\n1. Sending API handshake...")
      let handshake = message_encoder.start_api_message(config.client_id)
      let handshake_bytes =
        message_encoder.add_length_prefix_to_string(handshake)
      case connection.send_bytes(conn, handshake_bytes) {
        Ok(_) -> io.println("   ✅ Handshake sent")
        Error(e) -> io.println("   ❌ Error: " <> error_to_string(e))
      }

      connection.sleep(1000)

      // Send client ID as separate message
      io.println("\n2. Sending client ID...")
      let client_id_msg = protocol.client_id_message(config.client_id)
      case connection.send_bytes(conn, client_id_msg) {
        Ok(_) -> io.println("   ✅ Client ID sent")
        Error(e) -> io.println("   ❌ Error: " <> error_to_string(e))
      }

      connection.sleep(2000)

      // TEST 1: Request managed accounts (standard approach)
      io.println("\n3. TEST 1: Requesting managed accounts...")
      let accounts_msg = account_data.request_managed_accounts()
      let accounts_bytes =
        message_encoder.add_length_prefix_to_string(accounts_msg)
      case connection.send_bytes(conn, accounts_bytes) {
        Ok(_) -> io.println("   ✅ Managed accounts request sent")
        Error(e) -> io.println("   ❌ Error: " <> error_to_string(e))
      }

      io.println("   Waiting 10 seconds for response...")
      connection.sleep(10_000)

      // TEST 2: Try alternative approach - positions request
      io.println("\n4. TEST 2: Requesting positions...")
      let positions_msg = account_data.request_positions(1)
      let positions_bytes =
        message_encoder.add_length_prefix_to_string(positions_msg)
      case connection.send_bytes(conn, positions_bytes) {
        Ok(_) -> io.println("   ✅ Positions request sent")
        Error(e) -> io.println("   ❌ Error: " <> error_to_string(e))
      }

      io.println("   Waiting 10 seconds for response...")
      connection.sleep(10_000)

      // TEST 3: Request account summary
      io.println("\n5. TEST 3: Requesting account summary...")
      let summary_msg =
        account_data.request_account_summary(
          1,
          "All",
          account_data.common_account_tags(),
        )
      let summary_bytes =
        message_encoder.add_length_prefix_to_string(summary_msg)
      case connection.send_bytes(conn, summary_bytes) {
        Ok(_) -> io.println("   ✅ Account summary request sent")
        Error(e) -> io.println("   ❌ Error: " <> error_to_string(e))
      }

      io.println("   Waiting 15 seconds for response...")
      connection.sleep(15_000)

      // Close connection
      io.println("\n6. Closing connection...")
      case connection.close(conn) {
        Ok(_) -> io.println("   ✅ Connection closed")
        Error(e) -> io.println("   ❌ Error closing: " <> error_to_string(e))
      }
    }
    Error(error) -> {
      io.println("❌ CONNECTION FAILED: " <> error_to_string(error))
    }
  }

  io.println(
    "\n" <> "==========================================================",
  )
  io.println("                 TEST COMPLETE                               ")
  io.println("==========================================================")
  io.println("Review the responses above to identify any issues.")
  io.println("If no account data was received, check:")
  io.println("- IB TWS is running and configured for API access")
  io.println("- Paper trading account has proper permissions")
  io.println("- Client ID 999 is not already in use")
}

fn error_to_string(error: connection.ConnectionError) -> String {
  case error {
    connection.ConnectionFailed(msg) -> "Connection failed: " <> msg
    connection.InvalidHost -> "Invalid host"
    connection.InvalidPort -> "Invalid port"
    connection.SocketError(msg) -> "Socket error: " <> msg
    connection.Timeout -> "Timeout"
  }
}
