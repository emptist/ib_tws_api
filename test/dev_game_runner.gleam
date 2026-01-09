import account_data
import account_message_handler
import api_messages
import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option.{Some}
import message_encoder
import order_management
import protocol

/// IB TWS Dev Game Runner
/// This module systematically answers the dev game questions to test the API wrapper
/// and identify any issues in the implementation.
pub fn main() {
  io.println("========================================")
  io.println("       IB TWS API DEV GAME RUNNER       ")
  io.println("========================================")
  io.println("")
  io.println(
    "Mission: Answer all 6 game questions and log results for investigation",
  )
  io.println("")

  // Create connection config (7497 for paper trading)
  let config = connection.config("127.0.0.1", 7497, 999)

  io.println("🎮 GAME STARTED")
  io.println("Connecting to IB TWS API (Paper Trading)...")
  io.println("Host: " <> config.host)
  io.println("Port: " <> int.to_string(config.port))
  io.println("Client ID: " <> int.to_string(config.client_id))
  io.println("")

  // Create account handler
  let account_handler = account_message_handler.default_account_handler()

  // Data callback to capture and process account messages
  let data_callback = fn(data: String) {
    let timestamp = connection.get_timestamp()
    io.println("\n[" <> timestamp <> "] SERVER DATA:")
    io.println(data)
    io.println("[" <> timestamp <> "] END SERVER DATA\n")

    // Process account-related messages first
    account_message_handler.process_account_message(data, account_handler)

    // Only answer question 1 if this looks like managed accounts data
    // (not the session timestamp messages that come first)
    let message_type = account_message_handler.detect_message_type(data)
    case message_type == "managed_accounts" {
      True -> account_message_handler.answer_question_1(data, account_handler)
      False -> Nil
    }
  }

  case connection.connect_with_callback(config, Some(data_callback)) {
    Ok(conn) -> {
      io.println("✅ CONNECTION ESTABLISHED!")
      io.println("")

      // Send handshake
      io.println("STEP 1: Sending handshake...")
      let handshake = protocol.start_api_message(100, 200)
      case connection.send_bytes(conn, handshake) {
        Ok(_) -> io.println("✅ Handshake sent")
        Error(e) ->
          io.println("❌ Error sending handshake: " <> error_to_string(e))
      }

      // Wait for server response
      connection.sleep(1000)

      // Send START_API message with type-safe encoding
      io.println(
        "\nSTEP 2: Sending START_API message with type-safe encoding...",
      )
      let start_api_msg = api_messages.start_api_message(config.client_id)
      let start_api_encoded = api_messages.encode_message(start_api_msg)
      case connection.send_bytes(conn, start_api_encoded) {
        Ok(_) ->
          io.println("✅ START_API message sent with correct protocol format")
        Error(e) ->
          io.println("❌ Error sending START_API: " <> error_to_string(e))
      }

      // Wait for connection to be fully established
      connection.sleep(2000)

      io.println("\n" <> "==================================================")
      io.println("🎯 ANSWERING GAME QUESTIONS")
      io.println("==================================================")
      io.println("")

      // Question 1: Which accounts do we have?
      io.println("QUESTION 1: Which accounts do we have?")
      io.println("Requesting managed accounts...")
      let accounts_msg = account_data.request_managed_accounts()
      let accounts_bytes =
        message_encoder.add_length_prefix_to_string(accounts_msg)
      io.println(
        "[DEBUG] Managed accounts request message length: "
        <> int.to_string(bit_array.byte_size(accounts_bytes)),
      )
      case connection.send_bytes(conn, accounts_bytes) {
        Ok(_) -> io.println("✅ Managed accounts request sent")
        Error(e) -> io.println("❌ Error: " <> error_to_string(e))
      }

      // Wait longer for account data (server might be slow to respond)
      io.println("Waiting 5 seconds for managed accounts response...")
      connection.sleep(5000)

      // Question 2: Positions and funds for each account
      io.println("\nQUESTION 2: Positions and funds for each account")
      io.println("Requesting account updates (positions and portfolio)...")

      // Request positions
      let positions_msg = account_data.request_positions(1)
      let positions_bytes =
        message_encoder.add_length_prefix_to_string(positions_msg)
      io.println(
        "[DEBUG] Positions request message length: "
        <> int.to_string(bit_array.byte_size(positions_bytes)),
      )
      case connection.send_bytes(conn, positions_bytes) {
        Ok(_) -> io.println("✅ Positions request sent")
        Error(e) -> io.println("❌ Error: " <> error_to_string(e))
      }

      // Request account summary
      let account_summary_msg =
        account_data.request_account_summary(
          0,
          "All",
          account_data.common_account_tags(),
        )
      let account_summary_bytes =
        message_encoder.add_length_prefix_to_string(account_summary_msg)
      io.println(
        "[DEBUG] Account summary message length: "
        <> int.to_string(bit_array.byte_size(account_summary_bytes)),
      )
      case connection.send_bytes(conn, account_summary_bytes) {
        Ok(_) -> io.println("✅ Account summary request sent")
        Error(e) -> io.println("❌ Error: " <> error_to_string(e))
      }

      // Wait longer for position and account data
      io.println("Waiting 8 seconds for position and account data...")
      connection.sleep(8000)

      // Question 3: Open orders for each account
      io.println("\nQUESTION 3: Open orders for each account")
      io.println("Requesting open orders...")

      let open_orders_msg = order_management.request_open_orders()
      io.println(
        "[DEBUG] Open orders message length: "
        <> int.to_string(bit_array.byte_size(open_orders_msg)),
      )
      case connection.send_bytes(conn, open_orders_msg) {
        Ok(_) -> io.println("✅ Open orders request sent")
        Error(e) -> io.println("❌ Error: " <> error_to_string(e))
      }

      // Wait longer for open orders data
      io.println("Waiting 5 seconds for open orders data...")
      connection.sleep(5000)

      // Questions 4 & 5: Trading actions
      io.println("\nQUESTIONS 4 & 5: Trading actions")
      io.println(
        "Skip trading for now - will implement after basic queries work",
      )
      io.println("(Sell current positions and buy SLV at market price)")

      // Question 6: Cancel pending orders
      io.println("\nQUESTION 6: Cancel pending orders")
      io.println(
        "Skip order cancellation for now - will implement after trading",
      )

      // Keep connection open to receive all data
      io.println("\n" <> "==================================================")
      io.println("🕒 KEEPING CONNECTION OPEN FOR 30 SECONDS")
      io.println("Waiting for all server responses...")
      io.println("==================================================")
      io.println("")

      io.println("Waiting 45 seconds for all remaining responses...")
      connection.sleep(45_000)

      // Close connection
      io.println("Closing connection...")
      case connection.close(conn) {
        Ok(_) -> io.println("✅ Connection closed")
        Error(e) -> io.println("❌ Error closing: " <> error_to_string(e))
      }
    }
    Error(error) -> {
      io.println("❌ CONNECTION FAILED: " <> error_to_string(error))
    }
  }

  io.println("")
  io.println("========================================")
  io.println("         GAME COMPLETE - LOG REVIEW      ")
  io.println("========================================")
  io.println("")
  io.println("Review the server responses above to answer:")
  io.println("1. Which accounts are available?")
  io.println("2. Positions and funds for each account")
  io.println("3. Open orders for each account")
  io.println("")
  io.println("Next steps:")
  io.println("- Analyze the received data")
  io.println("- Implement trading actions once basic queries work")
  io.println("- Fix any parsing issues identified")
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

/// Log detailed game results for investigation
pub fn log_game_results(question: Int, result_data: String, status: String) {
  let timestamp = connection.get_timestamp()
  io.println("\n" <> "🎮 GAME LOG - QUESTION " <> int.to_string(question))
  io.println("Timestamp: " <> timestamp)
  io.println("Status: " <> status)
  io.println("Data: " <> result_data)
  io.println("")
}
