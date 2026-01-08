import account_data
import account_message_handler
import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option.{Some}
import protocol_fixed as protocol

/// Extended wait test to see if we eventually receive account data
pub fn main() {
  io.println("========================================")
  io.println("       EXTENDED WAIT TEST RUNNER       ")
  io.println("========================================")
  io.println("Mission: Test if account data arrives with longer wait times")

  let config = connection.config("127.0.0.1", 7497, 999)
  let account_handler = account_message_handler.default_account_handler()

  let data_callback = fn(data: String) {
    let timestamp = connection.get_timestamp()
    io.println("\n[" <> timestamp <> "] SERVER DATA:")
    io.println(data)
    io.println("[" <> timestamp <> "] END SERVER DATA\n")

    account_message_handler.process_account_message(data, account_handler)

    let message_type = account_message_handler.detect_message_type(data)
    case message_type == "managed_accounts" {
      True -> account_message_handler.answer_question_1(data, account_handler)
      False -> Nil
    }
  }

  case connection.connect_with_callback(config, Some(data_callback)) {
    Ok(conn) -> {
      io.println("✅ CONNECTION ESTABLISHED!")

      // Send handshake
      let handshake = protocol.start_api_message(100, 200)
      case connection.send_bytes(conn, handshake) {
        Ok(_) -> io.println("✅ Handshake sent")
        Error(e) ->
          io.println("❌ Error sending handshake: " <> error_to_string(e))
      }

      connection.sleep(1000)

      // Send client ID
      let client_id_msg = protocol.client_id_message(config.client_id)
      case connection.send_bytes(conn, client_id_msg) {
        Ok(_) -> io.println("✅ Client ID sent")
        Error(e) ->
          io.println("❌ Error sending client ID: " <> error_to_string(e))
      }

      connection.sleep(2000)

      // Request managed accounts (wait much longer for response)
      io.println("\nREQUESTING MANAGED ACCOUNTS (waiting 30 seconds)...")
      let accounts_msg = account_data.request_managed_accounts()
      case connection.send_bytes(conn, accounts_msg) {
        Ok(_) -> io.println("✅ Managed accounts request sent")
        Error(e) -> io.println("❌ Error: " <> error_to_string(e))
      }

      // Wait 30 seconds for response
      connection.sleep(30_000)

      // Close connection
      case connection.close(conn) {
        Ok(_) -> io.println("✅ Connection closed")
        Error(e) -> io.println("❌ Error closing: " <> error_to_string(e))
      }
    }
    Error(error) -> {
      io.println("❌ CONNECTION FAILED: " <> error_to_string(error))
    }
  }
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
