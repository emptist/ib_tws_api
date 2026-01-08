import binary_message_decoder
import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/string
import message_encoder
import protocol

/// Minimal diagnostic script to test TWS connection
/// This connects to TWS and shows what data (if any) is received
pub fn main() {
  io.println("\n=== TWS CONNECTION DIAGNOSTIC ===")
  io.println("This will attempt to connect to TWS and show all received data")
  io.println("")

  // Generate unique client ID
  let client_id = connection.generate_client_id()
  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  // Create connection config for paper trading (port 7497)
  let conn_config = connection.config("127.0.0.1", 7497, client_id)

  // Track whether we've received server handshake response
  // We need to send client ID ONLY after receiving server response
  let handshake_received = io.println("")

  // Connect with callback to handle data
  case
    connection.connect_with_callback(
      conn_config,
      Some(fn(data) {
        // Log raw data
        io.println(
          "📥 [DATA RECEIVED] Length: " <> int.to_string(string.length(data)),
        )
        io.println("   Raw: " <> data)
        io.println("")

        // Convert to bit array
        let bits = bit_array.from_string(data)

        // Try to parse as handshake response first
        case binary_message_decoder.parse_handshake_response(bits) {
          Ok(#(version, timestamp)) -> {
            io.println("✅ [HANDSHAKE] Server response parsed!")
            io.println("   Version: " <> int.to_string(version))
            io.println("   Timestamp: " <> timestamp)
            io.println("")
            io.println(
              "📤 [STEP 2] Now sending client ID after receiving server response...",
            )

            // Send client ID immediately after receiving server response
            let client_id_msg =
              protocol.client_id_message(conn_config.client_id)
            io.println(
              "   Client ID message size: "
              <> int.to_string(bit_array.byte_size(client_id_msg))
              <> " bytes",
            )

            // Note: We can't send from callback context, this is just for logging
            io.println("   (Client ID will be sent from main flow)")
            io.println("")
          }
          Error(handshake_err) -> {
            io.println("ℹ️  [INFO] Not a handshake response: " <> handshake_err)
            // Try to parse as binary message
            case binary_message_decoder.parse_message(bits) {
              Ok(msg) -> {
                binary_message_decoder.log_message(msg)
              }
              Error(parse_err) -> {
                io.println("❌ [PARSE ERROR] " <> parse_err)
              }
            }
          }
        }
      }),
    )
  {
    Ok(conn) -> {
      io.println("✅ Connection initiated")
      io.println("")

      // Wait for socket to be ready
      io.println("⏳ Waiting 2 seconds for socket to be ready...")
      connection.sleep(2000)

      io.println("✅ Socket should be ready now")
      io.println("")

      // Step 1: Send START_API handshake
      io.println("📤 [STEP 1] Sending START_API handshake...")
      let handshake = protocol.start_api_message(100, 200)
      io.println(
        "   Handshake size: "
        <> int.to_string(bit_array.byte_size(handshake))
        <> " bytes",
      )

      case connection.send_bytes(conn, handshake) {
        Ok(_) -> {
          io.println("✅ Handshake sent successfully")
          io.println("")

          // Wait for server to respond with version and timestamp
          io.println("⏳ Waiting 10 seconds for server handshake response...")
          io.println("(Server should respond with version and timestamp)")
          io.println(
            "(Client ID will be sent ONLY after receiving server response)",
          )
          io.println("")
          connection.keep_alive(10_000)

          io.println("")
          io.println(
            "⏱️  Waiting period complete - checking if we received server response...",
          )
          io.println("")

          // Step 2: Send client ID (only after we've waited for server response)
          io.println("📤 [STEP 2] Sending client ID message...")
          io.println("   Client ID: " <> int.to_string(conn_config.client_id))

          let client_id_msg = protocol.client_id_message(conn_config.client_id)
          io.println(
            "   Client ID message size: "
            <> int.to_string(bit_array.byte_size(client_id_msg))
            <> " bytes",
          )

          case connection.send_bytes(conn, client_id_msg) {
            Ok(_) -> {
              io.println("✅ Client ID sent successfully")
              io.println("")

              io.println("⏳ Waiting 15 seconds for any data from TWS...")
              io.println(
                "(TWS should send account data, positions, etc. if connection is working)",
              )
              io.println("(Keeping Node.js process alive for async events)")
              io.println("")
              connection.keep_alive(15_000)

              io.println("")
              io.println("⏱️  Diagnostic complete - process will exit now")
            }
            Error(err) -> {
              let error_msg = case err {
                connection.SocketError(msg) -> msg
                connection.ConnectionFailed(msg) -> msg
                connection.InvalidHost -> "Invalid host"
                connection.InvalidPort -> "Invalid port"
                connection.Timeout -> "Connection timeout"
              }
              io.println("❌ Failed to send client ID: " <> error_msg)
            }
          }
        }
        Error(err) -> {
          let error_msg = case err {
            connection.SocketError(msg) -> msg
            connection.ConnectionFailed(msg) -> msg
            connection.InvalidHost -> "Invalid host"
            connection.InvalidPort -> "Invalid port"
            connection.Timeout -> "Connection timeout"
          }
          io.println("❌ Failed to send handshake: " <> error_msg)
        }
      }
    }
    Error(err) -> {
      let error_msg = case err {
        connection.ConnectionFailed(msg) -> msg
        connection.InvalidHost -> "Invalid host"
        connection.InvalidPort -> "Invalid port"
        connection.SocketError(msg) -> msg
        connection.Timeout -> "Connection timeout"
      }
      io.println("❌ Failed to connect: " <> error_msg)
      io.println("")
      io.println("Please check:")
      io.println("  1. Is TWS application running?")
      io.println(
        "  2. Is API enabled in TWS? (File → Global Configuration → API → Settings)",
      )
      io.println("  3. Is 'Enable ActiveX and Socket Clients' checked?")
      io.println("  4. Is the port set to 7497?")
      io.println("  5. Is 'Read-Only API' unchecked?")
    }
  }

  io.println("")
  io.println("=== DIAGNOSTIC COMPLETE ===")
  io.println("")
}
