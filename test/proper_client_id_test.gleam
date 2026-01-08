import binary_message_decoder
import connection
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option.{Some}
import message_encoder
import protocol

/// Test that properly sends client ID and waits for responses
/// Client ID is CRITICAL - without it, TWS won't send any data
pub fn main() {
  io.println("\n=== PROPER CLIENT ID TEST ===")
  io.println("This test ensures client ID is sent correctly")
  io.println("")

  // Generate unique client ID
  let client_id = connection.generate_client_id()
  io.println("Client ID: " <> int.to_string(client_id))
  io.println("")

  // Create connection config for paper trading (port 7497)
  let conn_config = connection.config("127.0.0.1", 7497, client_id)

  // Connect with callback to handle data
  case
    connection.connect_with_callback(
      conn_config,
      Some(fn(data) {
        // Log raw data
        io.println("📥 [RAW DATA] Received: " <> data)
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
      let handshake = message_encoder.start_api_message(conn_config.client_id)
      let handshake_bytes =
        message_encoder.add_length_prefix_to_string(handshake)
      io.println(
        "   Handshake size: "
        <> int.to_string(bit_array.byte_size(handshake_bytes))
        <> " bytes",
      )

      case connection.send_bytes(conn, handshake_bytes) {
        Ok(_) -> {
          io.println("✅ Handshake sent successfully")
          io.println("")

          // CRITICAL: Wait for server to respond BEFORE sending client ID
          io.println(
            "⏳ [STEP 2] Waiting 5 seconds for server handshake response...",
          )
          io.println(
            "   (Server must respond with version before we send client ID)",
          )
          io.println("")

          connection.sleep(5000)

          // Step 2: Send client ID (ONLY after server responds)
          io.println("📤 [STEP 3] Sending client ID message...")
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
              io.println("=== CLIENT ID TRANSMISSION COMPLETE ===")
              io.println("")

              // CRITICAL: TWS won't send data unless we explicitly request it!
              io.println("📤 [STEP 4] Requesting account summary...")
              let account_summary_msg =
                message_encoder.request_account_summary(
                  9001,
                  "All",
                  "$LEDGER:ALL",
                )
              let account_summary_bytes =
                message_encoder.add_length_prefix_to_string(account_summary_msg)
              io.println(
                "   Request size: "
                <> int.to_string(bit_array.byte_size(account_summary_bytes))
                <> " bytes",
              )

              case connection.send_bytes(conn, account_summary_bytes) {
                Ok(_) -> {
                  io.println("✅ Account summary request sent")
                  io.println("")

                  io.println("📤 [STEP 5] Requesting positions...")
                  let positions_msg = message_encoder.request_positions(9002)
                  let positions_bytes =
                    message_encoder.add_length_prefix_to_string(positions_msg)
                  io.println(
                    "   Request size: "
                    <> int.to_string(bit_array.byte_size(positions_bytes))
                    <> " bytes",
                  )

                  case connection.send_bytes(conn, positions_bytes) {
                    Ok(_) -> {
                      io.println("✅ Positions request sent")
                      io.println("")

                      io.println("📤 [STEP 6] Requesting open orders...")
                      let orders_msg = message_encoder.request_open_orders()
                      let orders_bytes =
                        message_encoder.add_length_prefix_to_string(orders_msg)
                      io.println(
                        "   Request size: "
                        <> int.to_string(bit_array.byte_size(orders_bytes))
                        <> " bytes",
                      )

                      case connection.send_bytes(conn, orders_bytes) {
                        Ok(_) -> {
                          io.println("✅ Open orders request sent")
                          io.println("")

                          io.println("⏳ Now listening for server responses...")
                          io.println(
                            "   (All data requests sent, waiting for TWS to respond)",
                          )
                          io.println("")

                          // Wait for data
                          connection.sleep(60_000)

                          io.println("")
                          io.println("⏱️  Test timeout reached")
                        }
                        Error(err) -> {
                          let error_msg = case err {
                            connection.SocketError(msg) -> msg
                            connection.ConnectionFailed(msg) -> msg
                            connection.InvalidHost -> "Invalid host"
                            connection.InvalidPort -> "Invalid port"
                            connection.Timeout -> "Connection timeout"
                          }
                          io.println(
                            "❌ Failed to send open orders request: "
                            <> error_msg,
                          )
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
                      io.println(
                        "❌ Failed to send positions request: " <> error_msg,
                      )
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
                  io.println(
                    "❌ Failed to send account summary request: " <> error_msg,
                  )
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
    }
  }

  io.println("")
  io.println("=== TEST COMPLETE ===")
}
