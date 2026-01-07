import gleam/int
import gleam/option
import gleam/string
import ib_tws_api/protocol
import ib_tws_api/socket.{type Socket, SocketError as SocketConnectionError}

pub type Client {
  Client(
    host: String,
    port: Int,
    client_id: Int,
    socket: option.Option(Socket),
    next_order_id: Int,
    buffer: BitArray,
  )
}

pub type ClientError {
  ConnectionFailed(String)
  AuthenticationFailed(String)
  SocketError(String)
}

pub fn new_client(
  host host: String,
  port port: Int,
  client_id client_id: Int,
) -> Client {
  Client(
    host: host,
    port: port,
    client_id: client_id,
    socket: option.None,
    next_order_id: 0,
    buffer: <<>>,
  )
}

pub fn connect(client: Client) -> Result(Client, ClientError) {
  case socket.connect_socket(client.host, client.port) {
    Ok(socket) -> {
      let connected_client =
        Client(..client, socket: option.Some(socket), buffer: <<>>)

      // Step 1: Send greeting message
      case socket.send_raw_bytes(socket, protocol.encode_greeting_message()) {
        Ok(_) -> {
          // Wait for server version response
          case receive_server_version(socket, 10_000) {
            Ok(#(version, _server_time)) -> {
              // Step 2: Send StartAPI message
              case
                socket.send_raw_bytes(
                  socket,
                  protocol.encode_start_api(client.client_id),
                )
              {
                Ok(_) -> {
                  let updated_client = Client(..connected_client, buffer: <<>>)
                  Ok(updated_client)
                }
                Error(err) -> {
                  let _ = socket.close_socket(socket)
                  let error_msg = "Send failed: " <> string.inspect(err)
                  Error(SocketError(error_msg))
                }
              }
            }
            Error(err) -> {
              let _ = socket.close_socket(socket)
              let error_msg = "Receive failed: " <> string.inspect(err)
              Error(SocketError(error_msg))
            }
          }
        }
        Error(err) -> {
          let _ = socket.close_socket(socket)
          let error_msg = "Send greeting failed: " <> string.inspect(err)
          Error(SocketError(error_msg))
        }
      }
    }
    Error(err) -> {
      let error_msg = "Connect failed: " <> string.inspect(err)
      Error(SocketError(error_msg))
    }
  }
}

fn receive_server_version(
  socket: socket.Socket,
  timeout: Int,
) -> Result(#(Int, String), ClientError) {
  case socket.tcp_recv(socket, 4096, timeout) {
    Ok(data) -> {
      case decode_server_version_response(data) {
        Ok(result) -> Ok(result)
        Error(err) -> Error(AuthenticationFailed(err))
      }
    }
    Error(err) -> {
      let error_msg = "TCP recv failed: " <> string.inspect(err)
      Error(SocketError(error_msg))
    }
  }
}

fn decode_server_version_response(
  data: BitArray,
) -> Result(#(Int, String), String) {
  // Format: version_string\0server_time_string\0
  case data {
    <<>> -> Error("Empty response from server")
    _ -> {
      case protocol.decode_string_null_terminated(data) {
        Ok(#(version_str, rest)) -> {
          case protocol.decode_string_null_terminated(rest) {
            Ok(#(server_time, _remaining)) -> {
              case int.parse(version_str) {
                Ok(version) -> Ok(#(version, server_time))
                Error(_) -> Error("Invalid version number")
              }
            }
            Error(err) -> Error("Failed to decode server time: " <> err)
          }
        }
        Error(err) -> Error("Failed to decode version: " <> err)
      }
    }
  }
}

pub fn send_message(
  client: Client,
  msg: protocol.Message,
) -> Result(Nil, ClientError) {
  case client.socket {
    option.Some(socket) -> {
      case socket.send_message(socket, msg) {
        Ok(_) -> Ok(Nil)
        Error(err) -> {
          let error_msg = "Send message failed: " <> string.inspect(err)
          Error(SocketError(error_msg))
        }
      }
    }
    option.None -> Error(SocketError("Client is not connected"))
  }
}

pub fn receive_message(
  client: Client,
  timeout: Int,
) -> Result(#(protocol.Message, Client), ClientError) {
  case client.socket {
    option.Some(socket) -> {
      case socket.receive_message(socket, client.buffer, timeout) {
        Ok(#(msg, remaining_buffer)) -> {
          let updated_client = Client(..client, buffer: remaining_buffer)
          Ok(#(msg, updated_client))
        }
        Error(err) -> {
          let error_msg = "Receive message failed: " <> string.inspect(err)
          Error(SocketError(error_msg))
        }
      }
    }
    option.None -> Error(SocketError("Client is not connected"))
  }
}

pub fn disconnect(client: Client) -> Result(Nil, ClientError) {
  case client.socket {
    option.Some(socket) -> {
      case socket.close_socket(socket) {
        Ok(_) -> Ok(Nil)
        Error(err) -> {
          let error_msg = "Close socket failed: " <> string.inspect(err)
          Error(SocketError(error_msg))
        }
      }
    }
    option.None -> Ok(Nil)
  }
}

pub fn is_connected(client: Client) -> Bool {
  case client.socket {
    option.Some(_) -> True
    option.None -> False
  }
}
