import gleam/option
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

      // Send ConnectRequest
      case
        socket.send_message(socket, protocol.ConnectRequest(client.client_id))
      {
        Ok(_) -> {
          // Wait for ConnectAck or ConnectFailed
          case socket.receive_message(socket, <<>>, 10_000) {
            Ok(#(msg, remaining_buffer)) -> {
              case msg {
                protocol.ConnectAck(_version, _server_time) -> {
                  let updated_client =
                    Client(..connected_client, buffer: remaining_buffer)
                  Ok(updated_client)
                }
                protocol.ConnectFailed(reason) -> {
                  let _ = socket.close_socket(socket)
                  Error(AuthenticationFailed(reason))
                }
                _ -> {
                  let _ = socket.close_socket(socket)
                  Error(AuthenticationFailed("Unexpected response from server"))
                }
              }
            }
            Error(err) -> {
              let _ = socket.close_socket(socket)
              let error_msg = case err {
                SocketConnectionError(msg) -> msg
              }
              Error(SocketError(error_msg))
            }
          }
        }
        Error(err) -> {
          let _ = socket.close_socket(socket)
          let error_msg = case err {
            SocketConnectionError(msg) -> msg
          }
          Error(SocketError(error_msg))
        }
      }
    }
    Error(err) -> {
      let error_msg = case err {
        SocketConnectionError(msg) -> msg
      }
      Error(SocketError(error_msg))
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
          let error_msg = case err {
            SocketConnectionError(msg) -> msg
          }
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
          let error_msg = case err {
            SocketConnectionError(msg) -> msg
          }
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
          let error_msg = case err {
            SocketConnectionError(msg) -> msg
          }
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
