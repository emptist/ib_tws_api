import ib_tws_api/protocol

@external(erlang, "gen_tcp", "connect")
pub fn tcp_connect(
  host: String,
  port: Int,
  options: List(#(String, Bool)),
) -> Result(Socket, String)

@external(erlang, "gen_tcp", "send")
pub fn tcp_send(socket: Socket, data: BitArray) -> Result(Nil, String)

@external(erlang, "gen_tcp", "recv")
pub fn tcp_recv(
  socket: Socket,
  length: Int,
  timeout: Int,
) -> Result(BitArray, String)

@external(erlang, "gen_tcp", "close")
pub fn tcp_close(socket: Socket) -> Result(Nil, String)

pub type Socket

pub type ConnectionError {
  SocketError(String)
}

pub fn connect_socket(
  host: String,
  port: Int,
) -> Result(Socket, ConnectionError) {
  let options = [
    #("binary", True),
    #("packet", False),
    #("active", False),
    #("reuseaddr", True),
  ]

  case tcp_connect(host, port, options) {
    Ok(socket) -> Ok(socket)
    Error(err) -> Error(SocketError("Failed to connect: " <> err))
  }
}

pub fn send_message(
  socket: Socket,
  msg: protocol.Message,
) -> Result(Nil, ConnectionError) {
  let data = protocol.encode_message(msg)
  case tcp_send(socket, data) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(SocketError("Failed to send message"))
  }
}

pub fn receive_message(
  socket: Socket,
  timeout: Int,
) -> Result(protocol.Message, ConnectionError) {
  case tcp_recv(socket, 0, timeout) {
    Ok(data) -> {
      case protocol.decode_message(data) {
        Ok(msg) -> Ok(msg)
        Error(err) -> Error(SocketError("Failed to decode message: " <> err))
      }
    }
    Error(_) -> Error(SocketError("Failed to receive message"))
  }
}

pub fn close_socket(socket: Socket) -> Result(Nil, ConnectionError) {
  case tcp_close(socket) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(SocketError("Failed to close socket"))
  }
}

pub fn receive_loop(
  socket: Socket,
  handler: fn(protocol.Message) -> Nil,
  timeout: Int,
) -> Nil {
  case receive_message(socket, timeout) {
    Ok(msg) -> {
      handler(msg)
      receive_loop(socket, handler, timeout)
    }
    Error(_) -> Nil
  }
}
