import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api/protocol

@external(erlang, "gen_tcp", "send")
@external(javascript, "../ffi/socket_ffi.mjs", "send")
pub fn tcp_send(socket: Socket, data: BitArray) -> Dynamic

@external(erlang, "gen_tcp", "recv")
@external(javascript, "../ffi/socket_ffi.mjs", "recv")
pub fn tcp_recv(
  socket: Socket,
  length: Int,
  timeout: Int,
) -> Result(BitArray, Dynamic)

@external(erlang, "gen_tcp", "close")
@external(javascript, "../ffi/socket_ffi.mjs", "close")
pub fn tcp_close(socket: Socket) -> Dynamic

pub type Socket

pub type ConnectionError {
  SocketError(String)
}

@external(erlang, "socket_helper", "connect")
@external(javascript, "../ffi/socket_ffi.mjs", "connect")
fn tcp_connect(host: String, port: Int) -> Result(Socket, Dynamic)

fn parse_ip_string(host: String) -> Result(String, String) {
  case string.split(host, ".") {
    [_, _, _, _] -> Ok(host)
    _ -> Error("Invalid IP address format")
  }
}

pub fn connect_socket(
  host: String,
  port: Int,
) -> Result(Socket, ConnectionError) {
  io.println("Connecting to " <> host <> ":" <> int.to_string(port))

  case parse_ip_string(host) {
    Ok(host_string) -> {
      case tcp_connect(host_string, port) {
        Ok(socket) -> {
          io.println("Socket connected successfully")
          Ok(socket)
        }
        Error(err) -> {
          io.println("Socket connection failed: " <> string.inspect(err))
          Error(SocketError("Failed to connect: " <> string.inspect(err)))
        }
      }
    }
    Error(err) -> {
      io.println("Failed to parse host address: " <> err)
      Error(SocketError("Failed to parse host: " <> err))
    }
  }
}

pub fn send_message(
  socket: Socket,
  msg: protocol.Message,
) -> Result(Nil, ConnectionError) {
  let data = protocol.encode_message(msg)
  io.println(
    "Sending message, data length: " <> int.to_string(bit_array.byte_size(data)),
  )
  io.println("Data: " <> string.inspect(data))
  let _ = tcp_send(socket, data)
  Ok(Nil)
}

pub fn receive_message(
  socket: Socket,
  buffer: BitArray,
  timeout: Int,
) -> Result(#(protocol.Message, BitArray), ConnectionError) {
  receive_message_with_buffer(socket, buffer, timeout)
}

fn receive_message_with_buffer(
  socket: Socket,
  buffer: BitArray,
  timeout: Int,
) -> Result(#(protocol.Message, BitArray), ConnectionError) {
  io.println(
    "Attempting to receive message with timeout: " <> int.to_string(timeout),
  )
  io.println("Buffer size: " <> int.to_string(bit_array.byte_size(buffer)))

  case buffer {
    <<>> -> {
      case tcp_recv(socket, 4096, timeout) {
        Ok(data) -> {
          io.println(
            "Received data, length: "
            <> int.to_string(bit_array.byte_size(data)),
          )
          case data {
            <<>> -> {
              io.println("Connection closed by server")
              Error(SocketError("Connection closed by server"))
            }
            _ -> {
              io.println(
                "Combined buffer size: "
                <> int.to_string(bit_array.byte_size(data)),
              )
              case protocol.decode_message(data) {
                Ok(msg) -> {
                  io.println("Successfully decoded message")
                  Ok(#(msg, <<>>))
                }
                Error(err) -> {
                  io.println("Decode error: " <> err)
                  io.println("Need more data, receiving again...")
                  receive_message_with_buffer(socket, data, timeout)
                }
              }
            }
          }
        }
        Error(err) -> {
          io.println("Receive error: " <> string.inspect(err))
          let error_msg = "Failed to receive message: " <> string.inspect(err)
          io.println("Creating error with message: " <> error_msg)
          Error(SocketError(error_msg))
        }
      }
    }
    _ -> {
      io.println(
        "Using existing buffer, size: "
        <> int.to_string(bit_array.byte_size(buffer)),
      )
      case protocol.decode_message(buffer) {
        Ok(msg) -> {
          io.println("Successfully decoded message")
          Ok(#(msg, <<>>))
        }
        Error(err) -> {
          io.println("Decode error: " <> err)
          io.println("Need more data, receiving again...")
          case tcp_recv(socket, 4096, timeout) {
            Ok(data) -> {
              io.println(
                "Received additional data, length: "
                <> int.to_string(bit_array.byte_size(data)),
              )
              case data {
                <<>> -> {
                  io.println("Connection closed by server")
                  Error(SocketError("Connection closed by server"))
                }
                _ -> {
                  let new_buffer = bit_array.append(buffer, data)
                  io.println(
                    "New buffer size: "
                    <> int.to_string(bit_array.byte_size(new_buffer)),
                  )
                  receive_message_with_buffer(socket, new_buffer, timeout)
                }
              }
            }
            Error(err) -> {
              io.println("Receive error: " <> string.inspect(err))
              let error_msg =
                "Failed to receive message: " <> string.inspect(err)
              Error(SocketError(error_msg))
            }
          }
        }
      }
    }
  }
}

pub fn close_socket(socket: Socket) -> Result(Nil, ConnectionError) {
  io.println("Closing socket: " <> string.inspect(socket))
  let result = tcp_close(socket)
  io.println("Close result: " <> string.inspect(result))
  Ok(Nil)
}
