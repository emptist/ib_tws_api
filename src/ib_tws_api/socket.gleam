import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/io
import gleam/string
import ib_tws_api/protocol

@external(erlang, "gen_tcp", "send")
pub fn tcp_send(socket: Socket, data: BitArray) -> Dynamic

@external(erlang, "gen_tcp", "recv")
pub fn tcp_recv(
  socket: Socket,
  length: Int,
  timeout: Int,
) -> Result(BitArray, Dynamic)

@external(erlang, "gen_tcp", "close")
pub fn tcp_close(socket: Socket) -> Dynamic

pub type Socket

pub type ConnectionError {
  SocketError(String)
}

@external(erlang, "erlang", "binary_to_atom")
fn binary_to_atom(binary: BitArray) -> Dynamic

@external(erlang, "erlang", "make_tuple")
fn make_tuple(size: Int, elem: Dynamic) -> Dynamic

@external(erlang, "erlang", "setelement")
fn set_element(index: Int, tuple: Dynamic, value: Dynamic) -> Dynamic

@external(erlang, "erlang", "self")
fn get_self() -> Dynamic

@external(erlang, "gen_tcp", "controlling_process")
fn set_controlling_process(socket: Socket, pid: Dynamic) -> Dynamic

@external(erlang, "gen_tcp", "connect")
fn tcp_connect(
  address: Dynamic,
  port: Int,
  options: List(Dynamic),
) -> Result(Socket, Dynamic)

pub fn connect_socket(
  host: String,
  port: Int,
) -> Result(Socket, ConnectionError) {
  io.println("Connecting to " <> host <> ":" <> int.to_string(port))
  
  let host_atom = binary_to_atom(<<host:utf8>>)
  let active_atom = binary_to_atom(<<"active">>)
  let false_atom = binary_to_atom(<<"false">>)
  
  let active_tuple = make_tuple(2, active_atom)
  let active_tuple = set_element(2, active_tuple, false_atom)
  
  let options = [active_tuple]
  
  case tcp_connect(host_atom, port, options) {
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

pub fn send_message(
  socket: Socket,
  msg: protocol.Message,
) -> Result(Nil, ConnectionError) {
  let data = protocol.encode_message(msg)
  io.println("Sending message, data length: " <> int.to_string(bit_array.byte_size(data)))
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
  io.println("Attempting to receive message with timeout: " <> int.to_string(timeout))
  io.println("Buffer size: " <> int.to_string(bit_array.byte_size(buffer)))
  
  case buffer {
    <<>> -> {
      case tcp_recv(socket, 4096, timeout) {
        Ok(data) -> {
          io.println("Received data, length: " <> int.to_string(bit_array.byte_size(data)))
          case data {
            <<>> -> {
              io.println("Connection closed by server")
              Error(SocketError("Connection closed by server"))
            }
            _ -> {
              io.println("Combined buffer size: " <> int.to_string(bit_array.byte_size(data)))
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
      io.println("Using existing buffer, size: " <> int.to_string(bit_array.byte_size(buffer)))
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
              io.println("Received additional data, length: " <> int.to_string(bit_array.byte_size(data)))
              case data {
                <<>> -> {
                  io.println("Connection closed by server")
                  Error(SocketError("Connection closed by server"))
                }
                _ -> {
                  let new_buffer = bit_array.append(buffer, data)
                  io.println("New buffer size: " <> int.to_string(bit_array.byte_size(new_buffer)))
                  receive_message_with_buffer(socket, new_buffer, timeout)
                }
              }
            }
            Error(err) -> {
              io.println("Receive error: " <> string.inspect(err))
              let error_msg = "Failed to receive message: " <> string.inspect(err)
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
