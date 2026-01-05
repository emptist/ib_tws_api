import gleam/io
import gleam/int
import ib_tws_api
import ib_tws_api/protocol
import ib_tws_api/client.{type ClientError, ConnectionFailed, AuthenticationFailed, SocketError}

pub fn main() {
  io.println("Attempting to connect to IB TWS at 127.0.0.1:7496...")
  
  let client = ib_tws_api.new_client("127.0.0.1", 7496, 1)
  
  case ib_tws_api.connect(client) {
    Ok(connected_client) -> {
      io.println("Successfully connected to TWS!")
      
      case ib_tws_api.is_connected(connected_client) {
        True -> io.println("Client is connected")
        False -> io.println("Client is not connected")
      }
      
      io.println("Sending AccountSummaryRequest...")
      let account_summary_req = protocol.AccountSummaryRequest(1, "All", ["TotalCashBalance", "NetLiquidation"])
      
      case ib_tws_api.send_message(connected_client, account_summary_req) {
        Ok(_) -> {
          io.println("AccountSummaryRequest sent successfully")
          
          io.println("Waiting for response...")
          case ib_tws_api.receive_message(connected_client, 5000) {
            Ok(msg) -> {
              io.println("Received message:")
              io.println("  Type: " <> msg |> inspect_message_type)
              
              case msg {
                protocol.AccountSummary(account, tag, value, currency) -> {
                  io.println("  Account: " <> account)
                  io.println("  Tag: " <> tag)
                  io.println("  Value: " <> value)
                  io.println("  Currency: " <> currency)
                }
                _ -> io.println("  (Not an AccountSummary message)")
              }
            }
            Error(err) -> {
              io.println("Error receiving message: " <> inspect_error(err))
            }
          }
        }
        Error(err) -> {
          io.println("Error sending AccountSummaryRequest: " <> inspect_error(err))
        }
      }
      
      io.println("Disconnecting...")
      case ib_tws_api.disconnect(connected_client) {
        Ok(_) -> io.println("Disconnected successfully")
        Error(err) -> io.println("Error disconnecting: " <> inspect_error(err))
      }
    }
    Error(err) -> {
      io.println("Failed to connect to TWS: " <> inspect_error(err))
    }
  }
}

fn inspect_message_type(msg: protocol.Message) -> String {
  case msg {
    protocol.ConnectRequest(_) -> "ConnectRequest"
    protocol.ConnectAck(_, _) -> "ConnectAck"
    protocol.ConnectFailed(_) -> "ConnectFailed"
    protocol.Disconnect -> "Disconnect"
    protocol.Ping -> "Ping"
    protocol.Pong -> "Pong"
    protocol.MarketDataRequest(_, _) -> "MarketDataRequest"
    protocol.MarketDataTick(_, _, _) -> "MarketDataTick"
    protocol.OrderPlace(_) -> "OrderPlace"
    protocol.OrderStatus(_, _, _, _) -> "OrderStatus"
    protocol.AccountSummaryRequest(_, _, _) -> "AccountSummaryRequest"
    protocol.AccountSummary(_, _, _, _) -> "AccountSummary"
    protocol.PositionsRequest -> "PositionsRequest"
    protocol.Position(_, _, _, _) -> "Position"
    protocol.Unknown(id, _) -> "Unknown(" <> int.to_string(id) <> ")"
  }
}

fn inspect_error(err: ClientError) -> String {
  case err {
    ConnectionFailed(msg) -> "ConnectionFailed: " <> msg
    AuthenticationFailed(msg) -> "AuthenticationFailed: " <> msg
    SocketError(msg) -> "SocketError: " <> msg
  }
}
