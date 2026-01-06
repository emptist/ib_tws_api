import ib_tws_api/client
import ib_tws_api/types
import ib_tws_api/protocol

pub type Client =
  client.Client

pub type ClientError =
  client.ClientError

pub type Contract =
  types.Contract

pub type Order =
  types.Order

pub type OrderState =
  types.OrderState

pub type Execution =
  types.Execution

pub type Tick =
  types.Tick

pub type MarketDepth =
  types.MarketDepth

pub type AccountSummary =
  types.AccountSummary

pub type Position =
  types.Position

pub type PortfolioValue =
  types.PortfolioValue

pub type Message =
  protocol.Message

pub fn new_client(host: String, port: Int, client_id: Int) -> Client {
  client.new_client(host, port, client_id)
}

pub fn connect(client: Client) -> Result(Client, ClientError) {
  client.connect(client)
}

pub fn disconnect(client: Client) -> Result(Nil, ClientError) {
  client.disconnect(client)
}

pub fn is_connected(client: Client) -> Bool {
  client.is_connected(client)
}

pub fn send_message(
  client: Client,
  msg: protocol.Message,
) -> Result(Nil, ClientError) {
  client.send_message(client, msg)
}

pub fn receive_message(
  client: Client,
  timeout: Int,
) -> Result(#(protocol.Message, Client), ClientError) {
  client.receive_message(client, timeout)
}
