import gleam/bit_array
import gleam/float
import gleam/int
import gleam/option
import gleam/string
import ib_tws_api/types

pub type Message {
  ConnectRequest(client_id: Int)
  ConnectAck(version: Int, server_time: String)
  ConnectFailed(reason: String)
  Disconnect
  Ping
  Pong
  MarketDataRequest(req_id: Int, contract: types.Contract)
  MarketDataTick(ticker_id: Int, tick_type: Int, value: Float)
  OrderPlace(order: types.Order)
  OrderStatus(order_id: Int, status: String, filled: Float, remaining: Float)
  AccountSummaryRequest(req_id: Int, group: String, tags: List(String))
  AccountSummary(account: String, tag: String, value: String, currency: String)
  PositionsRequest
  Position(
    account: String,
    contract: types.Contract,
    position: Float,
    avg_cost: Float,
  )
  Unknown(message_id: Int, data: BitArray)
}

pub const protocol_version = 176

pub const min_server_ver = 1

pub fn encode_message(msg: Message) -> BitArray {
  case msg {
    ConnectRequest(client_id) -> encode_connect_request(client_id)
    MarketDataRequest(req_id, contract) ->
      encode_market_data_request(req_id, contract)
    OrderPlace(order) -> encode_order_place(order)
    AccountSummaryRequest(req_id, group, tags) ->
      encode_account_summary_request(req_id, group, tags)
    PositionsRequest -> encode_positions_request()
    Ping -> encode_ping()
    _ -> <<>>
  }
}

pub fn decode_message(data: BitArray) -> Result(Message, String) {
  case parse_message_id(data) {
    Ok(message_id) -> {
      case message_id {
        4 -> decode_connect_ack(data)
        5 -> decode_connect_failed(data)
        9 -> decode_market_data_tick(data)
        3 -> decode_order_status(data)
        6 -> decode_account_summary(data)
        61 -> decode_position(data)
        8 -> decode_ping(data)
        _ -> Ok(Unknown(message_id, data))
      }
    }
    Error(err) -> Error(err)
  }
}

fn parse_message_id(data: BitArray) -> Result(Int, String) {
  case data {
    <<message_id:32, _rest>> -> Ok(message_id)
    _ -> Error("Invalid message format")
  }
}

fn encode_connect_request(client_id: Int) -> BitArray {
  let version = protocol_version
  let client_id_str = int.to_string(client_id)
  let version_str = int.to_string(version)

  <<
    version:size(32),
    client_id_str:utf8,
    0:size(8),
    version_str:utf8,
    0:size(8),
  >>
}

fn decode_connect_ack(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:32, version:32, rest:bits>> -> {
      case decode_string_null_terminated(rest) {
        Ok(#(server_time, _remaining)) -> Ok(ConnectAck(version, server_time))
        Error(err) -> Error("Failed to decode server time: " <> err)
      }
    }
    _ -> Error("Invalid connect ack format")
  }
}

fn decode_connect_failed(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:32, rest:bits>> -> {
      case decode_string_null_terminated(rest) {
        Ok(#(reason, _remaining)) -> Ok(ConnectFailed(reason))
        Error(err) -> Error("Failed to decode connect failed reason: " <> err)
      }
    }
    _ -> Error("Invalid connect failed format")
  }
}

fn encode_market_data_request(req_id: Int, contract: types.Contract) -> BitArray {
  let contract_id_str = int.to_string(contract.contract_id)
  let symbol_str = contract.symbol
  let sec_type_str = contract.security_type
  let exchange_str = contract.exchange
  let currency_str = contract.currency
  let last_trade_date_str = contract.last_trade_date_or_contract_month
  let strike_str = float.to_string(contract.strike)
  let right_str = contract.right
  let multiplier_str = contract.multiplier
  let primary_exchange_str = contract.primary_exchange
  let generic_tick_list = ""

  <<
    9:size(32),
    req_id:size(32),
    contract_id_str:utf8,
    0:size(8),
    symbol_str:utf8,
    0:size(8),
    sec_type_str:utf8,
    0:size(8),
    last_trade_date_str:utf8,
    0:size(8),
    strike_str:utf8,
    0:size(8),
    right_str:utf8,
    0:size(8),
    multiplier_str:utf8,
    0:size(8),
    exchange_str:utf8,
    0:size(8),
    primary_exchange_str:utf8,
    0:size(8),
    currency_str:utf8,
    0:size(8),
    generic_tick_list:utf8,
    0:size(8),
    0:size(8),
    0:size(8),
  >>
}

fn decode_market_data_tick(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:32, ticker_id:32, tick_type:32, price:float, _rest>> ->
      Ok(MarketDataTick(ticker_id, tick_type, price))
    _ -> Error("Invalid market data tick format")
  }
}

fn encode_order_place(order: types.Order) -> BitArray {
  let order_id_str = int.to_string(order.order_id)
  let client_id_str = int.to_string(order.client_id)
  let action_str = order.action
  let quantity_str = float.to_string(order.total_quantity)
  let order_type_str = order.order_type
  let limit_price_str = case order.limit_price {
    option.Some(price) -> float.to_string(price)
    option.None -> ""
  }
  let stop_price_str = case order.stop_price {
    option.Some(price) -> float.to_string(price)
    option.None -> ""
  }
  let tif_str = order.time_in_force
  let oca_group_str = order.oca_group
  let account_str = order.account
  let outside_rth = case order.outside_rth {
    True -> 1
    False -> 0
  }
  let hidden = case order.hidden {
    True -> 1
    False -> 0
  }
  let display_size_str = int.to_string(order.display_size)
  let trail_stop_price_str = case order.trail_stop_price {
    option.Some(price) -> float.to_string(price)
    option.None -> ""
  }
  let parent_id_str = int.to_string(order.parent_id)

  <<
    3:size(32),
    order_id_str:utf8,
    0:size(8),
    client_id_str:utf8,
    0:size(8),
    action_str:utf8,
    0:size(8),
    quantity_str:utf8,
    0:size(8),
    order_type_str:utf8,
    0:size(8),
    limit_price_str:utf8,
    0:size(8),
    stop_price_str:utf8,
    0:size(8),
    tif_str:utf8,
    0:size(8),
    oca_group_str:utf8,
    0:size(8),
    account_str:utf8,
    0:size(8),
    outside_rth:size(8),
    hidden:size(8),
    display_size_str:utf8,
    0:size(8),
    trail_stop_price_str:utf8,
    0:size(8),
    parent_id_str:utf8,
    0:size(8),
  >>
}

fn decode_order_status(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:32, order_id:32, filled:float, remaining:float, _rest>> ->
      Ok(OrderStatus(order_id, "", filled, remaining))
    _ -> Error("Invalid order status format")
  }
}

fn encode_account_summary_request(
  req_id: Int,
  group: String,
  tags: List(String),
) -> BitArray {
  let req_id_str = int.to_string(req_id)
  let tags_str = string.join(tags, ",")

  <<
    6:size(32),
    req_id_str:utf8,
    0:size(8),
    group:utf8,
    0:size(8),
    tags_str:utf8,
    0:size(8),
  >>
}

fn decode_account_summary(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:32, rest:bits>> -> {
      case decode_int(rest) {
        Ok(#(_req_id, rest1)) -> {
          case decode_string_null_terminated(rest1) {
            Ok(#(account, rest2)) -> {
              case decode_string_null_terminated(rest2) {
                Ok(#(tag, rest3)) -> {
                  case decode_string_null_terminated(rest3) {
                    Ok(#(value, rest4)) -> {
                      case decode_string_null_terminated(rest4) {
                        Ok(#(currency, _remaining)) ->
                          Ok(AccountSummary(account, tag, value, currency))
                        Error(err) ->
                          Error("Failed to decode currency: " <> err)
                      }
                    }
                    Error(err) -> Error("Failed to decode value: " <> err)
                  }
                }
                Error(err) -> Error("Failed to decode tag: " <> err)
              }
            }
            Error(err) -> Error("Failed to decode account: " <> err)
          }
        }
        Error(err) -> Error("Failed to decode req_id: " <> err)
      }
    }
    _ -> Error("Invalid account summary format")
  }
}

fn encode_positions_request() -> BitArray {
  <<61:size(32)>>
}

fn decode_position(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:32, rest:bits>> -> {
      case decode_string_null_terminated(rest) {
        Ok(#(account, rest1)) -> {
          case decode_contract(rest1) {
            Ok(#(contract, rest2)) -> {
              case decode_float(rest2) {
                Ok(#(position, rest3)) -> {
                  case decode_float(rest3) {
                    Ok(#(avg_cost, _remaining)) ->
                      Ok(Position(account, contract, position, avg_cost))
                    Error(err) -> Error("Failed to decode avg_cost: " <> err)
                  }
                }
                Error(err) -> Error("Failed to decode position: " <> err)
              }
            }
            Error(err) -> Error("Failed to decode contract: " <> err)
          }
        }
        Error(err) -> Error("Failed to decode account: " <> err)
      }
    }
    _ -> Error("Invalid position format")
  }
}

fn decode_contract(
  data: BitArray,
) -> Result(#(types.Contract, BitArray), String) {
  case decode_int(data) {
    Ok(#(contract_id, rest1)) -> {
      case decode_string_null_terminated(rest1) {
        Ok(#(symbol, rest2)) -> {
          case decode_string_null_terminated(rest2) {
            Ok(#(security_type, rest3)) -> {
              case decode_string_null_terminated(rest3) {
                Ok(#(last_trade_date, rest4)) -> {
                  case decode_float(rest4) {
                    Ok(#(strike, rest5)) -> {
                      case decode_string_null_terminated(rest5) {
                        Ok(#(right, rest6)) -> {
                          case decode_string_null_terminated(rest6) {
                            Ok(#(multiplier, rest7)) -> {
                              case decode_string_null_terminated(rest7) {
                                Ok(#(exchange, rest8)) -> {
                                  case decode_string_null_terminated(rest8) {
                                    Ok(#(currency, rest9)) -> {
                                      case
                                        decode_string_null_terminated(rest9)
                                      {
                                        Ok(#(_local_symbol, rest10)) -> {
                                          case
                                            decode_string_null_terminated(
                                              rest10,
                                            )
                                          {
                                            Ok(#(_trading_class, remaining)) -> {
                                              let contract =
                                                types.Contract(
                                                  contract_id: contract_id,
                                                  symbol: symbol,
                                                  security_type: security_type,
                                                  exchange: exchange,
                                                  currency: currency,
                                                  last_trade_date_or_contract_month: last_trade_date,
                                                  strike: strike,
                                                  right: right,
                                                  multiplier: multiplier,
                                                  primary_exchange: "",
                                                )
                                              Ok(#(contract, remaining))
                                            }
                                            Error(err) ->
                                              Error(
                                                "Failed to decode trading_class: "
                                                <> err,
                                              )
                                          }
                                        }
                                        Error(err) ->
                                          Error(
                                            "Failed to decode local_symbol: "
                                            <> err,
                                          )
                                      }
                                    }
                                    Error(err) ->
                                      Error(
                                        "Failed to decode currency: " <> err,
                                      )
                                  }
                                }
                                Error(err) ->
                                  Error("Failed to decode exchange: " <> err)
                              }
                            }
                            Error(err) ->
                              Error("Failed to decode multiplier: " <> err)
                          }
                        }
                        Error(err) -> Error("Failed to decode right: " <> err)
                      }
                    }
                    Error(err) -> Error("Failed to decode strike: " <> err)
                  }
                }
                Error(err) -> Error("Failed to decode last_trade_date: " <> err)
              }
            }
            Error(err) -> Error("Failed to decode security_type: " <> err)
          }
        }
        Error(err) -> Error("Failed to decode symbol: " <> err)
      }
    }
    Error(err) -> Error("Failed to decode contract_id: " <> err)
  }
}

fn encode_ping() -> BitArray {
  <<8:size(32)>>
}

fn decode_ping(data: BitArray) -> Result(Message, String) {
  case data {
    <<8:size(32), _rest>> -> Ok(Ping)
    _ -> Error("Invalid ping format")
  }
}

pub fn encode_string_null_terminated(s: String) -> BitArray {
  <<s:utf8, 0:size(8)>>
}

pub fn decode_string_null_terminated(
  data: BitArray,
) -> Result(#(String, BitArray), String) {
  do_decode_string_null_terminated(data, <<>>)
}

fn do_decode_string_null_terminated(
  data: BitArray,
  acc: BitArray,
) -> Result(#(String, BitArray), String) {
  case data {
    <<>> -> Error("Empty data")
    <<0:size(8), rest:bits>> -> {
      let bytes = bit_array.append(acc, <<>>)
      case bit_array.to_string(bytes) {
        Ok(str) -> Ok(#(str, rest))
        Error(_) -> Error("Failed to convert bytes to string")
      }
    }
    <<b:8, rest:bits>> ->
      do_decode_string_null_terminated(rest, bit_array.append(acc, <<b:8>>))
    _ -> Error("Invalid string format")
  }
}

pub fn encode_int(i: Int) -> BitArray {
  <<i:size(32)>>
}

pub fn decode_int(data: BitArray) -> Result(#(Int, BitArray), String) {
  case data {
    <<i:32, rest:bits>> -> Ok(#(i, rest))
    _ -> Error("Invalid integer format")
  }
}

pub fn encode_float(f: Float) -> BitArray {
  <<f:float>>
}

pub fn decode_float(data: BitArray) -> Result(#(Float, BitArray), String) {
  case data {
    <<f:float, rest:bits>> -> Ok(#(f, rest))
    _ -> Error("Invalid float format")
  }
}

pub fn encode_bool(b: Bool) -> BitArray {
  case b {
    True -> <<1:size(8)>>
    False -> <<0:size(8)>>
  }
}

pub fn decode_bool(data: BitArray) -> Result(#(Bool, BitArray), String) {
  case data {
    <<0:size(8), rest:bits>> -> Ok(#(False, rest))
    <<1:size(8), rest:bits>> -> Ok(#(True, rest))
    _ -> Error("Invalid boolean format")
  }
}

pub fn encode_string_list(list: List(String)) -> BitArray {
  case list {
    [] -> <<>>
    [head, ..tail] -> {
      let encoded_head = encode_string_null_terminated(head)
      let encoded_tail = encode_string_list(tail)
      bit_array.append(encoded_head, encoded_tail)
    }
  }
}

pub fn decode_string_list(
  data: BitArray,
  count: Int,
) -> Result(#(List(String), BitArray), String) {
  case count {
    0 -> Ok(#([], data))
    _ -> {
      case decode_string_null_terminated(data) {
        Ok(#(str, rest)) -> {
          case decode_string_list(rest, count - 1) {
            Ok(#(list, remaining)) -> Ok(#([str, ..list], remaining))
            Error(err) -> Error(err)
          }
        }
        Error(err) -> Error(err)
      }
    }
  }
}
