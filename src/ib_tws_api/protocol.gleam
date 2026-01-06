import gleam/bit_array
import gleam/float
import gleam/int
import gleam/option
import gleam/result
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
  CancelMarketData(req_id: Int)
  MarketDataTick(ticker_id: Int, tick_type: Int, value: Float)
  OrderPlace(order: types.Order)
  CancelOrder(order_id: Int)
  OrderStatus(order_id: Int, status: String, filled: Float, remaining: Float)
  OpenOrder(order: types.Order)
  OpenOrderEnd
  ExecutionDetail(execution: types.Execution)
  ExecutionDetailEnd
  AccountSummaryRequest(req_id: Int, group: String, tags: List(String))
  AccountSummary(account: String, tag: String, value: String, currency: String)
  PositionsRequest
  Position(
    account: String,
    contract: types.Contract,
    position: Float,
    avg_cost: Float,
  )
  RealTimeBarsRequest(
    req_id: Int,
    contract: types.Contract,
    bar_size: Int,
    what_to_show: String,
    use_rth: Bool,
  )
  RealTimeBar(bar: types.RealTimeBar)
  CancelRealTimeBars(req_id: Int)
  Unknown(message_id: Int, data: BitArray)
}

pub const protocol_version = 176

pub const min_server_ver = 1

pub fn encode_message(msg: Message) -> BitArray {
  case msg {
    ConnectRequest(client_id) -> encode_connect_request(client_id)
    Disconnect -> encode_disconnect()
    Ping -> encode_ping()
    Pong -> encode_pong()
    MarketDataRequest(req_id, contract) ->
      encode_market_data_request(req_id, contract)
    CancelMarketData(req_id) -> encode_cancel_market_data(req_id)
    OrderPlace(order) -> encode_order_place(order)
    CancelOrder(order_id) -> encode_cancel_order(order_id)
    OpenOrder(order) -> encode_open_order(order)
    AccountSummaryRequest(req_id, group, tags) ->
      encode_account_summary_request(req_id, group, tags)
    PositionsRequest -> encode_positions_request()
    RealTimeBarsRequest(req_id, contract, bar_size, what_to_show, use_rth) ->
      encode_realtime_bars_request(
        req_id,
        contract,
        bar_size,
        what_to_show,
        use_rth,
      )
    CancelRealTimeBars(req_id) -> encode_cancel_realtime_bars(req_id)
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
        47 -> decode_open_order(data)
        48 -> Ok(OpenOrderEnd)
        49 -> decode_execution_detail(data)
        50 -> Ok(ExecutionDetailEnd)
        52 -> decode_realtime_bar(data)
        _ -> Ok(Unknown(message_id, data))
      }
    }
    Error(err) -> Error(err)
  }
}

fn parse_message_id(data: BitArray) -> Result(Int, String) {
  case data {
    <<message_id:int-little-size(32), _rest:bits>> -> Ok(message_id)
    _ -> Error("Invalid message format")
  }
}

fn encode_connect_request(client_id: Int) -> BitArray {
  <<
    protocol_version:int-little-size(32),
    client_id:int-little-size(32),
  >>
}

fn decode_connect_ack(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:int-little-size(32), version:int-little-size(32), rest:bits>> -> {
      use #(server_time, _remaining) <- result.try(decode_string_null_terminated(rest))
      Ok(ConnectAck(version, server_time))
    }
    _ -> Error("Invalid connect ack format")
  }
}

fn decode_connect_failed(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:int-little-size(32), rest:bits>> -> {
      use #(reason, _remaining) <- result.try(decode_string_null_terminated(
        rest,
      ))
      Ok(ConnectFailed(reason))
    }
    _ -> Error("Invalid connect failed format")
  }
}

fn encode_market_data_request(req_id: Int, contract: types.Contract) -> BitArray {
  let symbol_str = contract.symbol
  let sec_type_str = contract.security_type
  let exchange_str = contract.exchange
  let currency_str = contract.currency
  let last_trade_date_str = contract.last_trade_date_or_contract_month
  let right_str = contract.right
  let multiplier_str = contract.multiplier
  let primary_exchange_str = contract.primary_exchange
  let generic_tick_list = ""

  <<
    1:int-little-size(32),
    req_id:int-little-size(32),
    contract.contract_id:int-little-size(32),
    symbol_str:utf8,
    0:size(8),
    sec_type_str:utf8,
    0:size(8),
    last_trade_date_str:utf8,
    0:size(8),
    contract.strike:float,
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
    <<
      _message_id:int-little-size(32),
      ticker_id:int-little-size(32),
      tick_type:int-little-size(32),
      price:float,
      _rest:bits,
    >> -> {
      Ok(MarketDataTick(ticker_id, tick_type, price))
    }
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
    3:int-little-size(32),
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
    <<
      _message_id:int-little-size(32),
      order_id:int-little-size(32),
      filled:float,
      remaining:float,
      _rest:bits,
    >> -> {
      Ok(OrderStatus(order_id, "", filled, remaining))
    }
    _ -> Error("Invalid order status format")
  }
}

fn decode_open_order(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:int-little-size(32), rest:bits>> -> {
      use #(order_id, rest1) <- result.try(decode_int(rest))
      use #(contract_id_str, rest2) <- result.try(decode_string_null_terminated(
        rest1,
      ))
      use #(symbol, rest3) <- result.try(decode_string_null_terminated(rest2))
      use #(security_type, rest4) <- result.try(decode_string_null_terminated(
        rest3,
      ))
      use #(last_trade_date, rest5) <- result.try(decode_string_null_terminated(
        rest4,
      ))
      use #(strike, rest6) <- result.try(decode_float(rest5))
      use #(right, rest7) <- result.try(decode_string_null_terminated(rest6))
      use #(multiplier, rest8) <- result.try(decode_string_null_terminated(
        rest7,
      ))
      use #(exchange, rest9) <- result.try(decode_string_null_terminated(rest8))
      use #(primary_exchange, rest10) <- result.try(
        decode_string_null_terminated(rest9),
      )
      use #(currency, rest11) <- result.try(decode_string_null_terminated(
        rest10,
      ))
      use #(_local_symbol, rest12) <- result.try(decode_string_null_terminated(
        rest11,
      ))
      use #(_trading_class, rest13) <- result.try(decode_string_null_terminated(
        rest12,
      ))
      use #(_action, rest14) <- result.try(decode_string_null_terminated(rest13))
      use #(total_quantity, rest15) <- result.try(decode_float(rest14))
      use #(_order_type, rest16) <- result.try(decode_string_null_terminated(
        rest15,
      ))
      use #(limit_price, rest17) <- result.try(decode_float(rest16))
      use #(stop_price, rest18) <- result.try(decode_float(rest17))
      use #(_time_in_force, rest19) <- result.try(decode_string_null_terminated(
        rest18,
      ))
      use #(_oca_group, rest20) <- result.try(decode_string_null_terminated(
        rest19,
      ))
      use #(_account, rest21) <- result.try(decode_string_null_terminated(
        rest20,
      ))
      use #(outside_rth, rest22) <- result.try(decode_int(rest21))
      use #(hidden, rest23) <- result.try(decode_int(rest22))
      use #(display_size, rest24) <- result.try(decode_float(rest23))
      use #(trail_stop_price, rest25) <- result.try(decode_float(rest24))
      use #(parent_id, _remaining) <- result.try(decode_int(rest25))

      let _contract =
        types.Contract(
          contract_id: int.parse(contract_id_str) |> result.unwrap(0),
          symbol: symbol,
          security_type: security_type,
          exchange: exchange,
          currency: currency,
          last_trade_date_or_contract_month: last_trade_date,
          strike: strike,
          right: right,
          multiplier: multiplier,
          primary_exchange: primary_exchange,
        )
      let order =
        types.Order(
          order_id: order_id,
          client_id: 0,
          action: "",
          total_quantity: total_quantity,
          order_type: "",
          limit_price: option.Some(limit_price),
          stop_price: option.Some(stop_price),
          time_in_force: "",
          oca_group: "",
          account: "",
          outside_rth: outside_rth == 1,
          hidden: hidden == 1,
          display_size: float.truncate(display_size),
          trail_stop_price: option.Some(trail_stop_price),
          parent_id: parent_id,
        )
      Ok(OpenOrder(order))
    }
    _ -> Error("Invalid open order format")
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
    6:int-little-size(32),
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
    <<_message_id:int-little-size(32), rest:bits>> -> {
      use #(_req_id, rest1) <- result.try(decode_int(rest))
      use #(account, rest2) <- result.try(decode_string_null_terminated(rest1))
      use #(tag, rest3) <- result.try(decode_string_null_terminated(rest2))
      use #(value, rest4) <- result.try(decode_string_null_terminated(rest3))
      use #(currency, _remaining) <- result.try(decode_string_null_terminated(
        rest4,
      ))
      Ok(AccountSummary(account, tag, value, currency))
    }
    _ -> Error("Invalid account summary format")
  }
}

fn encode_positions_request() -> BitArray {
  <<61:int-little-size(32)>>
}

fn decode_position(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:int-little-size(32), rest:bits>> -> {
      use #(account, rest1) <- result.try(decode_string_null_terminated(rest))
      use #(contract, rest2) <- result.try(decode_contract(rest1))
      use #(position, rest3) <- result.try(decode_float(rest2))
      use #(avg_cost, _remaining) <- result.try(decode_float(rest3))
      Ok(Position(account, contract, position, avg_cost))
    }
    _ -> Error("Invalid position format")
  }
}

fn decode_contract(
  data: BitArray,
) -> Result(#(types.Contract, BitArray), String) {
  use #(contract_id, rest1) <- result.try(decode_int(data))
  use #(symbol, rest2) <- result.try(decode_string_null_terminated(rest1))
  use #(security_type, rest3) <- result.try(decode_string_null_terminated(rest2))
  use #(last_trade_date, rest4) <- result.try(decode_string_null_terminated(
    rest3,
  ))
  use #(strike, rest5) <- result.try(decode_float(rest4))
  use #(right, rest6) <- result.try(decode_string_null_terminated(rest5))
  use #(multiplier, rest7) <- result.try(decode_string_null_terminated(rest6))
  use #(exchange, rest8) <- result.try(decode_string_null_terminated(rest7))
  use #(currency, rest9) <- result.try(decode_string_null_terminated(rest8))
  use #(_local_symbol, rest10) <- result.try(decode_string_null_terminated(
    rest9,
  ))
  use #(_trading_class, remaining) <- result.try(decode_string_null_terminated(
    rest10,
  ))

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

fn encode_ping() -> BitArray {
  <<8:int-little-size(32)>>
}

fn decode_ping(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:int-little-size(32), _rest:bits>> -> Ok(Ping)
    _ -> Error("Invalid ping format")
  }
}

fn encode_disconnect() -> BitArray {
  <<5:int-little-size(32)>>
}

fn encode_pong() -> BitArray {
  <<9:int-little-size(32)>>
}

fn encode_cancel_market_data(req_id: Int) -> BitArray {
  <<2:int-little-size(32), req_id:int-little-size(32)>>
}

fn encode_cancel_order(order_id: Int) -> BitArray {
  <<4:int-little-size(32), order_id:int-little-size(32)>>
}

fn encode_open_order(order: types.Order) -> BitArray {
  encode_order_place(order)
}

fn decode_execution_detail(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:int-little-size(32), rest:bits>> -> {
      use #(order_id, rest2) <- result.try(decode_int(rest))
      use #(client_id, rest3) <- result.try(decode_int(rest2))
      use #(exec_id, rest4) <- result.try(decode_string_null_terminated(rest3))
      use #(time, rest5) <- result.try(decode_string_null_terminated(rest4))
      use #(acct_number, rest6) <- result.try(decode_string_null_terminated(
        rest5,
      ))
      use #(exchange, rest7) <- result.try(decode_string_null_terminated(rest6))
      use #(side, rest8) <- result.try(decode_string_null_terminated(rest7))
      use #(shares, rest9) <- result.try(decode_float(rest8))
      use #(price, rest10) <- result.try(decode_float(rest9))
      use #(perm_id, rest11) <- result.try(decode_int(rest10))
      use #(client_order_id, rest12) <- result.try(decode_int(rest11))
      use #(liquidation, rest13) <- result.try(decode_int(rest12))
      use #(cum_qty, rest14) <- result.try(decode_float(rest13))
      use #(avg_price, rest15) <- result.try(decode_float(rest14))
      use #(order_ref, rest16) <- result.try(decode_string_null_terminated(
        rest15,
      ))
      use #(ev_rule, rest17) <- result.try(decode_string_null_terminated(rest16))
      use #(ev_multiplier, rest18) <- result.try(decode_float(rest17))
      use #(model_code, rest19) <- result.try(decode_string_null_terminated(
        rest18,
      ))
      use #(last_liquidity, _remaining) <- result.try(decode_int(rest19))

      let execution =
        types.Execution(
          order_id: order_id,
          client_id: client_id,
          exec_id: exec_id,
          time: time,
          acct_number: acct_number,
          exchange: exchange,
          side: side,
          shares: shares,
          price: price,
          perm_id: perm_id,
          client_order_id: client_order_id,
          liquidation: liquidation,
          cum_qty: cum_qty,
          avg_price: avg_price,
          order_ref: order_ref,
          ev_rule: ev_rule,
          ev_multiplier: ev_multiplier,
          model_code: model_code,
          last_liquidity: last_liquidity,
        )
      Ok(ExecutionDetail(execution))
    }
    _ -> Error("Invalid execution detail format")
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
  <<i:int-little-size(32)>>
}

pub fn decode_int(data: BitArray) -> Result(#(Int, BitArray), String) {
  case data {
    <<i:int-little-size(32), rest:bits>> -> Ok(#(i, rest))
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

fn encode_realtime_bars_request(
  req_id: Int,
  contract: types.Contract,
  bar_size: Int,
  what_to_show: String,
  use_rth: Bool,
) -> BitArray {
  let contract_id_str = int.to_string(contract.contract_id)
  let bar_size_str = int.to_string(bar_size)
  let use_rth_int = case use_rth {
    True -> 1
    False -> 0
  }

  <<
    50:int-little-size(32),
    req_id:int-little-size(32),
    contract_id_str:utf8,
    0:size(8),
    contract.symbol:utf8,
    0:size(8),
    contract.security_type:utf8,
    0:size(8),
    contract.last_trade_date_or_contract_month:utf8,
    0:size(8),
    contract.strike:float,
    contract.right:utf8,
    0:size(8),
    contract.multiplier:utf8,
    0:size(8),
    contract.exchange:utf8,
    0:size(8),
    contract.primary_exchange:utf8,
    0:size(8),
    contract.currency:utf8,
    0:size(8),
    bar_size_str:utf8,
    0:size(8),
    what_to_show:utf8,
    0:size(8),
    use_rth_int:int-little-size(32),
  >>
}

fn encode_cancel_realtime_bars(req_id: Int) -> BitArray {
  <<51:int-little-size(32), req_id:int-little-size(32)>>
}

fn decode_realtime_bar(data: BitArray) -> Result(Message, String) {
  case data {
    <<_message_id:int-little-size(32), rest:bits>> -> {
      use #(req_id, rest1) <- result.try(decode_int(rest))
      use #(time, rest2) <- result.try(decode_int(rest1))
      use #(open, rest3) <- result.try(decode_float(rest2))
      use #(high, rest4) <- result.try(decode_float(rest3))
      use #(low, rest5) <- result.try(decode_float(rest4))
      use #(close, rest6) <- result.try(decode_float(rest5))
      use #(volume, rest7) <- result.try(decode_int(rest6))
      use #(wap, rest8) <- result.try(decode_float(rest7))
      use #(count, _remaining) <- result.try(decode_int(rest8))

      let bar =
        types.RealTimeBar(
          req_id: req_id,
          time: time,
          open: open,
          high: high,
          low: low,
          close: close,
          volume: volume,
          wap: wap,
          count: count,
        )
      Ok(RealTimeBar(bar))
    }
    _ -> Error("Invalid realtime bar format")
  }
}
