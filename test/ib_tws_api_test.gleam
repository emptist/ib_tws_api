// import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option
import gleam/string
import gleeunit
import gleeunit/should
import ib_tws_api
import ib_tws_api/protocol
import ib_tws_api/types.{Contract, Order}

pub fn main() {
  gleeunit.main()
}

pub fn client_creation_test() {
  let client = ib_tws_api.new_client("127.0.0.1", 7497, 1)
  ib_tws_api.is_connected(client) |> should.be_false()
}

pub fn contract_creation_test() {
  let contract =
    Contract(
      contract_id: 0,
      symbol: "AAPL",
      security_type: "STK",
      exchange: "SMART",
      currency: "USD",
      last_trade_date_or_contract_month: "",
      strike: 0.0,
      right: "",
      multiplier: "",
      primary_exchange: "",
    )
  contract.symbol |> should.equal("AAPL")
}

pub fn order_creation_test() {
  let order =
    Order(
      order_id: 1,
      client_id: 1,
      order_type: "MKT",
      action: "BUY",
      total_quantity: 100.0,
      limit_price: option.None,
      stop_price: option.None,
      time_in_force: "DAY",
      oca_group: "",
      account: "DU123456",
      outside_rth: False,
      hidden: False,
      display_size: 0,
      trail_stop_price: option.None,
      parent_id: 0,
    )
  order.action |> should.equal("BUY")
  order.total_quantity |> should.equal(100.0)
}

pub fn protocol_encode_connect_request_test() {
  let msg = protocol.ConnectRequest(1)
  let data = protocol.encode_message(msg)
  data |> should.not_equal(<<>>)
}

pub fn protocol_encode_disconnect_test() {
  let msg = protocol.Disconnect
  let data = protocol.encode_message(msg)
  data |> should.not_equal(<<>>)
}

pub fn protocol_encode_ping_test() {
  let msg = protocol.Ping
  let data = protocol.encode_message(msg)
  data |> should.not_equal(<<>>)
}

pub fn protocol_encode_pong_test() {
  let msg = protocol.Pong
  let data = protocol.encode_message(msg)
  data |> should.not_equal(<<>>)
}

pub fn protocol_encode_cancel_market_data_test() {
  let msg = protocol.CancelMarketData(100)
  let data = protocol.encode_message(msg)
  data |> should.not_equal(<<>>)
}

pub fn protocol_encode_cancel_order_test() {
  let msg = protocol.CancelOrder(1001)
  let data = protocol.encode_message(msg)
  data |> should.not_equal(<<>>)
}

pub fn protocol_decode_connect_ack_test() {
  let version = 176
  let server_time = "2024-01-01 12:00:00"
  let data = <<4:int-little-size(32), version:int-little-size(32), server_time:utf8, 0:size(8)>>

  let result = protocol.decode_message(data)
  io.println("ConnectAck result: " <> result |> string.inspect)

  case result {
    Ok(protocol.ConnectAck(v, t)) -> {
      v |> should.equal(176)
      t |> should.equal(server_time)
    }
    _ -> should.be_true(False)
  }
}

pub fn protocol_decode_connect_failed_test() {
  let error_msg = "Authentication failed"
  let data = <<5:int-little-size(32), error_msg:utf8, 0:size(8)>>

  case protocol.decode_message(data) {
    Ok(protocol.ConnectFailed(msg)) -> {
      msg |> should.equal(error_msg)
    }
    _ -> should.be_true(False)
  }
}

pub fn protocol_decode_account_summary_test() {
  let account = "DU123456"
  let tag = "TotalCashBalance"
  let value = "100000.00"
  let currency = "USD"
  let data = <<
    6:int-little-size(32),
    1:int-little-size(32),
    account:utf8,
    0:size(8),
    tag:utf8,
    0:size(8),
    value:utf8,
    0:size(8),
    currency:utf8,
    0:size(8),
  >>

  case protocol.decode_message(data) {
    Ok(protocol.AccountSummary(acc, t, v, c)) -> {
      acc |> should.equal(account)
      t |> should.equal(tag)
      v |> should.equal(value)
      c |> should.equal(currency)
    }
    _ -> should.be_true(False)
  }
}

pub fn protocol_decode_position_test() {
  let account = "DU123456"
  let contract_id = 123_456
  let symbol = "AAPL"
  let security_type = "STK"
  let last_trade_date = ""
  let strike = 0.0
  let right = ""
  let multiplier = ""
  let exchange = "SMART"
  let currency = "USD"
  let local_symbol = ""
  let trading_class = ""
  let position = 100.0
  let avg_cost = 150.25

  let data = <<
    61:int-little-size(32),
    account:utf8,
    0:size(8),
    contract_id:int-little-size(32),
    symbol:utf8,
    0:size(8),
    security_type:utf8,
    0:size(8),
    last_trade_date:utf8,
    0:size(8),
    strike:float,
    right:utf8,
    0:size(8),
    multiplier:utf8,
    0:size(8),
    exchange:utf8,
    0:size(8),
    currency:utf8,
    0:size(8),
    local_symbol:utf8,
    0:size(8),
    trading_class:utf8,
    0:size(8),
    position:float,
    avg_cost:float,
  >>

  case protocol.decode_message(data) {
    Ok(protocol.Position(acc, contract, pos, cost)) -> {
      acc |> should.equal(account)
      contract.contract_id |> should.equal(contract_id)
      contract.symbol |> should.equal(symbol)
      contract.security_type |> should.equal(security_type)
      contract.exchange |> should.equal(exchange)
      contract.currency |> should.equal(currency)
      pos |> should.equal(position)
      cost |> should.equal(avg_cost)
    }
    _ -> should.be_true(False)
  }
}

pub fn protocol_decode_string_null_terminated_test() {
  let test_string = "Hello, World!"
  let data = <<test_string:utf8, 0:size(8)>>

  case protocol.decode_string_null_terminated(data) {
    Ok(#(str, rest)) -> {
      str |> should.equal(test_string)
      rest |> should.equal(<<>>)
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn protocol_decode_int_test() {
  let test_value = 123_456_789
  let data = <<test_value:int-little-size(32)>>

  case protocol.decode_int(data) {
    Ok(#(value, rest)) -> {
      value |> should.equal(test_value)
      rest |> should.equal(<<>>)
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn protocol_decode_float_test() {
  let test_value = 123.456
  let data = <<test_value:float>>

  case protocol.decode_float(data) {
    Ok(#(value, rest)) -> {
      value |> should.equal(test_value)
      rest |> should.equal(<<>>)
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn protocol_encode_open_order_test() {
  let _contract =
    Contract(
      contract_id: 123_456,
      symbol: "AAPL",
      security_type: "STK",
      exchange: "SMART",
      currency: "USD",
      last_trade_date_or_contract_month: "",
      strike: 0.0,
      right: "",
      multiplier: "",
      primary_exchange: "",
    )
  let order =
    Order(
      order_id: 1001,
      client_id: 1,
      order_type: "MKT",
      action: "BUY",
      total_quantity: 100.0,
      limit_price: option.None,
      stop_price: option.None,
      time_in_force: "DAY",
      oca_group: "",
      account: "DU123456",
      outside_rth: False,
      hidden: False,
      display_size: 0,
      trail_stop_price: option.None,
      parent_id: 0,
    )
  let msg = protocol.OpenOrder(order)
  let data = protocol.encode_message(msg)
  data |> should.not_equal(<<>>)
}

pub fn protocol_decode_open_order_test() {
  let order_id = 1001
  let contract_id = 123_456
  let contract_id_str = int.to_string(contract_id)
  let symbol = "AAPL"
  let security_type = "STK"
  let last_trade_date = ""
  let strike = 0.0
  let right = ""
  let multiplier = ""
  let exchange = "SMART"
  let primary_exchange = ""
  let currency = "USD"
  let local_symbol = ""
  let trading_class = ""
  let action = "BUY"
  let total_quantity = 100.0
  let order_type = "MKT"
  let limit_price = 150.0
  let stop_price = 149.0
  let time_in_force = "DAY"
  let oca_group = ""
  let account = "DU123456"
  let outside_rth = 0
  let hidden = 0
  let display_size = 0.0
  let trail_stop_price = 0.0
  let parent_id = 0

  let data = <<
    47:int-little-size(32),
    order_id:int-little-size(32),
    contract_id_str:utf8,
    0:size(8),
    symbol:utf8,
    0:size(8),
    security_type:utf8,
    0:size(8),
    last_trade_date:utf8,
    0:size(8),
    strike:float,
    right:utf8,
    0:size(8),
    multiplier:utf8,
    0:size(8),
    exchange:utf8,
    0:size(8),
    primary_exchange:utf8,
    0:size(8),
    currency:utf8,
    0:size(8),
    local_symbol:utf8,
    0:size(8),
    trading_class:utf8,
    0:size(8),
    action:utf8,
    0:size(8),
    total_quantity:float,
    order_type:utf8,
    0:size(8),
    limit_price:float,
    stop_price:float,
    time_in_force:utf8,
    0:size(8),
    oca_group:utf8,
    0:size(8),
    account:utf8,
    0:size(8),
    outside_rth:int-little-size(32),
    hidden:int-little-size(32),
    display_size:float,
    trail_stop_price:float,
    parent_id:int-little-size(32),
  >>

  case protocol.decode_message(data) {
    Ok(protocol.OpenOrder(order)) -> {
      order.order_id |> should.equal(order_id)
      order.client_id |> should.equal(0)
      order.total_quantity |> should.equal(total_quantity)
    }
    _ -> should.be_true(False)
  }
}

pub fn protocol_decode_open_order_end_test() {
  let data = <<48:int-little-size(32)>>

  case protocol.decode_message(data) {
    Ok(protocol.OpenOrderEnd) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

pub fn protocol_encode_realtime_bars_request_test() {
  let contract =
    types.Contract(
      contract_id: 123_456,
      symbol: "AAPL",
      security_type: "STK",
      exchange: "SMART",
      currency: "USD",
      last_trade_date_or_contract_month: "",
      strike: 0.0,
      right: "",
      multiplier: "",
      primary_exchange: "",
    )
  let msg =
    protocol.RealTimeBarsRequest(
      req_id: 1,
      contract: contract,
      bar_size: 5,
      what_to_show: "MIDPOINT",
      use_rth: False,
    )
  let data = protocol.encode_message(msg)
  data |> should.not_equal(<<>>)
}

pub fn protocol_encode_cancel_realtime_bars_test() {
  let msg = protocol.CancelRealTimeBars(req_id: 1)
  let data = protocol.encode_message(msg)
  data |> should.not_equal(<<>>)
}

pub fn protocol_decode_realtime_bar_test() {
  let req_id = 1
  let time = 1_234_567_890
  let open = 150.0
  let high = 151.0
  let low = 149.0
  let close = 150.5
  let volume = 1000
  let wap = 150.25
  let count = 10

  let data = <<
    52:int-little-size(32),
    req_id:int-little-size(32),
    time:int-little-size(32),
    open:float,
    high:float,
    low:float,
    close:float,
    volume:int-little-size(32),
    wap:float,
    count:int-little-size(32),
  >>

  case protocol.decode_message(data) {
    Ok(protocol.RealTimeBar(bar)) -> {
      bar.req_id |> should.equal(req_id)
      bar.time |> should.equal(time)
      bar.open |> should.equal(open)
      bar.high |> should.equal(high)
      bar.low |> should.equal(low)
      bar.close |> should.equal(close)
      bar.volume |> should.equal(volume)
      bar.wap |> should.equal(wap)
      bar.count |> should.equal(count)
    }
    _ -> should.be_true(False)
  }
}
