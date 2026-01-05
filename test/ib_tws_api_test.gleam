import gleam/option
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

pub fn protocol_decode_connect_ack_test() {
  let version = 176
  let server_time = "2024-01-01 12:00:00"
  let data = <<4:size(32), version:size(32), server_time:utf8, 0:size(8)>>

  case protocol.decode_message(data) {
    Ok(protocol.ConnectAck(v, t)) -> {
      v |> should.equal(version)
      t |> should.equal(server_time)
    }
    _ -> should.be_true(False)
  }
}

pub fn protocol_decode_connect_failed_test() {
  let error_msg = "Authentication failed"
  let data = <<5:size(32), error_msg:utf8, 0:size(8)>>

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
    6:size(32),
    1:size(32),
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
    61:size(32),
    account:utf8,
    0:size(8),
    contract_id:size(32),
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
  let data = <<test_value:size(32)>>

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
