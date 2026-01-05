import gleam/int
import gleam/float
import gleam/io
import gleam/list
import ib_tws_api
import ib_tws_api/client.{
  type ClientError, AuthenticationFailed, ConnectionFailed, SocketError,
}
import ib_tws_api/protocol
import ib_tws_api/types

pub fn main() {
  io.println("=")
  io.println("IB TWS API - Real Integration Test")
  io.println("=")
  io.println("")

  let host = "127.0.0.1"
  let port = 7496
  let client_id = 1

  io.println("Configuration:")
  io.println("  Host: " <> host)
  io.println("  Port: " <> int.to_string(port) <> " (Live Trading)")
  io.println("  Client ID: " <> int.to_string(client_id))
  io.println("")

  io.println("Attempting to connect to IB TWS...")
  let client = ib_tws_api.new_client(host, port, client_id)

  case ib_tws_api.connect(client) {
    Ok(connected_client) -> {
      io.println("✓ Successfully connected to TWS!")
      io.println("")

      case ib_tws_api.is_connected(connected_client) {
        True -> io.println("✓ Client connection status: Connected")
        False -> io.println("✗ Client connection status: Not connected")
      }
      io.println("")

      test_account_summary(connected_client)
      test_open_orders(connected_client)
      test_positions(connected_client)
      test_market_data(connected_client)
      test_realtime_bars(connected_client)

      io.println("")
      io.println("Disconnecting...")
      case ib_tws_api.disconnect(connected_client) {
        Ok(_) -> io.println("✓ Disconnected successfully")
        Error(err) -> io.println("✗ Error disconnecting: " <> inspect_error(err))
      }
    }
    Error(err) -> {
      io.println("✗ Failed to connect to TWS: " <> inspect_error(err))
      io.println("")
      io.println("Troubleshooting:")
      io.println("  1. Make sure TWS or IB Gateway is running")
      io.println("  2. Check that the port is correct (7497 for paper, 7496 for live)")
      io.println("  3. Verify that API connections are enabled in TWS settings")
      io.println("  4. Check that 'ActiveX/Socket Clients' is enabled in TWS")
      io.println("  5. Ensure no other client is using the same client ID")
    }
  }

  io.println("")
  io.println("=")
  io.println("Test Complete")
  io.println("=")
}

fn test_account_summary(client: ib_tws_api.Client) {
  io.println("Test: Account Summary")
  io.println("-")

  io.println("  Sending AccountSummaryRequest...")
  let account_summary_req =
    protocol.AccountSummaryRequest(1, "All", [
      "TotalCashBalance",
      "NetLiquidation",
      "AvailableFunds",
      "GrossPositionValue",
      "BuyingPower",
    ])

  case ib_tws_api.send_message(client, account_summary_req) {
    Ok(_) -> {
      io.println("  ✓ AccountSummaryRequest sent successfully")

      io.println("  Waiting for responses (5 seconds)...")
      let messages = receive_messages(client, 5000, 10)

      case list.length(messages) {
        0 -> io.println("  ✗ No messages received")
        _ -> {
          io.println("  ✓ Received " <> int.to_string(list.length(messages)) <> " messages")

          list.each(messages, fn(msg) {
            case msg {
              protocol.AccountSummary(account, tag, value, currency) -> {
                io.println("    Account: " <> account)
                io.println("    Tag: " <> tag)
                io.println("    Value: " <> value)
                io.println("    Currency: " <> currency)
                io.println("")
              }
              _ -> io.println("    (Other message type)")
            }
          })
        }
      }
    }
    Error(err) -> {
      io.println("  ✗ Error sending AccountSummaryRequest: " <> inspect_error(err))
    }
  }

  io.println("")
}

fn test_open_orders(client: ib_tws_api.Client) {
  io.println("Test: Open Orders")
  io.println("-")

  io.println("  Note: OpenOrder messages are sent automatically by TWS upon connection")
  io.println("  Checking for any OpenOrder messages in the queue...")

  let messages = receive_messages(client, 1000, 5)

  let open_orders =
    list.filter(messages, fn(msg) {
      case msg {
        protocol.OpenOrder(_) -> True
        _ -> False
      }
    })

  case list.length(open_orders) {
    0 -> io.println("  ✓ No open orders (as expected for test account)")
    _ -> {
      io.println("  ✓ Found " <> int.to_string(list.length(open_orders)) <> " open orders")

      list.each(open_orders, fn(msg) {
        case msg {
          protocol.OpenOrder(order) -> {
            io.println("    Order ID: " <> int.to_string(order.order_id))
            io.println("    Action: " <> order.action)
            io.println("    Quantity: " <> float.to_string(order.total_quantity))
            io.println("    Order Type: " <> order.order_type)
            io.println("")
          }
          _ -> io.println("    (Other message type)")
        }
      })
    }
  }

  io.println("")
}

fn test_positions(client: ib_tws_api.Client) {
  io.println("Test: Positions")
  io.println("-")

  io.println("  Sending PositionsRequest...")
  case ib_tws_api.send_message(client, protocol.PositionsRequest) {
    Ok(_) -> {
      io.println("  ✓ PositionsRequest sent successfully")

      io.println("  Waiting for responses (5 seconds)...")
      let messages = receive_messages(client, 5000, 10)

      let positions =
        list.filter(messages, fn(msg) {
          case msg {
            protocol.Position(_, _, _, _) -> True
            _ -> False
          }
        })

      case list.length(positions) {
        0 -> io.println("  ✓ No positions (as expected for test account)")
        _ -> {
          io.println("  ✓ Found " <> int.to_string(list.length(positions)) <> " positions")

          list.each(positions, fn(msg) {
            case msg {
              protocol.Position(account, contract, position, avg_cost) -> {
                io.println("    Account: " <> account)
                io.println("    Symbol: " <> contract.symbol)
                io.println("    Position: " <> float.to_string(position))
                io.println("    Avg Cost: " <> float.to_string(avg_cost))
                io.println("")
              }
              _ -> io.println("    (Other message type)")
            }
          })
        }
      }
    }
    Error(err) -> {
      io.println("  ✗ Error sending PositionsRequest: " <> inspect_error(err))
    }
  }

  io.println("")
}

fn test_market_data(client: ib_tws_api.Client) {
  io.println("Test: Market Data")
  io.println("-")

  io.println("  Sending MarketDataRequest for AAPL...")
  let contract =
    types.Contract(
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

  let market_data_req = protocol.MarketDataRequest(100, contract)

  case ib_tws_api.send_message(client, market_data_req) {
    Ok(_) -> {
      io.println("  ✓ MarketDataRequest sent successfully")

      io.println("  Waiting for tick updates (10 seconds)...")
      let messages = receive_messages(client, 10000, 20)

      let ticks =
        list.filter(messages, fn(msg) {
          case msg {
            protocol.MarketDataTick(_, _, _) -> True
            _ -> False
          }
        })

      case list.length(ticks) {
        0 -> io.println("  ✗ No tick data received (market may be closed)")
        _ -> {
          io.println("  ✓ Received " <> int.to_string(list.length(ticks)) <> " tick updates")

          list.each(list.take(ticks, 5), fn(msg) {
            case msg {
              protocol.MarketDataTick(ticker_id, tick_type, value) -> {
                io.println(
                  "    Ticker ID: "
                  <> int.to_string(ticker_id)
                  <> ", Type: "
                  <> int.to_string(tick_type)
                  <> ", Value: "
                  <> float.to_string(value),
                )
              }
              _ -> io.println("    (Other message type)")
            }
          })
        }
      }

      io.println("  Canceling market data subscription...")
      case ib_tws_api.send_message(client, protocol.CancelMarketData(100)) {
        Ok(_) -> io.println("  ✓ Market data subscription canceled")
        Error(err) -> io.println("  ✗ Error canceling: " <> inspect_error(err))
      }
    }
    Error(err) -> {
      io.println("  ✗ Error sending MarketDataRequest: " <> inspect_error(err))
    }
  }

  io.println("")
}

fn test_realtime_bars(client: ib_tws_api.Client) {
  io.println("Test: Real-Time Bars")
  io.println("-")

  io.println("  Sending RealTimeBarsRequest for AAPL...")
  let contract =
    types.Contract(
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

  let realtime_bars_req =
    protocol.RealTimeBarsRequest(
      req_id: 200,
      contract: contract,
      bar_size: 5,
      what_to_show: "TRADES",
      use_rth: True,
    )

  case ib_tws_api.send_message(client, realtime_bars_req) {
    Ok(_) -> {
      io.println("  ✓ RealTimeBarsRequest sent successfully")

      io.println("  Waiting for bar updates (15 seconds)...")
      let messages = receive_messages(client, 15000, 20)

      let bars =
        list.filter(messages, fn(msg) {
          case msg {
            protocol.RealTimeBar(_) -> True
            _ -> False
          }
        })

      case list.length(bars) {
        0 -> io.println("  ✗ No bar data received (market may be closed)")
        _ -> {
          io.println("  ✓ Received " <> int.to_string(list.length(bars)) <> " bar updates")

          list.each(list.take(bars, 3), fn(msg) {
            case msg {
              protocol.RealTimeBar(bar) -> {
                io.println("    Time: " <> int.to_string(bar.time))
                io.println("    Open: " <> float.to_string(bar.open))
                io.println("    High: " <> float.to_string(bar.high))
                io.println("    Low: " <> float.to_string(bar.low))
                io.println("    Close: " <> float.to_string(bar.close))
                io.println("    Volume: " <> int.to_string(bar.volume))
                io.println("")
              }
              _ -> io.println("    (Other message type)")
            }
          })
        }
      }

      io.println("  Canceling real-time bars subscription...")
      case ib_tws_api.send_message(client, protocol.CancelRealTimeBars(200)) {
        Ok(_) -> io.println("  ✓ Real-time bars subscription canceled")
        Error(err) -> io.println("  ✗ Error canceling: " <> inspect_error(err))
      }
    }
    Error(err) -> {
      io.println("  ✗ Error sending RealTimeBarsRequest: " <> inspect_error(err))
    }
  }

  io.println("")
}

fn receive_messages(
  client: ib_tws_api.Client,
  timeout_ms: Int,
  max_messages: Int,
) -> List(protocol.Message) {
  receive_messages_loop(client, timeout_ms, max_messages, [])
}

fn receive_messages_loop(
  client: ib_tws_api.Client,
  timeout_ms: Int,
  max_messages: Int,
  accumulator: List(protocol.Message),
) -> List(protocol.Message) {
  case list.length(accumulator) >= max_messages {
    True -> list.reverse(accumulator)
    False -> {
      case ib_tws_api.receive_message(client, timeout_ms) {
        Ok(msg) -> {
          let new_accumulator = [msg, ..accumulator]
          receive_messages_loop(client, 1000, max_messages, new_accumulator)
        }
        Error(_) -> list.reverse(accumulator)
      }
    }
  }
}

fn inspect_error(err: ClientError) -> String {
  case err {
    ConnectionFailed(msg) -> "ConnectionFailed: " <> msg
    AuthenticationFailed(msg) -> "AuthenticationFailed: " <> msg
    SocketError(msg) -> "SocketError: " <> msg
  }
}
