import connection
import gleam/float
import gleam/int
import gleam/io
import gleam/string

/// Order action - type-safe buy/sell
pub type OrderAction {
  /// Buy action
  BuyAction
  /// Sell action
  SellAction
  /// Short action
  ShortAction
}

/// Order time in force
pub type TimeInForce {
  /// Day order - valid for day only
  Day
  /// Good Till Cancelled
  GTC
  /// Immediate or Cancel
  IOC
  /// All or None
  AON
}

/// Market order - executes immediately at current market price
/// Type-safe: No price fields needed (market doesn't need prices)
pub type MarketOrder {
  MarketOrder(
    order_id: Int,
    action: OrderAction,
    quantity: Int,
    time_in_force: TimeInForce,
  )
}

/// Limit order - executes at specified price or better
/// Type-safe: Must have limit_price, stop_price is always 0.0
pub type LimitOrder {
  LimitOrder(
    order_id: Int,
    action: OrderAction,
    quantity: Int,
    limit_price: Float,
    time_in_force: TimeInForce,
  )
}

/// Stop order - becomes market order when stop price is reached
/// Type-safe: Must have stop_price, limit_price is always 0.0
pub type StopOrder {
  StopOrder(
    order_id: Int,
    action: OrderAction,
    quantity: Int,
    stop_price: Float,
    time_in_force: TimeInForce,
  )
}

/// Stop-Limit order - becomes limit order when stop price is reached
/// Type-safe: Must have both stop_price and limit_price
pub type StopLimitOrder {
  StopLimitOrder(
    order_id: Int,
    action: OrderAction,
    quantity: Int,
    stop_price: Float,
    limit_price: Float,
    time_in_force: TimeInForce,
  )
}

/// Trailing stop order - stop price trails market by specified amount
/// Type-safe: Must have trailing_amount or trailing_percent
pub type TrailingStopOrder {
  /// Trail by fixed dollar amount
  TrailingAmount(
    order_id: Int,
    action: OrderAction,
    quantity: Int,
    trailing_amount: Float,
    time_in_force: TimeInForce,
  )
  /// Trail by percentage
  TrailingPercent(
    order_id: Int,
    action: OrderAction,
    quantity: Int,
    trailing_percent: Float,
    time_in_force: TimeInForce,
  )
}

/// Unified order type - Gleam's type system ensures all fields are valid
pub type Order {
  Market(MarketOrder)
  Limit(LimitOrder)
  Stop(StopOrder)
  StopLimit(StopLimitOrder)
  TrailingStop(TrailingStopOrder)
}

/// Place an order (paper trading or live trading with correct account type)
/// Returns: message bytes to send to IB TWS
/// Uses Gleam's type system to guarantee all order fields are valid
pub fn place_order(
  account_type: connection.AccountType,
  contract_id: Int,
  order: Order,
) -> Result(BitArray, String) {
  // Safety check: Only allow trading with appropriate account types
  case connection.is_trading_allowed(account_type) {
    True -> {
      // Trading IS allowed (paper trading or live trading)
      // IB TWS API message code for placing orders is 5
      // Format: version, order_id, contract_id, action, order_type, quantity, limit_price, stop_price, time_in_force

      let #(
        order_id,
        action_str,
        order_type_str,
        quantity,
        limit_price,
        stop_price,
        tif_str,
      ) = case order {
        Market(mkt) -> {
          let MarketOrder(
            order_id: order_id,
            action: action,
            quantity: quantity,
            time_in_force: tif,
          ) = mkt
          #(
            order_id,
            order_action_to_string(action),
            "MKT",
            quantity,
            0.0,
            0.0,
            time_in_force_to_string(tif),
          )
        }
        Limit(lmt) -> {
          let LimitOrder(
            order_id: order_id,
            action: action,
            quantity: quantity,
            limit_price: limit_price,
            time_in_force: tif,
          ) = lmt
          #(
            order_id,
            order_action_to_string(action),
            "LMT",
            quantity,
            limit_price,
            0.0,
            time_in_force_to_string(tif),
          )
        }
        Stop(stp) -> {
          let StopOrder(
            order_id: order_id,
            action: action,
            quantity: quantity,
            stop_price: stop_price,
            time_in_force: tif,
          ) = stp
          #(
            order_id,
            order_action_to_string(action),
            "STP",
            quantity,
            0.0,
            stop_price,
            time_in_force_to_string(tif),
          )
        }
        StopLimit(stp_lmt) -> {
          let StopLimitOrder(
            order_id: order_id,
            action: action,
            quantity: quantity,
            stop_price: stop_price,
            limit_price: limit_price,
            time_in_force: tif,
          ) = stp_lmt
          #(
            order_id,
            order_action_to_string(action),
            "STP LMT",
            quantity,
            limit_price,
            stop_price,
            time_in_force_to_string(tif),
          )
        }
        TrailingStop(trail) -> {
          case trail {
            TrailingAmount(
              order_id: order_id,
              action: action,
              quantity: quantity,
              trailing_amount: trail_amt,
              time_in_force: tif,
            ) -> {
              // For trailing stop, use trail_amt as stop_price
              #(
                order_id,
                order_action_to_string(action),
                "TRAIL",
                quantity,
                0.0,
                trail_amt,
                time_in_force_to_string(tif),
              )
            }
            TrailingPercent(
              order_id: order_id,
              action: action,
              quantity: quantity,
              trailing_percent: trail_pct,
              time_in_force: tif,
            ) -> {
              // For trailing percent, use trail_pct as stop_price
              #(
                order_id,
                order_action_to_string(action),
                "TRAIL",
                quantity,
                0.0,
                trail_pct,
                time_in_force_to_string(tif),
              )
            }
          }
        }
      }

      let action_len = string.length(action_str)
      let order_type_len = string.length(order_type_str)
      let tif_len = string.length(tif_str)

      Ok(<<
        5:16,
        45:32,
        order_id:32,
        contract_id:32,
        action_len:8,
        action_str:utf8,
        order_type_len:8,
        order_type_str:utf8,
        quantity:32,
        limit_price:64-float,
        stop_price:64-float,
        tif_len:8,
        tif_str:utf8,
      >>)
    }
    False -> {
      // Trading is NOT allowed (LiveTradingReadOnly)
      Error(
        "Trading is not allowed with this account type. "
        <> "Please use PaperTrading or LiveTrading account type.",
      )
    }
  }
}

/// Cancel an order
/// Returns: message bytes to send to IB TWS
pub fn cancel_order(order_id: Int) -> BitArray {
  // IB TWS API message code for canceling orders is 4
  // Format: version, order_id
  <<4:16, 1:32, order_id:32>>
}

/// Create a market order - executes immediately at current market price
/// Type-safe: No price parameters needed (market orders don't use prices)
pub fn create_market_order(
  order_id: Int,
  action: OrderAction,
  quantity: Int,
) -> Order {
  Market(MarketOrder(
    order_id: order_id,
    action: action,
    quantity: quantity,
    time_in_force: Day,
  ))
}

/// Create a limit order - executes at specified price or better
/// Type-safe: Must provide limit_price (stop_price is not needed)
pub fn create_limit_order(
  order_id: Int,
  action: OrderAction,
  quantity: Int,
  limit_price: Float,
) -> Order {
  Limit(LimitOrder(
    order_id: order_id,
    action: action,
    quantity: quantity,
    limit_price: limit_price,
    time_in_force: Day,
  ))
}

/// Create a stop order - becomes market order when stop price is reached
/// Type-safe: Must provide stop_price (limit_price is not needed)
pub fn create_stop_order(
  order_id: Int,
  action: OrderAction,
  quantity: Int,
  stop_price: Float,
) -> Order {
  Stop(StopOrder(
    order_id: order_id,
    action: action,
    quantity: quantity,
    stop_price: stop_price,
    time_in_force: Day,
  ))
}

/// Create a stop-limit order - becomes limit order when stop price is reached
/// Type-safe: Must provide both stop_price and limit_price
pub fn create_stop_limit_order(
  order_id: Int,
  action: OrderAction,
  quantity: Int,
  stop_price: Float,
  limit_price: Float,
) -> Order {
  StopLimit(StopLimitOrder(
    order_id: order_id,
    action: action,
    quantity: quantity,
    stop_price: stop_price,
    limit_price: limit_price,
    time_in_force: Day,
  ))
}

/// Create a trailing stop order by dollar amount
/// Type-safe: Must provide trailing_amount (stop price trails market by this amount)
pub fn create_trailing_stop_amount(
  order_id: Int,
  action: OrderAction,
  quantity: Int,
  trailing_amount: Float,
) -> Order {
  TrailingStop(TrailingAmount(
    order_id: order_id,
    action: action,
    quantity: quantity,
    trailing_amount: trailing_amount,
    time_in_force: Day,
  ))
}

/// Create a trailing stop order by percentage
/// Type-safe: Must provide trailing_percent (stop price trails market by this %)
pub fn create_trailing_stop_percent(
  order_id: Int,
  action: OrderAction,
  quantity: Int,
  trailing_percent: Float,
) -> Order {
  TrailingStop(TrailingPercent(
    order_id: order_id,
    action: action,
    quantity: quantity,
    trailing_percent: trailing_percent,
    time_in_force: Day,
  ))
}

/// Debug print order details - uses pattern matching for type safety
pub fn debug_order(order: Order) {
  case order {
    Market(mkt) -> {
      io.println("=== Market Order ===")
      io.println("  Order ID: " <> int.to_string(mkt.order_id))
      io.println("  Action: " <> order_action_to_string(mkt.action))
      io.println("  Type: MARKET")
      io.println("  Quantity: " <> int.to_string(mkt.quantity))
      io.println(
        "  Time in Force: " <> time_in_force_to_string(mkt.time_in_force),
      )
    }
    Limit(lmt) -> {
      io.println("=== Limit Order ===")
      io.println("  Order ID: " <> int.to_string(lmt.order_id))
      io.println("  Action: " <> order_action_to_string(lmt.action))
      io.println("  Type: LIMIT")
      io.println("  Quantity: " <> int.to_string(lmt.quantity))
      io.println("  Limit Price: " <> float.to_string(lmt.limit_price))
      io.println(
        "  Time in Force: " <> time_in_force_to_string(lmt.time_in_force),
      )
    }
    Stop(stp) -> {
      io.println("=== Stop Order ===")
      io.println("  Order ID: " <> int.to_string(stp.order_id))
      io.println("  Action: " <> order_action_to_string(stp.action))
      io.println("  Type: STOP")
      io.println("  Quantity: " <> int.to_string(stp.quantity))
      io.println("  Stop Price: " <> float.to_string(stp.stop_price))
      io.println(
        "  Time in Force: " <> time_in_force_to_string(stp.time_in_force),
      )
    }
    StopLimit(stp_lmt) -> {
      io.println("=== Stop-Limit Order ===")
      io.println("  Order ID: " <> int.to_string(stp_lmt.order_id))
      io.println("  Action: " <> order_action_to_string(stp_lmt.action))
      io.println("  Type: STOP LIMIT")
      io.println("  Quantity: " <> int.to_string(stp_lmt.quantity))
      io.println("  Stop Price: " <> float.to_string(stp_lmt.stop_price))
      io.println("  Limit Price: " <> float.to_string(stp_lmt.limit_price))
      io.println(
        "  Time in Force: " <> time_in_force_to_string(stp_lmt.time_in_force),
      )
    }
    TrailingStop(trail) -> {
      case trail {
        TrailingAmount(
          order_id: order_id,
          action: action,
          quantity: quantity,
          trailing_amount: amt,
          time_in_force: tif,
        ) -> {
          io.println("=== Trailing Stop Order (Amount) ===")
          io.println("  Order ID: " <> int.to_string(order_id))
          io.println("  Action: " <> order_action_to_string(action))
          io.println("  Type: TRAILING STOP")
          io.println("  Quantity: " <> int.to_string(quantity))
          io.println("  Trailing Amount: $" <> float.to_string(amt))
          io.println("  Time in Force: " <> time_in_force_to_string(tif))
        }
        TrailingPercent(
          order_id: order_id,
          action: action,
          quantity: quantity,
          trailing_percent: pct,
          time_in_force: tif,
        ) -> {
          io.println("=== Trailing Stop Order (Percent) ===")
          io.println("  Order ID: " <> int.to_string(order_id))
          io.println("  Action: " <> order_action_to_string(action))
          io.println("  Type: TRAILING STOP")
          io.println("  Quantity: " <> int.to_string(quantity))
          io.println("  Trailing Percent: " <> float.to_string(pct) <> "%")
          io.println("  Time in Force: " <> time_in_force_to_string(tif))
        }
      }
    }
  }
}

/// Get order ID from any order type - demonstrates Gleam's pattern matching
pub fn get_order_id(order: Order) -> Int {
  case order {
    Market(mkt) -> mkt.order_id
    Limit(lmt) -> lmt.order_id
    Stop(stp) -> stp.order_id
    StopLimit(stp_lmt) -> stp_lmt.order_id
    TrailingStop(trail) -> {
      case trail {
        TrailingAmount(order_id: order_id, ..) -> order_id
        TrailingPercent(order_id: order_id, ..) -> order_id
      }
    }
  }
}

/// Get order action from any order type
pub fn get_order_action(order: Order) -> OrderAction {
  case order {
    Market(mkt) -> mkt.action
    Limit(lmt) -> lmt.action
    Stop(stp) -> stp.action
    StopLimit(stp_lmt) -> stp_lmt.action
    TrailingStop(trail) -> {
      case trail {
        TrailingAmount(action: action, ..) -> action
        TrailingPercent(action: action, ..) -> action
      }
    }
  }
}

/// Convert OrderAction to string for IB TWS API
fn order_action_to_string(action: OrderAction) -> String {
  case action {
    BuyAction -> "BUY"
    SellAction -> "SELL"
    ShortAction -> "SSHORT"
  }
}

/// Convert TimeInForce to string for IB TWS API
fn time_in_force_to_string(tif: TimeInForce) -> String {
  case tif {
    Day -> "DAY"
    GTC -> "GTC"
    IOC -> "IOC"
    AON -> "AON"
  }
}
