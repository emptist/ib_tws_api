import connection
import gleam/float
import gleam/int
import gleam/io
import gleam/string

/// Order type for IB TWS API
pub type OrderType {
  MarketOrder
  LimitOrder
  StopOrder
  StopLimitOrder
}

/// Order side (buy or sell)
pub type OrderSide {
  Buy
  Sell
}

/// Order action
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
  /// Day order - valid for the day only
  Day
  /// Good Till Cancelled
  GTC
  /// Immediate or Cancel
  IOC
  /// All or None
  AON
}

/// Order for IB TWS API
pub type Order {
  Order(
    order_id: Int,
    action: OrderAction,
    order_type: OrderType,
    quantity: Int,
    limit_price: Float,
    stop_price: Float,
    time_in_force: TimeInForce,
  )
}

/// Place an order (paper trading only)
/// Returns: message bytes to send to IB TWS
/// This function will only work with paper trading accounts
pub fn place_order(
  account_type: connection.AccountType,
  order_id: Int,
  contract_id: Int,
  order: Order,
) -> Result(BitArray, String) {
  // Safety check: Only allow trading with paper trading accounts
  case connection.is_trading_allowed(account_type) {
    True -> {
      // Trading IS allowed (paper trading account)
      // IB TWS API message code for placing orders is 5
      // Format: version, order_id, contract_id, action, order_type, quantity, limit_price, stop_price, time_in_force

      let action_str = order_action_to_string(order.action)
      let order_type_str = order_type_to_string(order.order_type)
      let tif_str = time_in_force_to_string(order.time_in_force)

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
        order.quantity:32,
        order.limit_price:64-float,
        order.stop_price:64-float,
        tif_len:8,
        tif_str:utf8,
      >>)
    }
    False -> {
      // Trading is NOT allowed (live trading account)
      Error(
        "Trading is not allowed with this account type. "
        <> "Please use paper trading account for safety.",
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

/// Create a market order
pub fn create_market_order(
  order_id: Int,
  action: OrderAction,
  quantity: Int,
) -> Order {
  Order(
    order_id: order_id,
    action: action,
    order_type: MarketOrder,
    quantity: quantity,
    limit_price: 0.0,
    stop_price: 0.0,
    time_in_force: Day,
  )
}

/// Create a limit order
pub fn create_limit_order(
  order_id: Int,
  action: OrderAction,
  quantity: Int,
  limit_price: Float,
) -> Order {
  Order(
    order_id: order_id,
    action: action,
    order_type: LimitOrder,
    quantity: quantity,
    limit_price: limit_price,
    stop_price: 0.0,
    time_in_force: Day,
  )
}

/// Debug print order details
pub fn debug_order(order: Order) {
  io.println("Order:")
  io.println("  Order ID: " <> int.to_string(order.order_id))
  io.println("  Action: " <> order_action_to_string(order.action))
  io.println("  Type: " <> order_type_to_string(order.order_type))
  io.println("  Quantity: " <> int.to_string(order.quantity))
  io.println("  Limit Price: " <> float.to_string(order.limit_price))
  io.println("  Stop Price: " <> float.to_string(order.stop_price))
  io.println(
    "  Time in Force: " <> time_in_force_to_string(order.time_in_force),
  )
}

/// Convert OrderAction to string for IB TWS API
fn order_action_to_string(action: OrderAction) -> String {
  case action {
    BuyAction -> "BUY"
    SellAction -> "SELL"
    ShortAction -> "SSHORT"
  }
}

/// Convert OrderType to string for IB TWS API
fn order_type_to_string(order_type: OrderType) -> String {
  case order_type {
    MarketOrder -> "MKT"
    LimitOrder -> "LMT"
    StopOrder -> "STP"
    StopLimitOrder -> "STP LMT"
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
