import gleam/string

/// Order status types from IB TWS
pub type OrderStatus {
  PendingSubmit
  PendingCancel
  PreSubmitted
  Submitted
  ApiPending
  ApiCancelled
  Filled
  Cancelled
  Inactive
}

/// Open order data
pub type OpenOrder {
  OpenOrder(
    order_id: Int,
    contract_id: Int,
    action: String,
    quantity: Float,
    order_type: String,
    limit_price: Float,
    stop_price: Float,
    status: String,
    filled: Int,
    remaining: Int,
    avg_fill_price: Float,
  )
}

/// Request all open orders
/// Message code: 9 (REQ_OPEN_ORDERS)
pub fn request_open_orders() -> BitArray {
  // REQ_OPEN_ORDERS message format (version 1)
  <<9:16, 1:32>>
}

/// Cancel all open orders
/// Message code: 4 (CANCEL_ORDER) - but with order_id = 0 to cancel all
pub fn cancel_all_orders() -> BitArray {
  // CANCEL_ORDER with order_id = 0 cancels all orders
  <<4:16, 0:32>>
}

/// Cancel a specific order
/// Message code: 4 (CANCEL_ORDER)
pub fn cancel_order(order_id: Int) -> BitArray {
  // CANCEL_ORDER message format
  <<4:16, order_id:32>>
}

/// Modify an existing order
/// Message code: 26 (REQ_MKT_DATA) - using same structure as place order
/// Note: To modify, send a new order with the same order_id
pub fn modify_order(
  order_id: Int,
  contract_id: Int,
  action: String,
  quantity: Float,
  order_type: String,
  limit_price: Float,
  stop_price: Float,
) -> BitArray {
  let action_len = string.length(action)
  let order_type_len = string.length(order_type)

  // Place order message format (version 1) - same as placing new order
  // IB TWS treats modifying as placing an order with existing order_id
  <<
    5:16,
    1:32,
    order_id:32,
    contract_id:32,
    action_len:8,
    action:utf8,
    quantity:float,
    order_type_len:8,
    order_type:utf8,
    limit_price:float,
    stop_price:float,
  >>
}

/// Convert order status to string
pub fn order_status_to_string(status: OrderStatus) -> String {
  case status {
    PendingSubmit -> "PendingSubmit"
    PendingCancel -> "PendingCancel"
    PreSubmitted -> "PreSubmitted"
    Submitted -> "Submitted"
    ApiPending -> "ApiPending"
    ApiCancelled -> "ApiCancelled"
    Filled -> "Filled"
    Cancelled -> "Cancelled"
    Inactive -> "Inactive"
  }
}
