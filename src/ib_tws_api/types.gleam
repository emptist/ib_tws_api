import gleam/option
pub type Contract {
  Contract(
    contract_id: Int,
    symbol: String,
    security_type: String,
    exchange: String,
    currency: String,
    last_trade_date_or_contract_month: String,
    strike: Float,
    right: String,
    multiplier: String,
    primary_exchange: String,
  )
}

pub type Order {
  Order(
    order_id: Int,
    client_id: Int,
    order_type: String,
    action: String,
    total_quantity: Float,
    limit_price: option.Option(Float),
    stop_price: option.Option(Float),
    time_in_force: String,
    oca_group: String,
    account: String,
    outside_rth: Bool,
    hidden: Bool,
    display_size: Int,
    trail_stop_price: option.Option(Float),
    parent_id: Int,
  )
}

pub type OrderState {
  OrderState(
    status: String,
    init_margin_before: String,
    maint_margin_before: String,
    equity_with_loan_before: String,
    init_margin_change: String,
    maint_margin_change: String,
    equity_with_loan_change: String,
    init_margin_after: String,
    maint_margin_after: String,
    equity_with_loan_after: String,
    commission: Float,
    min_commission: Float,
    max_commission: Float,
    commission_currency: String,
    warning_text: String,
  )
}

pub type Execution {
  Execution(
    order_id: Int,
    client_id: Int,
    exec_id: String,
    time: String,
    acct_number: String,
    exchange: String,
    side: String,
    shares: Float,
    price: Float,
    perm_id: Int,
    client_order_id: Int,
    liquidation: Int,
    cum_qty: Float,
    avg_price: Float,
    order_ref: String,
    ev_rule: String,
    ev_multiplier: Float,
    model_code: String,
    last_liquidity: Int,
  )
}

pub type Tick {
  Tick(
    ticker_id: Int,
    field: Int,
    price: option.Option(Float),
    size: option.Option(Int),
    can_auto_execute: Bool,
  )
}

pub type MarketDepth {
  MarketDepth(
    req_id: Int,
    position: Int,
    operation: Int,
    side: Int,
    price: Float,
    size: Int,
  )
}

pub type ScannerSubscription {
  ScannerSubscription(
    number_of_rows: Int,
    instrument: String,
    location_code: String,
    scan_code: String,
    above_price: Float,
    below_price: Float,
    above_volume: Int,
    market_cap_above: Float,
    market_cap_below: Float,
    moody_rating_above: String,
    moody_rating_below: String,
    sp_rating_above: String,
    sp_rating_below: String,
    maturity_date_above: String,
    maturity_date_below: String,
    coupon_rate_above: Float,
    coupon_rate_below: Float,
    exclude_convertible: Bool,
    average_option_volume_above: Int,
    issuer_name: String,
    currency: String,
    sec_type: String,
  )
}

pub type HistoricalData {
  HistoricalData(
    req_id: Int,
    date: String,
    open: Float,
    high: Float,
    low: Float,
    close: Float,
    volume: Int,
    count: Int,
    wap: Float,
  )
}

pub type Bar {
  Bar(
    time: String,
    open: Float,
    high: Float,
    low: Float,
    close: Float,
    volume: Int,
    count: Int,
    wap: Float,
  )
}

pub type RealTimeBar {
  RealTimeBar(
    req_id: Int,
    time: Int,
    open: Float,
    high: Float,
    low: Float,
    close: Float,
    volume: Int,
    wap: Float,
    count: Int,
  )
}

pub type FundamentalType {
  ReportsFinSummary
  ReportsOwnership
  ReportSnapshot
  ReportsFinStatements
  REIT
  CalendarReport
}

pub type NewsBulletin {
  NewsBulletin(
    msg_id: Int,
    msg_type: Int,
    news_message: String,
    origin_exch: String,
  )
}

pub type AccountSummary {
  AccountSummary(
    req_id: Int,
    account: String,
    tag: String,
    value: String,
    currency: String,
  )
}

pub type Position {
  Position(
    account: String,
    contract: Contract,
    position: Float,
    avg_cost: Float,
  )
}

pub type PortfolioValue {
  PortfolioValue(
    contract: Contract,
    position: Float,
    market_price: Float,
    market_value: Float,
    average_cost: Float,
    unrealized_pnl: Float,
    realized_pnl: Float,
    account_name: String,
  )
}
