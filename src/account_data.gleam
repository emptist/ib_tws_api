import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/string

/// Account data request types for IB TWS API
/// This module provides functions to request positions and account summaries
/// Request all current positions from IB TWS
/// Message code: 13 (REQ_POSITIONS)
pub fn request_positions() -> BitArray {
  // REQ_POSITIONS message format:
  // Message code: 13 (2 bytes, big-endian)
  // Version: 1 (4 bytes)
  <<13:16, 1:32>>
}

/// Cancel position updates
/// Message code: 14 (CANCEL_POSITIONS)
pub fn cancel_positions() -> BitArray {
  // CANCEL_POSITIONS message format:
  // Message code: 14 (2 bytes, big-endian)
  <<14:16>>
}

/// Account summary tags - specify which data to retrieve
pub type AccountSummaryTag {
  /// Account type
  AccountTypeTag
  /// Net liquidation value
  NetLiquidation
  /// Total cash balance
  TotalCashBalance
  /// Settled cash
  SettledCash
  /// Accrued cash
  AccruedCash
  /// Buying power
  BuyingPower
  /// Equity with loan value
  EquityWithLoan
  /// Previous day equity with loan
  PreviousEquityWithLoan
  /// Gross position value
  GrossPosition
  /// RegT margin
  RegTMargin
  /// SMA (Special Memorandum Account)
  SMA
  /// Init margin requirement
  InitMarginReq
  /// Maint margin requirement
  MaintMarginReq
  /// Available funds
  AvailableFunds
  /// Excess liquidity
  ExcessLiquidity
  /// Cushion
  Cushion
  /// Full available funds
  FullAvailableFunds
  /// Full excess liquidity
  FullExcessLiquidity
  /// Look ahead next change
  LookAheadNextChange
  /// Look ahead init margin req
  LookAheadInitMarginReq
  /// Look ahead maint margin req
  LookAheadMaintMarginReq
  /// Look ahead available funds
  LookAheadAvailableFunds
  /// Look ahead excess liquidity
  LookAheadExcessLiquidity
  /// Highest severity
  HighestSeverity
  /// Day trades remaining
  DayTradesRemaining
  /// Leverage
  Leverage
}

/// Get the string representation of an account summary tag
pub fn account_summary_tag_to_string(tag: AccountSummaryTag) -> String {
  case tag {
    AccountTypeTag -> "AccountType"
    NetLiquidation -> "NetLiquidation"
    TotalCashBalance -> "TotalCashBalance"
    SettledCash -> "SettledCash"
    AccruedCash -> "AccruedCash"
    BuyingPower -> "BuyingPower"
    EquityWithLoan -> "EquityWithLoan"
    PreviousEquityWithLoan -> "PreviousEquityWithLoan"
    GrossPosition -> "GrossPosition"
    RegTMargin -> "RegTMargin"
    SMA -> "SMA"
    InitMarginReq -> "InitMarginReq"
    MaintMarginReq -> "MaintMarginReq"
    AvailableFunds -> "AvailableFunds"
    ExcessLiquidity -> "ExcessLiquidity"
    Cushion -> "Cushion"
    FullAvailableFunds -> "FullAvailableFunds"
    FullExcessLiquidity -> "FullExcessLiquidity"
    LookAheadNextChange -> "LookAheadNextChange"
    LookAheadInitMarginReq -> "LookAheadInitMarginReq"
    LookAheadMaintMarginReq -> "LookAheadMaintMarginReq"
    LookAheadAvailableFunds -> "LookAheadAvailableFunds"
    LookAheadExcessLiquidity -> "LookAheadExcessLiquidity"
    HighestSeverity -> "HighestSeverity"
    DayTradesRemaining -> "DayTradesRemaining"
    Leverage -> "Leverage"
  }
}

/// Request account summary with specified tags
/// Message code: 6 (REQ_ACCOUNT_SUMMARY)
/// 
/// Parameters:
/// - req_id: Request ID to identify this request (must be unique)
/// - group_name: Account group name (e.g., "All")
/// - tags: List of tags to request
pub fn request_account_summary(
  req_id: Int,
  group_name: String,
  tags: List(AccountSummaryTag),
) -> BitArray {
  // Convert tags to comma-separated string
  let tags_string =
    tags
    |> list.map(account_summary_tag_to_string)
    |> string.join(",")

  let group_len = string.length(group_name)
  let tags_len = string.length(tags_string)

  // REQ_ACCOUNT_SUMMARY message format:
  // Message code: 6 (2 bytes, big-endian)
  // Version: 1 (4 bytes)
  // Request ID: (4 bytes)
  // Group name length: (1 byte)
  // Group name: (variable)
  // Tags length: (1 byte)
  // Tags: (variable)
  <<
    6:16,
    1:32,
    req_id:32,
    group_len:8,
    group_name:utf8,
    tags_len:8,
    tags_string:utf8,
  >>
}

/// Cancel account summary request
/// Message code: 7 (CANCEL_ACCOUNT_SUMMARY)
pub fn cancel_account_summary(req_id: Int) -> BitArray {
  // CANCEL_ACCOUNT_SUMMARY message format:
  // Message code: 7 (2 bytes, big-endian)
  // Request ID: (4 bytes)
  <<7:16, req_id:32>>
}

/// Create a list of common account summary tags for comprehensive account data
pub fn common_account_tags() -> List(AccountSummaryTag) {
  [
    AccountTypeTag,
    NetLiquidation,
    TotalCashBalance,
    SettledCash,
    BuyingPower,
    EquityWithLoan,
    GrossPosition,
    RegTMargin,
    SMA,
    InitMarginReq,
    MaintMarginReq,
    AvailableFunds,
    ExcessLiquidity,
    DayTradesRemaining,
    Leverage,
  ]
}

/// Debug: Print position request message details
pub fn debug_position_request() {
  let msg = request_positions()
  let size = bit_array.byte_size(msg)
  io.println("=== Position Request Message ===")
  io.println("Message size: " <> int.to_string(size) <> " bytes")
  io.println("Message code: 13 (REQ_POSITIONS)")
  io.println("")
}

/// Debug: Print account summary request message details
pub fn debug_account_summary_request(
  req_id: Int,
  group_name: String,
  tags: List(AccountSummaryTag),
) {
  let msg = request_account_summary(req_id, group_name, tags)
  let size = bit_array.byte_size(msg)
  let tags_string =
    tags
    |> list.map(account_summary_tag_to_string)
    |> string.join(",")

  io.println("=== Account Summary Request Message ===")
  io.println("Message size: " <> int.to_string(size) <> " bytes")
  io.println("Request ID: " <> int.to_string(req_id))
  io.println("Group name: " <> group_name)
  io.println("Tags: " <> tags_string)
  io.println("")
}
