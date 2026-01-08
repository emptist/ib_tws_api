import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import message_encoder

/// Account data request types for IB TWS API
/// This module provides functions to request positions and account summaries
/// Request all current positions from IB TWS
/// Message code: 13 (REQ_POSITIONS)
pub fn request_positions(request_id: Int) -> String {
  // REQ_POSITIONS message format: "13\u{0000}1\u{0000}"
  // Message code: 13
  // Version: 1
  // Note: request_id parameter for future use, not used in current format

  let tokens = [
    int.to_string(13),
    // REQ_POSITIONS message ID
    int.to_string(1),
    // Version
  ]

  let result = string.join(tokens, "\u{0000}") <> "\u{0000}"
  io.println("[AccountData] REQ_POSITIONS: " <> result)
  result
}

/// Cancel position updates
/// Message code: 14 (CANCEL_POSITIONS)
pub fn cancel_positions() -> String {
  // CANCEL_POSITIONS message format: "14\u{0000}"
  // Message code: 14

  let tokens = [
    int.to_string(14),
    // CANCEL_POSITIONS message ID
  ]

  let result = string.join(tokens, "\u{0000}") <> "\u{0000}"
  io.println("[AccountData] CANCEL_POSITIONS: " <> result)
  result
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
) -> String {
  // Convert tags to comma-separated string
  let tags_string =
    tags
    |> list.map(account_summary_tag_to_string)
    |> string.join(",")

  // REQ_ACCOUNT_SUMMARY message format: "6\u{0000}1\u{0000}req_id\u{0000}group_name\u{0000}tags"
  // Message code: 6
  // Version: 1
  // Request ID
  // Group name
  // Tags (comma-separated)

  let tokens = [
    int.to_string(6),
    // REQ_ACCOUNT_SUMMARY message ID
    int.to_string(1),
    // Version
    int.to_string(req_id),
    group_name,
    tags_string,
  ]

  let result = string.join(tokens, "\u{0000}") <> "\u{0000}"
  io.println("[AccountData] REQ_ACCOUNT_SUMMARY: " <> result)
  result
}

/// Cancel account summary request
/// Message code: 7 (CANCEL_ACCOUNT_SUMMARY)
pub fn cancel_account_summary(req_id: Int) -> String {
  // CANCEL_ACCOUNT_SUMMARY message format: "7\u{0000}req_id"
  // Message code: 7
  // Request ID

  let tokens = [
    int.to_string(7),
    // CANCEL_ACCOUNT_SUMMARY message ID
    int.to_string(req_id),
  ]

  let result = string.join(tokens, "\u{0000}") <> "\u{0000}"
  io.println("[AccountData] CANCEL_ACCOUNT_SUMMARY: " <> result)
  result
}

/// Request managed accounts list
/// Message code: 15 (REQ_MANAGED_ACCTS)
/// Returns a comma-separated list of account IDs
pub fn request_managed_accounts() -> String {
  // REQ_MANAGED_ACCTS message format: "15\u{0000}1\u{0000}"
  // Message code: 15
  // Version: 1

  let tokens = [
    int.to_string(15),
    // REQ_MANAGED_ACCTS message ID
    int.to_string(1),
    // Version
  ]

  let result = string.join(tokens, "\u{0000}") <> "\u{0000}"
  io.println("[AccountData] REQ_MANAGED_ACCTS: " <> result)
  result
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
pub fn debug_position_request(request_id: Int) {
  let msg = request_positions(request_id)
  let size = string.length(msg)
  io.println("=== Position Request Message ===")
  io.println("Message size: " <> int.to_string(size) <> " bytes")
  io.println("Message code: 13 (REQ_POSITIONS)")
  io.println("Message: " <> msg)
  io.println("")
}

/// Debug: Print account summary request message details
pub fn debug_account_summary_request(
  req_id: Int,
  group_name: String,
  tags: List(AccountSummaryTag),
) {
  let msg = request_account_summary(req_id, group_name, tags)
  let size = string.length(msg)
  let tags_string =
    tags
    |> list.map(account_summary_tag_to_string)
    |> string.join(",")

  io.println("=== Account Summary Request Message ===")
  io.println("Message size: " <> int.to_string(size) <> " bytes")
  io.println("Request ID: " <> int.to_string(req_id))
  io.println("Group name: " <> group_name)
  io.println("Tags: " <> tags_string)
  io.println("Message: " <> msg)
  io.println("")
}
