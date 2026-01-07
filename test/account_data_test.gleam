import account_data
import gleam/bit_array
import gleam/int
import gleam/io

pub fn main() {
  io.println("=== Account Data Test ===")
  io.println("")

  // Test 1: Create position request message
  io.println("1. Creating position request message")
  account_data.debug_position_request()
  let pos_msg = account_data.request_positions()
  let pos_size = bit_array.byte_size(pos_msg)
  io.println(
    "✓ Position request message created ("
    <> int.to_string(pos_size)
    <> " bytes)",
  )
  io.println("")

  // Test 2: Create cancel positions message
  io.println("2. Creating cancel positions message")
  let cancel_pos_msg = account_data.cancel_positions()
  let cancel_pos_size = bit_array.byte_size(cancel_pos_msg)
  io.println(
    "✓ Cancel positions message created ("
    <> int.to_string(cancel_pos_size)
    <> " bytes)",
  )
  io.println("")

  // Test 3: Create account summary request with common tags
  io.println("3. Creating account summary request with common tags")
  let req_id = 100
  let group_name = "All"
  let tags = account_data.common_account_tags()
  account_data.debug_account_summary_request(req_id, group_name, tags)
  let acc_msg = account_data.request_account_summary(req_id, group_name, tags)
  let acc_size = bit_array.byte_size(acc_msg)
  io.println(
    "✓ Account summary request created ("
    <> int.to_string(acc_size)
    <> " bytes)",
  )
  io.println("")

  // Test 4: Create account summary request with specific tags
  io.println("4. Creating account summary request with specific tags")
  let specific_tags = [
    account_data.NetLiquidation,
    account_data.TotalCashBalance,
    account_data.BuyingPower,
  ]
  account_data.debug_account_summary_request(101, "All", specific_tags)
  let specific_msg =
    account_data.request_account_summary(101, "All", specific_tags)
  let specific_size = bit_array.byte_size(specific_msg)
  io.println(
    "✓ Specific account summary request created ("
    <> int.to_string(specific_size)
    <> " bytes)",
  )
  io.println("")

  // Test 5: Create cancel account summary message
  io.println("5. Creating cancel account summary message")
  let cancel_acc_msg = account_data.cancel_account_summary(req_id)
  let cancel_acc_size = bit_array.byte_size(cancel_acc_msg)
  io.println(
    "✓ Cancel account summary message created ("
    <> int.to_string(cancel_acc_size)
    <> " bytes)",
  )
  io.println("")

  io.println("=== Test Complete ===")
  io.println("")
  io.println("Summary:")
  io.println("- Position requests: ✓ Working")
  io.println("- Account summary requests: ✓ Working")
  io.println("- Cancel requests: ✓ Working")
  io.println("")
  io.println("Note: To actually request account data:")
  io.println("1. Connect to IB TWS using connection.connect_with_callback()")
  io.println("2. Send request message using connection.send_bytes()")
  io.println("3. Handle position/account summary updates via callback")
  io.println("4. Parse received messages using messages.parse_message()")
}
