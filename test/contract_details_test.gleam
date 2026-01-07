import contract_details
import gleam/bit_array
import gleam/io
import gleam/list
import gleeunit
import gleeunit/should

// This is main test entry point
pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Test 1: Contract Specification Creation
// ============================================================================

pub fn contract_spec_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 1: Contract Specification Creation")
  io.println(repeat_char("=", 70))

  // Test 1.1: Create stock contract spec
  io.println("\n1.1: Creating stock contract specification")
  let stock_spec = contract_details.create_stock_contract_spec("AAPL")
  should.equal(stock_spec.symbol, "AAPL")
  should.equal(stock_spec.exchange, "SMART")
  should.equal(stock_spec.currency, "USD")
  io.println("✓ Stock contract spec created:")
  io.println("   Symbol: " <> stock_spec.symbol)
  io.println("   Exchange: " <> stock_spec.exchange)
  io.println("   Currency: " <> stock_spec.currency)

  // Test 1.2: Create custom contract spec
  io.println("\n1.2: Creating custom contract specification")
  let custom_spec =
    contract_details.create_contract_spec(
      contract_details.Future,
      "ES",
      "CME",
      "USD",
    )
  should.equal(custom_spec.symbol, "ES")
  should.equal(custom_spec.exchange, "CME")
  io.println("✓ Custom contract spec created:")
  io.println("   Symbol: " <> custom_spec.symbol)
  io.println("   Type: Future")
  io.println("   Exchange: " <> custom_spec.exchange)

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Contract specification creation - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 2: Contract Details Request Messages
// ============================================================================

pub fn contract_details_request_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 2: Contract Details Request Messages")
  io.println(repeat_char("=", 70))

  // Test 2.1: Create contract details request
  io.println("\n2.1: Creating contract details request")
  let spec = contract_details.create_stock_contract_spec("AAPL")
  let request_msg = contract_details.request_contract_details(100, spec)
  let request_size = bit_array.byte_size(request_msg)
  should.be_true(request_size > 0)
  io.println(
    "✓ Contract details request created: "
    <> int.to_string(request_size)
    <> " bytes",
  )
  io.println("   Request ID: 100")
  io.println("   Symbol: AAPL")

  // Test 2.2: Create cancel contract details request
  io.println("\n2.2: Creating cancel contract details request")
  let cancel_msg = contract_details.cancel_contract_details(100)
  let cancel_size = bit_array.byte_size(cancel_msg)
  should.be_true(cancel_size > 0)
  io.println(
    "✓ Cancel contract details request created: "
    <> int.to_string(cancel_size)
    <> " bytes",
  )

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Contract details request messages - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 3: Contract Type Conversion
// ============================================================================

pub fn contract_type_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 3: Contract Type Conversion")
  io.println(repeat_char("=", 70))

  // Test 3.1: Test contract type to string conversion
  io.println("\n3.1: Testing contract type to string conversion")
  let stock_str =
    contract_details.contract_type_to_string(contract_details.Stock)
  should.equal(stock_str, "STK")
  io.println("✓ Stock: " <> stock_str)

  let option_str =
    contract_details.contract_type_to_string(contract_details.Option)
  should.equal(option_str, "OPT")
  io.println("✓ Option: " <> option_str)

  let future_str =
    contract_details.contract_type_to_string(contract_details.Future)
  should.equal(future_str, "FUT")
  io.println("✓ Future: " <> future_str)

  let forex_str =
    contract_details.contract_type_to_string(contract_details.Forex)
  should.equal(forex_str, "CASH")
  io.println("✓ Forex: " <> forex_str)

  // Test 3.2: Test parsing contract types
  io.println("\n3.2: Testing parsing contract types")
  case contract_details.parse_contract_type("STK") {
    Ok(contract_details.Stock) -> io.println("✓ Stock parsed correctly")
    _ -> should.fail()
  }

  case contract_details.parse_contract_type("OPT") {
    Ok(contract_details.Option) -> io.println("✓ Option parsed correctly")
    _ -> should.fail()
  }

  case contract_details.parse_contract_type("FUT") {
    Ok(contract_details.Future) -> io.println("✓ Future parsed correctly")
    _ -> should.fail()
  }

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Contract type conversion - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 4: Right Type Conversion (for Options)
// ============================================================================

pub fn right_type_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 4: Right Type Conversion (for Options)")
  io.println(repeat_char("=", 70))

  // Test 4.1: Test right type to string conversion
  io.println("\n4.1: Testing right type to string conversion")
  let put_str = contract_details.right_type_to_string(contract_details.Put)
  should.equal(put_str, "P")
  io.println("✓ Put: " <> put_str)

  let call_str = contract_details.right_type_to_string(contract_details.Call)
  should.equal(call_str, "C")
  io.println("✓ Call: " <> call_str)

  // Test 4.2: Test parsing right types
  io.println("\n4.2: Testing parsing right types")
  case contract_details.parse_right_type("P") {
    Ok(contract_details.Put) -> io.println("✓ Put parsed correctly")
    _ -> should.fail()
  }

  case contract_details.parse_right_type("C") {
    Ok(contract_details.Call) -> io.println("✓ Call parsed correctly")
    _ -> should.fail()
  }

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Right type conversion - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 5: Security ID Type Conversion
// ============================================================================

pub fn sec_id_type_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 5: Security ID Type Conversion")
  io.println(repeat_char("=", 70))

  // Test 5.1: Test security ID type to string conversion
  io.println("\n5.1: Testing security ID type to string conversion")
  let cusip_str = contract_details.sec_id_type_to_string(contract_details.CUSIP)
  should.equal(cusip_str, "CUSIP")
  io.println("✓ CUSIP: " <> cusip_str)

  let isin_str = contract_details.sec_id_type_to_string(contract_details.ISIN)
  should.equal(isin_str, "ISIN")
  io.println("✓ ISIN: " <> isin_str)

  let ric_str = contract_details.sec_id_type_to_string(contract_details.RIC)
  should.equal(ric_str, "RIC")
  io.println("✓ RIC: " <> ric_str)

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Security ID type conversion - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 6: Contract Details Formatting
// ============================================================================

pub fn contract_details_formatting_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 6: Contract Details Formatting")
  io.println(repeat_char("=", 70))

  // Test 6.1: Create and format contract details
  io.println("\n6.1: Creating and formatting contract details")
  let details = contract_details.create_sample_contract_details()
  let formatted = contract_details.format_contract_details(details)

  should.be_true(string.contains(formatted, "AAPL"))
  should.be_true(string.contains(formatted, "Apple Inc"))
  should.be_true(string.contains(formatted, "Technology"))
  io.println("✓ Contract details formatted:")
  io.println(formatted)

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Contract details formatting - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Test 7: Edge Cases
// ============================================================================

pub fn contract_details_edge_cases_test() {
  io.println("\n" <> repeat_char("=", 70))
  io.println("TEST 7: Contract Details Edge Cases")
  io.println(repeat_char("=", 70))

  // Test 7.1: Invalid contract type
  io.println("\n7.1: Testing invalid contract type")
  case contract_details.parse_contract_type("INVALID") {
    Error(_) ->
      io.println("✓ Correctly returns error for invalid contract type")
    _ -> should.fail()
  }

  // Test 7.2: Invalid right type
  io.println("\n7.2: Testing invalid right type")
  case contract_details.parse_right_type("X") {
    Error(_) -> io.println("✓ Correctly returns error for invalid right type")
    _ -> should.fail()
  }

  io.println("\n" <> repeat_char("-", 70))
  io.println("Summary: Contract details edge cases - PASS")
  io.println(repeat_char("-", 70))
}

// ============================================================================
// Helper Functions
// ============================================================================

fn repeat_char(char: String, times: Int) -> String {
  list.range(0, times - 1)
  |> list.map(fn(_) { char })
  |> string.concat
}

fn string_contains(haystack: String, needle: String) -> Bool {
  case string.split(haystack, needle) {
    [_] -> False
    _ -> True
  }
}

import gleam/int
import gleam/string
