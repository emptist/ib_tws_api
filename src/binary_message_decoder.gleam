import gleam/bit_array
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import protocol

/// IB TWS API Message Codes (2-byte big-endian)
pub type MessageCode {
  /// Error message (code 4)
  ErrorCode
  /// Tick price (code 1)
  TickPriceCode
  /// Tick size (code 2)
  TickSizeCode
  /// Order status (code 3)
  OrderStatusCode
  /// Open order (code 5)
  OpenOrderCode
  /// Account download end (code 16)
  AccountDownloadEndCode
  /// Position data (code 61)
  PositionCode
  /// Position end (code 62)
  PositionEndCode
  /// Account summary (code 63)
  AccountSummaryCode
  /// Account summary end (code 64)
  AccountSummaryEndCode
  /// Managed accounts (code 15)
  ManagedAccountsCode
  /// Next valid ID (code 9)
  NextValidIdCode
  /// Unknown message code
  UnknownCode(Int)
}

/// Parsed message types
pub type ParsedMessage {
  ServerTime(version: Int, timestamp: String)
  ErrorMsg(error_code: Int, error_id: Int, message: String)
  TickPrice(ticker_id: Int, tick_type: Int, price: Float, size: Int)
  TickSize(ticker_id: Int, tick_type: Int, size: Int)
  OrderStatus(
    order_id: Int,
    status: String,
    filled: Int,
    remaining: Int,
    avg_fill_price: Float,
  )
  OpenOrder(
    order_id: Int,
    contract: Contract,
    order: Order,
    order_state: OrderState,
  )
  AccountDownloadEnd(account: String)
  Position(
    account: String,
    contract: Contract,
    position: Float,
    avg_cost: Float,
  )
  PositionEnd
  AccountSummary(
    req_id: Int,
    account: String,
    tag: String,
    value: String,
    currency: String,
  )
  AccountSummaryEnd(req_id: Int)
  ManagedAccounts(accounts: List(String))
  NextValidId(order_id: Int)
  UnknownMessage(code: Int, data: BitArray)
}

/// Contract information
pub type Contract {
  Contract(
    contract_id: Int,
    symbol: String,
    security_type: String,
    last_trade_date_or_contract_month: String,
    strike: Float,
    right: String,
    multiplier: String,
    exchange: String,
    primary_exchange: String,
    currency: String,
    local_symbol: String,
    trading_class: String,
  )
}

/// Order information
pub type Order {
  Order(
    order_id: Int,
    client_id: Int,
    perm_id: Int,
    action: String,
    order_type: String,
    total_quantity: Float,
    lmt_price: Float,
    aux_price: Float,
    tif: String,
    oca_group: String,
    account: String,
  )
}

/// Order state
pub type OrderState {
  OrderState(
    status: String,
    init_margin: String,
    maint_margin: String,
    equity_with_loan: String,
    commission: Float,
    min_commission: Float,
    max_commission: Float,
    commission_currency: String,
    warning_text: String,
  )
}

/// Parse a 2-byte big-endian message code
pub fn parse_message_code(
  data: BitArray,
) -> Result(#(MessageCode, BitArray), String) {
  case data {
    <<code:16, rest:bits>> -> {
      let message_code = case code {
        4 -> ErrorCode
        1 -> TickPriceCode
        2 -> TickSizeCode
        3 -> OrderStatusCode
        5 -> OpenOrderCode
        16 -> AccountDownloadEndCode
        61 -> PositionCode
        62 -> PositionEndCode
        63 -> AccountSummaryCode
        64 -> AccountSummaryEndCode
        15 -> ManagedAccountsCode
        9 -> NextValidIdCode
        _ -> UnknownCode(code)
      }
      Ok(#(message_code, rest))
    }
    _ -> Error("Not enough bytes for message code")
  }
}

/// Read a NULL-terminated string from bit array
pub fn read_null_string(data: BitArray) -> Result(#(String, BitArray), String) {
  case data {
    <<>> -> Error("Empty bit array")
    <<0:8, rest:bits>> -> Ok(#("", rest))
    <<byte:8, rest:bits>> -> {
      case read_null_string(rest) {
        Ok(#(str, remaining)) -> {
          // Convert byte to string using bit array
          let char_str =
            bit_array.to_string(<<byte:8>>)
            |> result.unwrap("")
          Ok(#(char_str <> str, remaining))
        }
        Error(_) -> {
          // Convert byte to string using bit array
          let char_str =
            bit_array.to_string(<<byte:8>>)
            |> result.unwrap("")
          Ok(#(char_str, rest))
        }
      }
    }
    _ -> Error("Invalid bit array format")
  }
}

/// Read an integer from bit array (ASCII decimal representation)
pub fn read_int(data: BitArray) -> Result(#(Int, BitArray), String) {
  case read_null_string(data) {
    Ok(#(str, rest)) -> {
      case int.parse(string.trim(str)) {
        Ok(val) -> Ok(#(val, rest))
        Error(_) -> Error("Invalid integer: " <> str)
      }
    }
    Error(_) -> Error("Failed to read integer string")
  }
}

/// Read a float from bit array (ASCII decimal representation)
pub fn read_float(data: BitArray) -> Result(#(Float, BitArray), String) {
  case read_null_string(data) {
    Ok(#(str, rest)) -> {
      case float.parse(string.trim(str)) {
        Ok(val) -> Ok(#(val, rest))
        Error(_) -> {
          // Try to parse as integer first
          case int.parse(string.trim(str)) {
            Ok(val) -> Ok(#(int.to_float(val), rest))
            Error(_) -> Error("Invalid float: " <> str)
          }
        }
      }
    }
    Error(_) -> Error("Failed to read float string")
  }
}

/// Parse error message (code 4)
pub fn parse_error_message(data: BitArray) -> Result(ParsedMessage, String) {
  case data {
    <<error_code:32, error_id:32, rest:bits>> -> {
      case read_null_string(rest) {
        Ok(#(message, _)) -> Ok(ErrorMsg(error_code, error_id, message))
        Error(_) -> Error("Failed to parse error message string")
      }
    }
    _ -> Error("Invalid error message format")
  }
}

/// Parse position message (code 61)
pub fn parse_position(data: BitArray) -> Result(ParsedMessage, String) {
  case read_null_string(data) {
    Ok(#(account, rest1)) -> {
      case read_int(rest1) {
        Ok(#(contract_id, rest2)) -> {
          case read_null_string(rest2) {
            Ok(#(symbol, rest3)) -> {
              case read_null_string(rest3) {
                Ok(#(sec_type, rest4)) -> {
                  case read_null_string(rest4) {
                    Ok(#(last_trade, rest5)) -> {
                      case read_float(rest5) {
                        Ok(#(strike, rest6)) -> {
                          case read_null_string(rest6) {
                            Ok(#(right, rest7)) -> {
                              case read_null_string(rest7) {
                                Ok(#(multiplier, rest8)) -> {
                                  case read_null_string(rest8) {
                                    Ok(#(exchange, rest9)) -> {
                                      case read_null_string(rest9) {
                                        Ok(#(currency, rest10)) -> {
                                          case read_null_string(rest10) {
                                            Ok(#(local_symbol, rest11)) -> {
                                              case read_null_string(rest11) {
                                                Ok(#(trading_class, rest12)) -> {
                                                  case read_float(rest12) {
                                                    Ok(#(position, rest13)) -> {
                                                      case read_float(rest13) {
                                                        Ok(#(avg_cost, _)) -> {
                                                          let contract =
                                                            Contract(
                                                              contract_id: contract_id,
                                                              symbol: symbol,
                                                              security_type: sec_type,
                                                              last_trade_date_or_contract_month: last_trade,
                                                              strike: strike,
                                                              right: right,
                                                              multiplier: multiplier,
                                                              exchange: exchange,
                                                              primary_exchange: "",
                                                              currency: currency,
                                                              local_symbol: local_symbol,
                                                              trading_class: trading_class,
                                                            )
                                                          Ok(Position(
                                                            account,
                                                            contract,
                                                            position,
                                                            avg_cost,
                                                          ))
                                                        }
                                                        Error(err) ->
                                                          Error(
                                                            "Failed to parse avg_cost: "
                                                            <> err,
                                                          )
                                                      }
                                                    }
                                                    Error(err) ->
                                                      Error(
                                                        "Failed to parse position: "
                                                        <> err,
                                                      )
                                                  }
                                                }
                                                Error(_) ->
                                                  Error(
                                                    "Failed to parse trading_class",
                                                  )
                                              }
                                            }
                                            Error(_) ->
                                              Error(
                                                "Failed to parse local_symbol",
                                              )
                                          }
                                        }
                                        Error(_) ->
                                          Error("Failed to parse currency")
                                      }
                                    }
                                    Error(_) ->
                                      Error("Failed to parse exchange")
                                  }
                                }
                                Error(_) -> Error("Failed to parse multiplier")
                              }
                            }
                            Error(_) -> Error("Failed to parse right")
                          }
                        }
                        Error(_) -> Error("Failed to parse strike")
                      }
                    }
                    Error(_) -> Error("Failed to parse last_trade_date")
                  }
                }
                Error(_) -> Error("Failed to parse sec_type")
              }
            }
            Error(_) -> Error("Failed to parse symbol")
          }
        }
        Error(_) -> Error("Failed to parse contract_id")
      }
    }
    Error(_) -> Error("Failed to parse position account")
  }
}

/// Parse account summary message (code 63)
pub fn parse_account_summary(data: BitArray) -> Result(ParsedMessage, String) {
  case read_int(data) {
    Ok(#(req_id, rest1)) -> {
      case read_null_string(rest1) {
        Ok(#(account, rest2)) -> {
          case read_null_string(rest2) {
            Ok(#(tag, rest3)) -> {
              case read_null_string(rest3) {
                Ok(#(value, rest4)) -> {
                  case read_null_string(rest4) {
                    Ok(#(currency, _)) -> {
                      Ok(AccountSummary(req_id, account, tag, value, currency))
                    }
                    Error(_) ->
                      Error("Failed to parse currency in account summary")
                  }
                }
                Error(_) -> Error("Failed to parse account summary value")
              }
            }
            Error(_) -> Error("Failed to parse account summary tag")
          }
        }
        Error(_) -> Error("Failed to parse account summary account")
      }
    }
    Error(_) -> Error("Failed to parse account summary req_id")
  }
}

/// Parse managed accounts message (code 15)
pub fn parse_managed_accounts(data: BitArray) -> Result(ParsedMessage, String) {
  case read_null_string(data) {
    Ok(#(accounts_str, _)) -> {
      let accounts = case string.trim(accounts_str) {
        "" -> []
        str -> string.split(str, ",") |> list.map(string.trim)
      }
      Ok(ManagedAccounts(accounts))
    }
    Error(_) -> Error("Failed to parse managed accounts")
  }
}

/// Parse next valid ID message (code 9)
pub fn parse_next_valid_id(data: BitArray) -> Result(ParsedMessage, String) {
  case read_int(data) {
    Ok(#(order_id, _)) -> Ok(NextValidId(order_id))
    Error(_) -> Error("Failed to parse next valid ID")
  }
}

/// Parse position end message (code 62)
pub fn parse_position_end(_data: BitArray) -> Result(ParsedMessage, String) {
  Ok(PositionEnd)
}

/// Parse account summary end message (code 64)
pub fn parse_account_summary_end(
  data: BitArray,
) -> Result(ParsedMessage, String) {
  case read_int(data) {
    Ok(#(req_id, _)) -> Ok(AccountSummaryEnd(req_id))
    Error(_) -> Error("Failed to parse account summary end")
  }
}

/// Parse account download end message (code 16)
pub fn parse_account_download_end(
  data: BitArray,
) -> Result(ParsedMessage, String) {
  case read_null_string(data) {
    Ok(#(account, _)) -> Ok(AccountDownloadEnd(account))
    Error(_) -> Error("Failed to parse account download end")
  }
}

/// Main function to parse a complete IB TWS message
/// Format: [4-byte length][2-byte message code][payload...]
pub fn parse_message(data: BitArray) -> Result(ParsedMessage, String) {
  case data {
    <<length:32, rest:bits>> -> {
      let expected_size = bit_array.byte_size(rest)
      case expected_size >= length {
        True -> {
          // Extract the message payload using slice
          let payload = case bit_array.slice(rest, 0, length) {
            Ok(p) -> p
            Error(_) -> <<>>
          }

          // Parse message code from payload
          case parse_message_code(payload) {
            Ok(#(code, message_data)) -> {
              case code {
                ErrorCode -> parse_error_message(message_data)
                TickPriceCode -> Ok(UnknownMessage(1, message_data))
                TickSizeCode -> Ok(UnknownMessage(2, message_data))
                OrderStatusCode -> Ok(UnknownMessage(3, message_data))
                OpenOrderCode -> Ok(UnknownMessage(5, message_data))
                PositionCode -> parse_position(message_data)
                PositionEndCode -> parse_position_end(message_data)
                AccountSummaryCode -> parse_account_summary(message_data)
                AccountSummaryEndCode -> parse_account_summary_end(message_data)
                ManagedAccountsCode -> parse_managed_accounts(message_data)
                NextValidIdCode -> parse_next_valid_id(message_data)
                AccountDownloadEndCode ->
                  parse_account_download_end(message_data)
                UnknownCode(code_num) -> {
                  io.println(
                    "[DEBUG] Unknown message code: " <> int.to_string(code_num),
                  )
                  io.println(
                    "[DEBUG] Message data size: "
                    <> int.to_string(bit_array.byte_size(message_data)),
                  )
                  Ok(UnknownMessage(code_num, message_data))
                }
              }
            }
            Error(err) -> Error("Failed to parse message code: " <> err)
          }
        }
        False ->
          Error(
            "Incomplete message: expected "
            <> int.to_string(length)
            <> " bytes, got "
            <> int.to_string(expected_size),
          )
      }
    }
    _ -> Error("Invalid message format: missing length prefix")
  }
}

/// Parse server handshake response (special case)
/// This is a text-based response, not a binary message
/// Format: [control chars]VERSION[NULL]timestamp EST
/// Example:    200 20260108 08:16:53 EST
pub fn parse_handshake_response(
  data: BitArray,
) -> Result(#(Int, String), String) {
  case bit_array.to_string(data) {
    Ok(data_str) -> {
      // Remove leading control characters using FFI
      let cleaned_str = protocol.strip_leading_control_characters(data_str)

      // Split on NULL byte
      let parts = string.split(cleaned_str, "\u{0000}")
      let non_empty = list.filter(parts, fn(s) { s != "" })

      case non_empty {
        [version_str, ..rest] -> {
          case int.parse(string.trim(version_str)) {
            Ok(version) -> {
              let timestamp = string.join(rest, "\u{0000}")
              Ok(#(version, string.trim(timestamp)))
            }
            Error(_) -> Error("Invalid version: " <> version_str)
          }
        }
        _ -> Error("Invalid handshake response: " <> cleaned_str)
      }
    }
    Error(_) -> Error("Failed to convert bit array to string")
  }
}

/// Log message for debugging
pub fn log_message(message: ParsedMessage) {
  case message {
    ServerTime(version, timestamp) -> {
      io.println(
        "[Message] Server Time - Version: "
        <> int.to_string(version)
        <> ", Time: "
        <> timestamp,
      )
    }
    ErrorMsg(code, id, msg) -> {
      io.println(
        "[Error] Code: "
        <> int.to_string(code)
        <> ", ID: "
        <> int.to_string(id)
        <> ", Message: "
        <> msg,
      )
    }
    TickPrice(ticker_id, tick_type, price, size) -> {
      io.println(
        "[TickPrice] Ticker: "
        <> int.to_string(ticker_id)
        <> ", Type: "
        <> int.to_string(tick_type)
        <> ", Price: "
        <> float.to_string(price)
        <> ", Size: "
        <> int.to_string(size),
      )
    }
    TickSize(ticker_id, tick_type, size) -> {
      io.println(
        "[TickSize] Ticker: "
        <> int.to_string(ticker_id)
        <> ", Type: "
        <> int.to_string(tick_type)
        <> ", Size: "
        <> int.to_string(size),
      )
    }
    OrderStatus(order_id, status, filled, remaining, avg_fill) -> {
      io.println(
        "[OrderStatus] ID: "
        <> int.to_string(order_id)
        <> ", Status: "
        <> status
        <> ", Filled: "
        <> int.to_string(filled)
        <> ", Remaining: "
        <> int.to_string(remaining)
        <> ", Avg Fill: "
        <> float.to_string(avg_fill),
      )
    }
    Position(account, contract, position, avg_cost) -> {
      io.println(
        "[Position] Account: "
        <> account
        <> ", Symbol: "
        <> contract.symbol
        <> ", Qty: "
        <> float.to_string(position)
        <> ", Avg Cost: "
        <> float.to_string(avg_cost),
      )
    }
    AccountSummary(req_id, account, tag, value, currency) -> {
      io.println(
        "[AccountSummary] ReqID: "
        <> int.to_string(req_id)
        <> ", Account: "
        <> account
        <> ", Tag: "
        <> tag
        <> ", Value: "
        <> value
        <> " "
        <> currency,
      )
    }
    ManagedAccounts(accounts) -> {
      io.println("[ManagedAccounts] " <> string.join(accounts, ", "))
    }
    NextValidId(order_id) -> {
      io.println("[NextValidId] " <> int.to_string(order_id))
    }
    PositionEnd -> {
      io.println("[PositionEnd] All positions received")
    }
    AccountSummaryEnd(req_id) -> {
      io.println("[AccountSummaryEnd] ReqID: " <> int.to_string(req_id))
    }
    AccountDownloadEnd(account) -> {
      io.println("[AccountDownloadEnd] Account: " <> account)
    }
    OpenOrder(_, _, _, _) -> {
      io.println("[OpenOrder] Order received")
    }
    UnknownMessage(code, _) -> {
      io.println("[Unknown] Message code: " <> int.to_string(code))
    }
  }
}
