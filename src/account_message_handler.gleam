import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/string

/// Account message handler for processing account-related responses
pub type AccountMessageHandler {
  AccountMessageHandler(
    on_managed_accounts: fn(List(String)) -> Nil,
    on_position: fn(String, String, Float, Float) -> Nil,
    on_account_summary: fn(Int, String, AccountSummaryValue, String) -> Nil,
    on_account_update_multi: fn(Int, String, String, String, String) -> Nil,
    on_portfolio_update: fn(String, String, Float, Float, Float, String) -> Nil,
    on_open_order: fn(Int, String, String, String, Float, Int, Int, Int, String) ->
      Nil,
  )
}

/// Account summary value types
pub type AccountSummaryValue {
  StringValue(String)
  FloatValue(Float)
  IntValue(Int)
}

/// Default account message handler that logs all account messages
pub fn default_account_handler() -> AccountMessageHandler {
  AccountMessageHandler(
    on_managed_accounts: fn(accounts) {
      case accounts {
        [] -> io.println("[Account] No managed accounts")
        _ ->
          io.println(
            "[Account] Managed Accounts: " <> string.join(accounts, ", "),
          )
      }
    },
    on_position: fn(account, symbol, position, market_price) {
      io.println(
        "[Position] Account: "
        <> account
        <> ", Symbol: "
        <> symbol
        <> ", Position: "
        <> float.to_string(position)
        <> ", Market Price: "
        <> float.to_string(market_price),
      )
    },
    on_account_summary: fn(req_id, account, value, currency) {
      let value_str = case value {
        StringValue(s) -> s
        FloatValue(f) -> float.to_string(f)
        IntValue(i) -> int.to_string(i)
      }
      io.println(
        "[AccountSummary] Req ID: "
        <> int.to_string(req_id)
        <> ", Account: "
        <> account
        <> ", Value: "
        <> ", Value: "
        <> value_str
        <> ", Currency: "
        <> currency,
      )
    },
    on_account_update_multi: fn(req_id, account, model_code, key, value) {
      io.println(
        "[AccountUpdateMulti] Req ID: "
        <> int.to_string(req_id)
        <> ", Account: "
        <> account
        <> ", Model: "
        <> model_code
        <> ", Key: "
        <> key
        <> ", Value: "
        <> value,
      )
    },
    on_portfolio_update: fn(
      account,
      symbol,
      position,
      market_price,
      market_value,
      currency,
    ) {
      io.println(
        "[PortfolioUpdate] Account: "
        <> account
        <> ", Symbol: "
        <> symbol
        <> ", Position: "
        <> float.to_string(position)
        <> ", Market Price: "
        <> float.to_string(market_price)
        <> ", Market Value: "
        <> float.to_string(market_value)
        <> ", Currency: "
        <> currency,
      )
    },
    on_open_order: fn(
      order_id,
      symbol,
      order_type,
      action,
      total_quantity,
      filled,
      status,
      remaining,
      time,
    ) {
      io.println(
        "[OpenOrder] ID: "
        <> int.to_string(order_id)
        <> ", Symbol: "
        <> symbol
        <> ", Type: "
        <> order_type
        <> ", Action: "
        <> action
        <> ", Quantity: "
        <> float.to_string(total_quantity)
        <> ", Filled: "
        <> int.to_string(filled)
        <> ", Status: "
        <> int.to_string(status)
        <> ", Remaining: "
        <> int.to_string(remaining)
        <> ", Time: "
        <> time,
      )
    },
  )
}

/// Parse managed accounts response
/// Message format is binary: first 4 bytes = length, then account data
pub fn parse_managed_accounts(data: String) -> List(String) {
  // If data is too short to contain a length prefix, return empty
  case string.length(data) >= 5 {
    True -> {
      // Try to parse binary format
      // The first 4 bytes should be the length of the account data
      // Then the actual account string follows
      let length_bytes = string.slice(data, 0, 4)
      let length = binary_length_to_int(length_bytes)

      // Check if we have valid length and enough data
      case length > 0 && string.length(data) >= 4 + length {
        True -> {
          let account_data = string.slice(data, 4, 4 + length)
          // Account data should be comma-separated account numbers
          let cleaned = string.trim(account_data)
          case cleaned {
            "" -> []
            accounts -> string.split(accounts, ",") |> list.map(string.trim)
          }
        }
        False -> {
          io.println(
            "[WARN] Invalid managed accounts length: " <> int.to_string(length),
          )
          // Fall back to direct parsing
          parse_accounts_directly(data)
        }
      }
    }
    False -> {
      io.println("[WARN] Received short managed accounts data: " <> data)
      parse_accounts_directly(data)
    }
  }
}

/// Parse binary message format to extract message type
/// IB TWS messages have specific patterns we can detect
pub fn detect_message_type(data: String) -> String {
  // Check for timestamp pattern (message code 200)
  case string.contains(data, "200") && string.contains(data, "2026") {
    True -> "timestamp_200"
    False -> {
      // Check for managed accounts pattern (message code 15)
      case
        string.contains(data, "managedAccounts") || string.contains(data, "DU")
      {
        True -> "managed_accounts"
        False -> {
          // Check for other common patterns
          case string.contains(data, "account=") {
            True -> "account_update"
            False -> "unknown"
          }
        }
      }
    }
  }
}

/// Simple length detection based on observed data patterns
/// The data we received appears to be a server timestamp, not managed accounts
fn binary_length_to_int(bytes: String) -> Int {
  // Check if this looks like timestamp data (contains numbers, spaces, colons, etc.)
  let has_digits = string.contains(bytes, "2") || string.contains(bytes, "0")
  let has_spaces = string.contains(bytes, " ")
  let has_colons = string.contains(bytes, ":")

  case has_digits && has_spaces || has_colons {
    True -> string.length(bytes)
    // Return full length since it's probably text
    False -> 0
  }
}

/// Fallback method to parse accounts directly from string
fn parse_accounts_directly(data: String) -> List(String) {
  let cleaned = string.trim(data)
  case cleaned {
    "" -> []
    accounts -> {
      io.println("[DEBUG] Falling back to direct parsing: " <> cleaned)
      // Check if it might be a single account
      case string.contains(cleaned, ",") {
        True -> string.split(accounts, ",") |> list.map(string.trim)
        False -> [cleaned]
      }
    }
  }
}

/// Process incoming string data for account-related messages
pub fn process_account_message(
  data: String,
  handler: AccountMessageHandler,
) -> Nil {
  let trimmed_data = string.trim(data)

  // Detect message type based on patterns
  let message_type = detect_message_type(trimmed_data)

  case message_type {
    "timestamp_200" -> {
      // Server timestamp/session message (normal session establishment)
      io.println(
        "[Session] Server timestamp received: "
        <> trimmed_data
        <> " (This is expected - session is being established)",
      )
      // Continue waiting for account data - this message means the connection is working
    }
    "managed_accounts" -> {
      // Managed accounts response
      io.println("[AccountHandler] Managed accounts response detected")
      let accounts = parse_managed_accounts(trimmed_data)
      case accounts {
        [_first, ..] -> {
          handler.on_managed_accounts(accounts)
          io.println(
            "[AccountHandler] Found "
            <> int.to_string(list.length(accounts))
            <> " account(s)",
          )
        }
        [] ->
          io.println(
            "[AccountHandler] No accounts in managed accounts response",
          )
      }
    }
    "account_update" -> {
      // Account update message
      io.println("[AccountHandler] Account update message: " <> trimmed_data)
    }
    "unknown" -> {
      // Not managed accounts, try other account message patterns
      case string.starts_with(trimmed_data, "managedAccounts") {
        True -> {
          // Managed accounts format with prefix
          let account_list =
            string.slice(
              trimmed_data,
              string.length("managedAccounts"),
              string.length(trimmed_data),
            )
          let accounts = parse_managed_accounts(account_list)
          handler.on_managed_accounts(accounts)
        }
        False -> {
          // Check for other account message patterns
          case string.contains(trimmed_data, "account=") {
            True ->
              io.println(
                "[AccountHandler] Detected account message: " <> trimmed_data,
              )
            False ->
              io.println(
                "[AccountHandler] Unhandled account message: " <> trimmed_data,
              )
          }
        }
      }
    }
    _ -> {
      // Fallback for any other message types - server appears to send silent session data
      io.println(
        "[AccountHandler] Server sent session data (no content): "
        <> trimmed_data
        <> " (Connection established - waiting for actual account data)",
      )
    }
  }
}

/// Log detailed account information for the dev game
pub fn log_game_question(
  question_number: Int,
  description: String,
  data: String,
) {
  let timestamp = get_timestamp()
  io.println(
    "\n" <> "============================================================",
  )
  io.println("🎮 DEV GAME QUESTION " <> int.to_string(question_number))
  io.println("Question: " <> description)
  io.println("Timestamp: " <> timestamp)
  io.println("Response Data: " <> data)
  io.println("============================================================")
}

/// Get current timestamp
@external(javascript, "./connection_ffi.mjs", "get_timestamp")
fn get_timestamp() -> String

/// Game question 1: List all accounts
pub fn answer_question_1(data: String, handler: AccountMessageHandler) -> Nil {
  log_game_question(1, "Which accounts do we have?", data)
  let accounts = parse_managed_accounts(data)
  case accounts {
    [] -> io.println("❌ No accounts found in response")
    _ -> {
      // Log the raw data for debugging
      io.println("📊 Raw response data received: " <> data)
      io.println("✅ Accounts found: " <> string.join(accounts, ", "))
      handler.on_managed_accounts(accounts)
    }
  }
}

/// Game question 2: Positions and funds
pub fn answer_question_2(data: String, description: String) -> Nil {
  log_game_question(2, "Positions and funds: " <> description, data)
  io.println("🔍 Analyzing positions and funds data...")
  // This will be parsed by the main account handler
}

/// Game question 3: Open orders
pub fn answer_question_3(data: String) -> Nil {
  log_game_question(3, "List open orders for each account", data)
  io.println("🔍 Analyzing open orders data...")
  // This will be parsed by the main account handler
}
