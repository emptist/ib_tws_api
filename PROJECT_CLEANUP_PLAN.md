# Project Cleanup Plan

**Date:** 2026-01-09  
**Reason:** Project has grown too large with many redundant files  
**Goal:** Simplify structure, remove duplicates, consolidate documentation

---

## Current State Analysis

### Documentation Files (19 files - MANY REDUNDANT!)
```
API_CONFIGURATION_GUIDE.md
CODE_REVIEW.md
CONNECTION_AND_PARSING_SUCCESS.md
CONNECTION_STATUS_AND_NEXT_STEPS.md
CURRENT_STATUS.md
DEVELOPMENT_PLAN.md
FEATURE_ANALYSIS.md
HANDSHAKE_ISSUE_SUMMARY.md
HANDSHAKE_SUCCESS.md
PHASE9_COMPLETION.md
PHASE10_PLAN.md
PROJECT_SLOGAN.md
PROJECT_STATUS.md
STATUS_SUMMARY.md
SYSTEM_LEVEL_REVIEW.md
TECHNICAL_NOTES.md
TEST_SUITE.md
TWS_API_CONFIGURATION_TROUBLESHOOTING.md
TWS_API_CONNECTION_ISSUE.md
dev_game.md
configuration_reminder.gleam
```

### Source Files (src/) - 33 files
**Core modules (keep):**
- connection.gleam
- connection_ffi.mjs
- protocol.gleam
- message_encoder.gleam
- binary_message_decoder.gleam
- message_handler.gleam
- messages.gleam
- ib_tws_api.gleam

**Feature modules (keep for future use):**
- account_data.gleam
- market_data.gleam
- orders.gleam
- order_management.gleam
- real_time_bars.gleam
- portfolio_updates.gleam
- query_accounts.gleam
- query_accounts_raw.gleam
- account_message_handler.gleam
- contract_details.gleam
- historical_data.gleam
- execution_reports.gleam
- market_depth.gleam
- market_scanner.gleam
- news_and_research.gleam
- fundamental_data.gleam

**Test files in src/ (MOVE to test/):**
- complete_handshake_test.gleam → test/
- diagnostic_connection.gleam → DELETE (redundant)
- diagnostic_immediate.gleam → DELETE (redundant)
- dev_game_runner.gleam → test/
- run_connection_diagnostic.gleam → DELETE (redundant)
- safe_handshake_test.gleam → test/
- simple_connect_test.gleam → test/
- simple_handshake_only_test.gleam → test/
- protocol_fixed.gleam → DELETE (outdated, use protocol.gleam)
- capture_all_data.gleam → test/

### Test Files (test/) - 33 files!
**Diagnostic tests (DELETE most):**
- async_connection_test.gleam → DELETE
- async_handshake_test.gleam → DELETE
- auto_port_detection_test.gleam → DELETE
- auto_port_test.gleam → DELETE
- callback_based_connection.gleam → DELETE
- callback_handshake_test.gleam → DELETE
- check_port.gleam → KEEP (useful for port checking)
- comprehensive_connection_test.gleam → DELETE
- diagnostic_port_check.gleam → DELETE
- diagnostic_test.gleam → DELETE
- extended_wait_test.gleam → DELETE
- improved_handshake_test.gleam → DELETE
- keep_alive_handshake_test.gleam → KEEP (current test)
- persistent_connection_test.gleam → DELETE
- proper_client_id_test.gleam → DELETE
- proper_handshake_test.gleam → DELETE
- simple_callback_test.gleam → DELETE
- simple_diagnostic.gleam → DELETE
- simple_working_connection.gleam → DELETE
- test_handshake_only.gleam → KEEP (useful for handshake testing)

**Feature tests (KEEP):**
- account_data_test.gleam
- contract_details_test.gleam
- execution_reports_test.gleam
- fundamental_data_test.gleam
- market_data_test.gleam
- market_depth_test.gleam
- market_scanner_test.gleam
- message_parsing_test.gleam
- news_and_research_test.gleam
- orders_test.gleam
- portfolio_updates_test.gleam
- historical_data_test.gleam

**Integration tests (KEEP):**
- ib_tws_api_test.gleam
- real_account_data_test.gleam
- live_account_test.gleam
- paper_account_test.gleam
- paper_trading_verification.gleam
- dev_game_complete.gleam

### Examples (examples/) - 8 files (KEEP ALL)
These are useful for users.

---

## Cleanup Strategy

### Phase 1: Consolidate Documentation

**Keep:**
1. README.md (main documentation)
2. DEVELOPMENT_PLAN.md (current plan)
3. SYSTEM_LEVEL_REVIEW.md (critical issues)
4. TECHNICAL_NOTES.md (technical lessons)
5. docs/protocol_specification.md (protocol reference)
6. docs/reference_implementation_analysis.md (reference analysis)

**Merge and Delete:**
- Merge HANDSHAKE_ISSUE_SUMMARY.md, HANDSHAKE_SUCCESS.md, CONNECTION_AND_PARSING_SUCCESS.md, CONNECTION_STATUS_AND_NEXT_STEPS.md → SYSTEM_LEVEL_REVIEW.md
- Merge CURRENT_STATUS.md, PROJECT_STATUS.md, STATUS_SUMMARY.md → STATUS.md
- Delete PHASE9_COMPLETION.md, PHASE10_PLAN.md (outdated)
- Delete FEATURE_ANALYSIS.md (not needed)
- Delete PROJECT_SLOGAN.md (not needed)
- Delete TEST_SUITE.md (not needed)
- Delete API_CONFIGURATION_GUIDE.md (info in README)
- Delete TWS_API_CONFIGURATION_TROUBLESHOOTING.md (info in README)
- Delete TWS_API_CONNECTION_ISSUE.md (info in SYSTEM_LEVEL_REVIEW.md)
- Delete CODE_REVIEW.md (outdated)
- Delete dev_game.md (game is in dev_game_complete.gleam)
- Delete configuration_reminder.gleam (info in README)

### Phase 2: Clean Up Source Directory

**Move test files to test/:**
- complete_handshake_test.gleam → test/handshake_test.gleam
- safe_handshake_test.gleam → test/safe_handshake_test.gleam
- simple_connect_test.gleam → test/simple_connect_test.gleam
- simple_handshake_only_test.gleam → test/simple_handshake_only_test.gleam
- dev_game_runner.gleam → test/dev_game_runner.gleam
- capture_all_data.gleam → test/capture_all_data.gleam

**Delete redundant files:**
- protocol_fixed.gleam (use protocol.gleam)
- diagnostic_connection.gleam
- diagnostic_immediate.gleam
- run_connection_diagnostic.gleam

### Phase 3: Clean Up Test Directory

**Keep essential diagnostic tests:**
- check_port.gleam
- detect_ports.gleam
- keep_alive_handshake_test.gleam
- test_handshake_only.gleam

**Delete redundant diagnostic tests:**
- async_connection_test.gleam
- async_handshake_test.gleam
- auto_port_detection_test.gleam
- auto_port_test.gleam
- callback_based_connection.gleam
- callback_handshake_test.gleam
- comprehensive_connection_test.gleam
- diagnostic_port_check.gleam
- diagnostic_test.gleam
- extended_wait_test.gleam
- improved_handshake_test.gleam
- persistent_connection_test.gleam
- proper_client_id_test.gleam
- proper_handshake_test.gleam
- simple_callback_test.gleam
- simple_diagnostic.gleam
- simple_working_connection.gleam

**Keep feature and integration tests:**
- account_data_test.gleam
- contract_details_test.gleam
- execution_reports_test.gleam
- fundamental_data_test.gleam
- historical_data_test.gleam
- market_data_test.gleam
- market_depth_test.gleam
- market_scanner_test.gleam
- message_parsing_test.gleam
- news_and_research_test.gleam
- orders_test.gleam
- portfolio_updates_test.gleam
- ib_tws_api_test.gleam
- real_account_data_test.gleam
- live_account_test.gleam
- paper_account_test.gleam
- paper_trading_verification.gleam
- dev_game_complete.gleam

### Phase 4: Clean Up Root Directory

**Delete:**
- api_capture_log.txt (old log)
- test_output.log (old log)

---

## Final Structure After Cleanup

```
ib_tws_api/
├── README.md                          # Main documentation
├── DEVELOPMENT_PLAN.md                 # Development roadmap
├── SYSTEM_LEVEL_REVIEW.md             # Critical issues and fixes
├── TECHNICAL_NOTES.md                # Technical lessons
├── STATUS.md                         # Current project status
├── gleam.toml
├── manifest.toml
├── package.json
├── package-lock.json
├── .gitignore
├── .github/
│   └── workflows/
├── docs/
│   ├── protocol_specification.md       # Protocol reference
│   └── reference_implementation_analysis.md  # Reference analysis
├── examples/                         # User examples (8 files)
│   ├── account_data.gleam
│   ├── advanced_order_types.gleam
│   ├── basic_connection.gleam
│   ├── historical_data_example.gleam
│   ├── market_data.gleam
│   ├── order_management_example.gleam
│   ├── order_placement.gleam
│   └── real_time_bars_example.gleam
├── src/
│   ├── connection.gleam              # Core connection
│   ├── connection_ffi.mjs           # JavaScript FFI
│   ├── protocol.gleam               # Protocol messages
│   ├── message_encoder.gleam        # Message encoding
│   ├── binary_message_decoder.gleam  # Message decoding
│   ├── message_handler.gleam        # Message handling
│   ├── messages.gleam               # Message types
│   ├── ib_tws_api.gleam            # Main module
│   ├── account_data.gleam           # Account data features
│   ├── market_data.gleam            # Market data features
│   ├── orders.gleam                # Order features
│   ├── order_management.gleam       # Order management
│   ├── real_time_bars.gleam         # Real-time bars
│   ├── portfolio_updates.gleam       # Portfolio updates
│   ├── query_accounts.gleam         # Account queries
│   ├── query_accounts_raw.gleam      # Raw account queries
│   ├── account_message_handler.gleam  # Account message handling
│   ├── contract_details.gleam        # Contract details
│   ├── historical_data.gleam         # Historical data
│   ├── execution_reports.gleam       # Execution reports
│   ├── market_depth.gleam           # Market depth
│   ├── market_scanner.gleam         # Market scanner
│   ├── news_and_research.gleam      # News and research
│   └── fundamental_data.gleam      # Fundamental data
└── test/
    ├── check_port.gleam             # Port checking
    ├── detect_ports.gleam           # Port detection
    ├── keep_alive_handshake_test.gleam  # Handshake test
    ├── test_handshake_only.gleam    # Handshake only test
    ├── account_data_test.gleam      # Account data tests
    ├── contract_details_test.gleam  # Contract tests
    ├── execution_reports_test.gleam  # Execution tests
    ├── fundamental_data_test.gleam   # Fundamental tests
    ├── historical_data_test.gleam   # Historical tests
    ├── market_data_test.gleam       # Market data tests
    ├── market_depth_test.gleam      # Market depth tests
    ├── market_scanner_test.gleam    # Market scanner tests
    ├── message_parsing_test.gleam   # Message parsing tests
    ├── news_and_research_test.gleam  # News tests
    ├── orders_test.gleam            # Order tests
    ├── portfolio_updates_test.gleam  # Portfolio tests
    ├── ib_tws_api_test.gleam       # Integration tests
    ├── real_account_data_test.gleam  # Real account tests
    ├── live_account_test.gleam       # Live account tests
    ├── paper_account_test.gleam      # Paper account tests
    ├── paper_trading_verification.gleam  # Paper trading tests
    ├── dev_game_complete.gleam      # Game completion test
    ├── handshake_test.gleam          # Handshake tests (moved from src/)
    ├── safe_handshake_test.gleam    # Safe handshake tests
    ├── simple_connect_test.gleam     # Simple connection tests
    ├── simple_handshake_only_test.gleam  # Simple handshake tests
    ├── dev_game_runner.gleam         # Game runner
    └── capture_all_data.gleam        # Data capture tests
```

---

## Execution Steps

### Step 1: Delete redundant documentation files
```bash
rm API_CONFIGURATION_GUIDE.md
rm CODE_REVIEW.md
rm CONNECTION_AND_PARSING_SUCCESS.md
rm CONNECTION_STATUS_AND_NEXT_STEPS.md
rm CURRENT_STATUS.md
rm FEATURE_ANALYSIS.md
rm HANDSHAKE_ISSUE_SUMMARY.md
rm HANDSHAKE_SUCCESS.md
rm PHASE9_COMPLETION.md
rm PHASE10_PLAN.md
rm PROJECT_SLOGAN.md
rm PROJECT_STATUS.md
rm STATUS_SUMMARY.md
rm TEST_SUITE.md
rm TWS_API_CONFIGURATION_TROUBLESHOOTING.md
rm TWS_API_CONNECTION_ISSUE.md
rm dev_game.md
rm configuration_reminder.gleam
```

### Step 2: Delete old log files
```bash
rm api_capture_log.txt
rm test_output.log
```

### Step 3: Delete redundant source files
```bash
rm src/protocol_fixed.gleam
rm src/diagnostic_connection.gleam
rm src/diagnostic_immediate.gleam
rm src/run_connection_diagnostic.gleam
```

### Step 4: Move test files from src/ to test/
```bash
mv src/complete_handshake_test.gleam test/handshake_test.gleam
mv src/safe_handshake_test.gleam test/safe_handshake_test.gleam
mv src/simple_connect_test.gleam test/simple_connect_test.gleam
mv src/simple_handshake_only_test.gleam test/simple_handshake_only_test.gleam
mv src/dev_game_runner.gleam test/dev_game_runner.gleam
mv src/capture_all_data.gleam test/capture_all_data.gleam
```

### Step 5: Delete redundant test files
```bash
rm test/async_connection_test.gleam
rm test/async_handshake_test.gleam
rm test/auto_port_detection_test.gleam
rm test/auto_port_test.gleam
rm test/callback_based_connection.gleam
rm test/callback_handshake_test.gleam
rm test/comprehensive_connection_test.gleam
rm test/diagnostic_port_check.gleam
rm test/diagnostic_test.gleam
rm test/extended_wait_test.gleam
rm test/improved_handshake_test.gleam
rm test/persistent_connection_test.gleam
rm test/proper_client_id_test.gleam
rm test/proper_handshake_test.gleam
rm test/simple_callback_test.gleam
rm test/simple_diagnostic.gleam
rm test/simple_working_connection.gleam
```

### Step 6: Create STATUS.md
Merge current status from deleted files into new STATUS.md

---

## Benefits of Cleanup

1. **Reduced confusion:** Clearer structure, easier to navigate
2. **Faster compilation:** Fewer files to compile
3. **Better maintainability:** Less redundant code to maintain
4. **Clearer documentation:** Consolidated info in fewer files
5. **Easier onboarding:** New users can find what they need faster

---

## Post-Cleanup Tasks

1. Update README.md to reflect new structure
2. Update DEVELOPMENT_PLAN.md with cleanup changes
3. Run `gleam test` to ensure all tests still work
4. Run `gleam build` to ensure compilation works
5. Commit changes to git with clear message