# run_lldb_name_breakpoint_parity_test.cmake
#
# Verifies two LLDB breakpoint paths against a linked Hew binary:
#
#   1. Plain-name breakpoints  (breakpoint set -n add)  continue to resolve.
#      These work because LLDB matches DW_AT_name entries in DWARF.
#
#   2. Dotted Hew method names (hew-break Pair.sum) resolve via the
#      hew-break helper command in hew_lldb.py, which internally uses a
#      regex breakpoint that matches the Hew mangled symbol.
#
# Required variables: HEW_CLI, LLDB, HEW_FILE, OUT_BIN, HEW_LLDB_SCRIPT
#
# EXPECTED_ADD_LINE  / EXPECTED_SUM_LINE may be set to override the default
# expected stop-file-line numbers (defaults: 16 / 11).

foreach(REQUIRED_VAR HEW_CLI LLDB HEW_FILE OUT_BIN HEW_LLDB_SCRIPT)
  if(NOT DEFINED ${REQUIRED_VAR})
    message(FATAL_ERROR "Missing required variable: ${REQUIRED_VAR}")
  endif()
endforeach()

if(NOT DEFINED EXPECTED_ADD_LINE)
  set(EXPECTED_ADD_LINE 16)
endif()
if(NOT DEFINED EXPECTED_SUM_LINE)
  set(EXPECTED_SUM_LINE 11)
endif()

# ── Compile ───────────────────────────────────────────────────────────────────

file(REMOVE ${OUT_BIN})

execute_process(
  COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_BIN} -g
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)
if(NOT compile_result EQUAL 0)
  message(FATAL_ERROR "Compilation failed:\n${compile_out}\n${compile_err}")
endif()

if(NOT EXISTS ${OUT_BIN})
  message(FATAL_ERROR "Binary was not created: ${OUT_BIN}")
endif()

file(SIZE ${OUT_BIN} bin_size)
if(bin_size EQUAL 0)
  message(FATAL_ERROR "Binary is empty: ${OUT_BIN}")
endif()

# ── LLDB batch session ────────────────────────────────────────────────────────
#
# Step 1: plain-name check — breakpoint set -n add
# Step 2: dotted-name check — hew-break Pair.sum
#
# Both commands run against the same target before any process starts, so we
# only need symbol resolution, not execution.

execute_process(
  COMMAND ${LLDB}
    --no-lldbinit
    --batch
    -o "target create ${OUT_BIN}"
    -o "command script import ${HEW_LLDB_SCRIPT}"
    -o "breakpoint set -n add"
    -o "hew-break Pair.sum"
    -o "breakpoint list"
  RESULT_VARIABLE lldb_result
  OUTPUT_VARIABLE lldb_out
  ERROR_VARIABLE lldb_err
)

set(LLDB_OUTPUT "${lldb_out}\n${lldb_err}")

if(NOT lldb_result EQUAL 0)
  message(FATAL_ERROR "LLDB parity test failed:\n${LLDB_OUTPUT}")
endif()

# ── Assertion helpers ─────────────────────────────────────────────────────────

function(assert_lldb_output REGEX DESCRIPTION)
  string(REGEX MATCH "${REGEX}" MATCH_RESULT "${LLDB_OUTPUT}")
  if(NOT MATCH_RESULT)
    message(FATAL_ERROR
      "Missing LLDB evidence for: ${DESCRIPTION}\n"
      "Expected regex: ${REGEX}\n\n"
      "Full LLDB output:\n${LLDB_OUTPUT}"
    )
  endif()
endfunction()

function(refute_lldb_output REGEX DESCRIPTION)
  string(REGEX MATCH "${REGEX}" MATCH_RESULT "${LLDB_OUTPUT}")
  if(MATCH_RESULT)
    message(FATAL_ERROR
      "Unexpected LLDB output for: ${DESCRIPTION}\n"
      "Unexpected regex: ${REGEX}\n\n"
      "Full LLDB output:\n${LLDB_OUTPUT}"
    )
  endif()
endfunction()

# ── Assertions ────────────────────────────────────────────────────────────────

# 1. Plain -n add resolves: the breakpoint list shows the Hew add() location.
assert_lldb_output(
  "dwarf_linked_binary\\.hew:${EXPECTED_ADD_LINE}"
  "plain -n add location maps to add() at line ${EXPECTED_ADD_LINE}"
)

# 2. hew-break Pair.sum resolves to exactly 1 location (the unique impl method).
#    The helper command prints "Breakpoint N: 1 location(s)." on success.
assert_lldb_output(
  "Breakpoint [0-9]+: 1 location"
  "hew-break Pair.sum resolved to exactly 1 location"
)

# The resolved Pair.sum location must point to the correct method source line.
assert_lldb_output(
  "dwarf_linked_binary\\.hew:${EXPECTED_SUM_LINE}"
  "hew-break Pair.sum location maps to Pair.sum at line ${EXPECTED_SUM_LINE}"
)

# 3. No failure message from the helper command.
refute_lldb_output(
  "no locations found for 'Pair\\.sum'"
  "hew-break Pair.sum must not report failure"
)

message(STATUS "LLDB name-breakpoint parity test passed.")
