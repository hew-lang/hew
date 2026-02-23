# run_wasm_reject_test.cmake - Verify that a .hew file FAILS to compile for WASM
# with an expected error message.
# Variables: HEW_CLI, HEW_FILE, EXPECTED_ERROR, OUT_BIN

# Step 1: Try to compile for WASM (should FAIL)
execute_process(
  COMMAND ${HEW_CLI} build ${HEW_FILE} --target=wasm32-wasi -o ${OUT_BIN}
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)

# Step 2: Verify compilation failed
if(compile_result EQUAL 0)
  message(FATAL_ERROR
    "Expected compilation to FAIL but it SUCCEEDED.\n"
    "Program: ${HEW_FILE}\n"
    "Expected error containing: ${EXPECTED_ERROR}")
endif()

# Step 3: Verify error message contains expected text
string(FIND "${compile_err}" "${EXPECTED_ERROR}" error_pos)
if(error_pos EQUAL -1)
  # Also check stdout (some errors go there)
  string(FIND "${compile_out}" "${EXPECTED_ERROR}" error_pos2)
  if(error_pos2 EQUAL -1)
    message(FATAL_ERROR
      "Compilation failed but error message does not contain expected text.\n"
      "Expected: ${EXPECTED_ERROR}\n"
      "Got stderr:\n${compile_err}\n"
      "Got stdout:\n${compile_out}")
  endif()
endif()
