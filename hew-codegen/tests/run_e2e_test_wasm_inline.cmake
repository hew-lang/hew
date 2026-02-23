# run_e2e_test_wasm_inline.cmake - Compile a .hew file for WASM, verify inline expected output
# Variables: HEW_CLI, HEW_FILE, EXPECTED, OUT_BIN, WASMTIME

# Step 1: Compile for WASM
execute_process(
  COMMAND ${HEW_CLI} ${HEW_FILE} --target=wasm32-wasi -o ${OUT_BIN}
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)
if(NOT compile_result EQUAL 0)
  message(FATAL_ERROR "WASM compilation failed:\n${compile_out}\n${compile_err}")
endif()

# Step 2: Run with wasmtime
execute_process(
  COMMAND ${WASMTIME} run ${OUT_BIN}
  RESULT_VARIABLE run_result
  OUTPUT_VARIABLE run_out
  ERROR_VARIABLE run_err
  TIMEOUT 10
)
if(NOT run_result EQUAL 0)
  message(FATAL_ERROR "WASM execution failed (exit ${run_result}):\n${run_out}\n${run_err}")
endif()

# Step 3: Compare output
string(REPLACE "\\n" "\n" expected_text "${EXPECTED}")
if(NOT "${run_out}" STREQUAL "${expected_text}")
  message(FATAL_ERROR "Output mismatch:\nExpected:\n${expected_text}\nGot:\n${run_out}")
endif()
