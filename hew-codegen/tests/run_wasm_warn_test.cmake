# run_wasm_warn_test.cmake - Compile a .hew file for WASM, verify it SUCCEEDS
# (warning, not error), and verify the expected warning text appears in compiler output.
# Variables: HEW_CLI, HEW_FILE, EXPECTED_WARNING, OUT_BIN

execute_process(
  COMMAND ${HEW_CLI} ${HEW_FILE} --target=wasm32-wasi -o ${OUT_BIN}
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)
if(NOT compile_result EQUAL 0)
  message(FATAL_ERROR
    "Expected WASM compilation to SUCCEED (with a warning) but it FAILED.\n"
    "Program: ${HEW_FILE}\n"
    "Expected warning containing: ${EXPECTED_WARNING}\n"
    "stderr:\n${compile_err}\n"
    "stdout:\n${compile_out}")
endif()

string(FIND "${compile_err}" "${EXPECTED_WARNING}" warn_pos)
if(warn_pos EQUAL -1)
  string(FIND "${compile_out}" "${EXPECTED_WARNING}" warn_pos2)
  if(warn_pos2 EQUAL -1)
    message(FATAL_ERROR
      "WASM compilation succeeded but expected warning was not emitted.\n"
      "Expected warning containing: ${EXPECTED_WARNING}\n"
      "Got stderr:\n${compile_err}\n"
      "Got stdout:\n${compile_out}")
  endif()
endif()
