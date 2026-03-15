# run_warn_test.cmake - Compile a .hew file, verify it SUCCEEDS (warning, not error),
# verify the expected warning text appears in compiler output, then run and check output.
# Variables: HEW_CLI, HEW_FILE, EXPECTED_WARNING, EXPECTED_FILE, OUT_BIN

# Step 1: Compile (must succeed)
execute_process(
  COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_BIN}
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)
if(NOT compile_result EQUAL 0)
  message(FATAL_ERROR
    "Expected compilation to SUCCEED (with a warning) but it FAILED.\n"
    "Program: ${HEW_FILE}\n"
    "Expected warning containing: ${EXPECTED_WARNING}\n"
    "stderr:\n${compile_err}\n"
    "stdout:\n${compile_out}")
endif()

# Step 2: Verify the warning text appears in compiler output
string(FIND "${compile_err}" "${EXPECTED_WARNING}" warn_pos)
if(warn_pos EQUAL -1)
  string(FIND "${compile_out}" "${EXPECTED_WARNING}" warn_pos2)
  if(warn_pos2 EQUAL -1)
    message(FATAL_ERROR
      "Compilation succeeded but expected warning was not emitted.\n"
      "Expected warning containing: ${EXPECTED_WARNING}\n"
      "Got stderr:\n${compile_err}\n"
      "Got stdout:\n${compile_out}")
  endif()
endif()

# Step 3: Run the compiled binary
execute_process(
  COMMAND ${OUT_BIN}
  RESULT_VARIABLE run_result
  OUTPUT_VARIABLE run_out
  ERROR_VARIABLE run_err
  TIMEOUT 10
)
if(NOT run_result EQUAL 0)
  message(FATAL_ERROR "Execution failed (exit ${run_result}):\n${run_out}\n${run_err}")
endif()

# Step 4: Compare output against expected file
file(READ ${EXPECTED_FILE} expected_text)
if(NOT "${run_out}" STREQUAL "${expected_text}")
  message(FATAL_ERROR "Output mismatch:\nExpected:\n${expected_text}\nGot:\n${run_out}")
endif()
