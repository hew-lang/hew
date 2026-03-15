# run_e2e_panic_test.cmake - Compile a .hew file and verify it panics at runtime
# Variables: HEW_CLI, HEW_FILE, EXPECTED_STDERR, OUT_BIN

# Step 1: Compile (should succeed)
execute_process(
  COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_BIN}
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)
if(NOT compile_result EQUAL 0)
  message(FATAL_ERROR "Compilation failed:\n${compile_out}\n${compile_err}")
endif()

# Step 2: Run (should exit with non-zero status)
execute_process(
  COMMAND ${OUT_BIN}
  RESULT_VARIABLE run_result
  OUTPUT_VARIABLE run_out
  ERROR_VARIABLE run_err
  TIMEOUT 10
)
if(run_result EQUAL 0)
  message(FATAL_ERROR
    "Expected non-zero exit but got 0.\nstdout:\n${run_out}\nstderr:\n${run_err}")
endif()

# Step 3: Verify stderr contains expected message
string(FIND "${run_err}" "${EXPECTED_STDERR}" pos)
if(pos EQUAL -1)
  message(FATAL_ERROR
    "stderr does not contain expected text.\n"
    "Expected: ${EXPECTED_STDERR}\n"
    "Got stderr:\n${run_err}\n"
    "Exit code: ${run_result}")
endif()
