# run_e2e_test.cmake - Compile a .hew file and verify output
# Variables: HEW_CLI, HEW_FILE, EXPECTED, OUT_BIN

# Step 1: Compile
set(COMPILE_ARGS ${HEW_FILE} -o ${OUT_BIN})
if(DEFINED EXTRA_FLAGS)
  list(APPEND COMPILE_ARGS ${EXTRA_FLAGS})
endif()
execute_process(
  COMMAND ${HEW_CLI} ${COMPILE_ARGS}
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)
if(NOT compile_result EQUAL 0)
  message(FATAL_ERROR "Compilation failed:\n${compile_out}\n${compile_err}")
endif()

# Step 2: Run
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

# Step 3: Compare output
# Convert \n in expected to actual newlines
string(REPLACE "\\n" "\n" expected_text "${EXPECTED}")

if(NOT "${run_out}" STREQUAL "${expected_text}")
  message(FATAL_ERROR "Output mismatch:\nExpected:\n${expected_text}\nGot:\n${run_out}")
endif()
