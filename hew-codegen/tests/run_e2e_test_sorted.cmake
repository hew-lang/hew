# run_e2e_test_sorted.cmake - Like run_e2e_test_file but sorts output lines before comparing.
# For tests with non-deterministic output ordering (concurrent actors).
# Variables: HEW_CLI, HEW_FILE, EXPECTED_FILE, OUT_BIN

# Step 1: Compile
execute_process(
  COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_BIN}
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

# Step 3: Read expected output from file
file(READ ${EXPECTED_FILE} expected_text)

# Step 4: Sort both and compare
string(REGEX REPLACE "\n$" "" run_trimmed "${run_out}")
string(REGEX REPLACE "\n$" "" exp_trimmed "${expected_text}")
string(REPLACE "\n" ";" run_lines "${run_trimmed}")
string(REPLACE "\n" ";" exp_lines "${exp_trimmed}")
list(SORT run_lines)
list(SORT exp_lines)
string(REPLACE ";" "\n" run_sorted "${run_lines}")
string(REPLACE ";" "\n" exp_sorted "${exp_lines}")

if(NOT "${run_sorted}" STREQUAL "${exp_sorted}")
  message(FATAL_ERROR "Output mismatch (sorted comparison):\nExpected (sorted):\n${exp_sorted}\nGot (sorted):\n${run_sorted}")
endif()
