# run_e2e_test_file.cmake - Compile a .hew file and verify output against .expected file
# Variables: HEW_CLI, HEW_FILE, EXPECTED_FILE, OUT_BIN

include(${CMAKE_CURRENT_LIST_DIR}/run_hew_test_common.cmake)

hew_run_e2e_test(
  COMPILE_COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_BIN}
  RUN_COMMAND ${OUT_BIN}
  COMPILE_FAILURE_MESSAGE "Compilation failed"
  RUN_FAILURE_MESSAGE "Execution failed"
  RUN_TIMEOUT 10
)
