# run_e2e_panic_test.cmake - Compile a .hew file and verify it panics at runtime
# Variables: HEW_CLI, HEW_FILE, EXPECTED_STDERR, OUT_BIN

include(${CMAKE_CURRENT_LIST_DIR}/run_hew_test_common.cmake)

hew_run_panic_test(
  COMPILE_COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_BIN}
  RUN_COMMAND ${OUT_BIN}
  COMPILE_FAILURE_MESSAGE "Compilation failed"
  SUCCESS_MESSAGE "Expected non-zero exit but got 0."
  EXPECTED_STDERR "${EXPECTED_STDERR}"
  RUN_TIMEOUT 10
)
