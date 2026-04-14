# run_reject_test.cmake - Verify that a .hew file FAILS to compile
# with an expected error message.
# Variables: HEW_CLI, HEW_FILE, EXPECTED_ERROR, OUT_BIN

include(${CMAKE_CURRENT_LIST_DIR}/run_hew_test_common.cmake)

hew_run_reject_test(
  COMPILE_COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_BIN}
  SUCCESS_MESSAGE "Expected compilation to FAIL but it SUCCEEDED."
)
