# run_wasm_reject_test.cmake - Verify that a .hew file FAILS to compile for WASM
# with an expected error message.
# Variables: HEW_CLI, HEW_FILE, EXPECTED_ERROR, OUT_BIN

include(${CMAKE_CURRENT_LIST_DIR}/run_hew_test_common.cmake)

hew_run_reject_test(
  COMPILE_COMMAND ${HEW_CLI} build ${HEW_FILE} --target=wasm32-wasi -o ${OUT_BIN}
  SUCCESS_MESSAGE "Expected compilation to FAIL but it SUCCEEDED."
)
