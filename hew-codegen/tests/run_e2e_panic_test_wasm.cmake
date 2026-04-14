# run_e2e_panic_test_wasm.cmake - Compile a .hew file for WASM and verify it panics at runtime
# Variables: HEW_CLI, HEW_FILE, EXPECTED_STDERR, OUT_BIN, WASMTIME

include(${CMAKE_CURRENT_LIST_DIR}/run_hew_test_common.cmake)

hew_run_panic_test(
  COMPILE_COMMAND ${HEW_CLI} ${HEW_FILE} --target=wasm32-wasi -o ${OUT_BIN}
  RUN_COMMAND ${WASMTIME} run ${OUT_BIN}
  COMPILE_FAILURE_MESSAGE "WASM compilation failed"
  SUCCESS_MESSAGE "Expected WASM non-zero exit but got 0."
  EXPECTED_STDERR ${EXPECTED_STDERR}
  RUN_TIMEOUT 10
)
