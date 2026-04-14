# run_e2e_test_wasm_inline.cmake - Compile a .hew file for WASM, verify inline expected output
# Variables: HEW_CLI, HEW_FILE, EXPECTED, OUT_BIN, WASMTIME

include(${CMAKE_CURRENT_LIST_DIR}/run_hew_test_common.cmake)

hew_run_e2e_test(
  COMPILE_COMMAND ${HEW_CLI} ${HEW_FILE} --target=wasm32-wasi -o ${OUT_BIN}
  RUN_COMMAND ${WASMTIME} run ${OUT_BIN}
  COMPILE_FAILURE_MESSAGE "WASM compilation failed"
  RUN_FAILURE_MESSAGE "WASM execution failed"
  RUN_TIMEOUT 10
)
