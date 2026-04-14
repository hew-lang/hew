# run_e2e_test.cmake - Compile a .hew file and verify output
# Variables: HEW_CLI, HEW_FILE, EXPECTED, OUT_BIN

include(${CMAKE_CURRENT_LIST_DIR}/run_hew_test_common.cmake)

set(COMPILE_COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_BIN})
if(DEFINED EXTRA_FLAGS)
  list(APPEND COMPILE_COMMAND ${EXTRA_FLAGS})
endif()

hew_run_e2e_test(
  COMPILE_COMMAND ${COMPILE_COMMAND}
  RUN_COMMAND ${OUT_BIN}
  COMPILE_FAILURE_MESSAGE "Compilation failed"
  RUN_FAILURE_MESSAGE "Execution failed"
  RUN_TIMEOUT 10
)
