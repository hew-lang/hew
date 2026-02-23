# run_coro_test.cmake — Compile LLVM IR with coroutine intrinsics and check output
#
# Usage:
#   cmake -DLL_FILE=<path> -DEXPECTED=<string> -DOUT_BIN=<path> -P run_coro_test.cmake

# Find clang
find_program(CLANG clang-21)
if(NOT CLANG)
  find_program(CLANG clang)
endif()
if(NOT CLANG)
  message(FATAL_ERROR "clang not found")
endif()

# Compile the .ll file with -O1 to ensure coroutine passes run
execute_process(
  COMMAND ${CLANG} -O1 -Wno-override-module -o ${OUT_BIN} ${LL_FILE}
  RESULT_VARIABLE compile_rc
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)
if(NOT compile_rc EQUAL 0)
  message(FATAL_ERROR "Compilation failed:\n${compile_err}")
endif()

# Run the compiled binary
execute_process(
  COMMAND ${OUT_BIN}
  RESULT_VARIABLE run_rc
  OUTPUT_VARIABLE run_out
  ERROR_VARIABLE run_err
  TIMEOUT 10
)
if(NOT run_rc EQUAL 0)
  message(FATAL_ERROR "Execution failed (rc=${run_rc}):\n${run_err}")
endif()

# Compare output
if(NOT run_out STREQUAL EXPECTED)
  message(FATAL_ERROR
    "Output mismatch!\nExpected:\n${EXPECTED}\nGot:\n${run_out}")
endif()
