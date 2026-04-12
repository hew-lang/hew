# run_coro_test.cmake — Compile LLVM IR with coroutine intrinsics and check output
#
# Usage:
#   cmake -DCLANG=<path> -DLL_FILE=<path> -DEXPECTED=<string> -DOUT_BIN=<path>
#         -P run_coro_test.cmake
#
# CLANG must match the LLVM version the .ll intrinsic declarations target.
# Apple clang ships different coro intrinsic return types than upstream LLVM
# (e.g. Apple clang 21 defines llvm.coro.end as returning i1; upstream LLVM 22+
# defines it as returning void), so using the system clang when the project is
# built against Homebrew LLVM will cause "Intrinsic has incorrect return type!".
# CMakeLists.txt passes the project-configured clang via -DCLANG=.

# Prefer the caller-supplied CLANG (set by CTest via CMakeLists.txt).
if(NOT CLANG)
  find_program(CLANG clang)
endif()
if(NOT CLANG)
  message(FATAL_ERROR "clang not found — pass -DCLANG=<path> or ensure clang is on PATH")
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
