# run_lldb_smoke_test.cmake - Compile a .hew file to a linked binary with debug
# info, then exercise LLDB in batch mode against Hew source.
#
# Variables: HEW_CLI, LLDB, HEW_FILE, OUT_BIN, BREAKPOINT_LINE

foreach(REQUIRED_VAR HEW_CLI LLDB HEW_FILE OUT_BIN BREAKPOINT_LINE)
  if(NOT DEFINED ${REQUIRED_VAR})
    message(FATAL_ERROR "Missing required variable: ${REQUIRED_VAR}")
  endif()
endforeach()

file(REMOVE ${OUT_BIN})

execute_process(
  COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_BIN} -g
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)
if(NOT compile_result EQUAL 0)
  message(FATAL_ERROR "Compilation failed:\n${compile_out}\n${compile_err}")
endif()

if(NOT EXISTS ${OUT_BIN})
  message(FATAL_ERROR "Binary was not created: ${OUT_BIN}")
endif()

file(SIZE ${OUT_BIN} bin_size)
if(bin_size EQUAL 0)
  message(FATAL_ERROR "Binary is empty: ${OUT_BIN}")
endif()

get_filename_component(HEW_BASENAME ${HEW_FILE} NAME)
string(REPLACE "." "\\." HEW_BASENAME_REGEX "${HEW_BASENAME}")

execute_process(
  COMMAND ${LLDB}
    --no-lldbinit
    --batch
    -o "target create ${OUT_BIN}"
    -o "breakpoint set --file ${HEW_BASENAME} --line ${BREAKPOINT_LINE}"
    -o "run"
    -o "frame info"
    -o "thread backtrace"
    -o "source list -f ${HEW_BASENAME} -l ${BREAKPOINT_LINE}"
  RESULT_VARIABLE lldb_result
  OUTPUT_VARIABLE lldb_out
  ERROR_VARIABLE lldb_err
)

set(LLDB_OUTPUT "${lldb_out}\n${lldb_err}")

if(NOT lldb_result EQUAL 0)
  message(FATAL_ERROR "LLDB smoke failed:\n${LLDB_OUTPUT}")
endif()

function(assert_lldb_output REGEX DESCRIPTION)
  string(REGEX MATCH "${REGEX}" MATCH_RESULT "${LLDB_OUTPUT}")
  if(NOT MATCH_RESULT)
    message(FATAL_ERROR
      "Missing LLDB evidence for ${DESCRIPTION}.\n"
      "Expected regex: ${REGEX}\n\n"
      "Full LLDB output:\n${LLDB_OUTPUT}"
    )
  endif()
endfunction()

assert_lldb_output(
  "Breakpoint [0-9]+: where = .*${HEW_BASENAME_REGEX}:${BREAKPOINT_LINE}"
  "file/line breakpoint resolution"
)
assert_lldb_output(
  "stop reason = breakpoint"
  "stopping at the Hew breakpoint"
)
assert_lldb_output(
  "frame #0: .* at ${HEW_BASENAME_REGEX}:${BREAKPOINT_LINE}"
  "frame info pointing at Hew source"
)
assert_lldb_output(
  "${BREAKPOINT_LINE}[ \t]+fn main\\(\\)"
  "source listing for Hew main()"
)
assert_lldb_output(
  "21[ \t]+let p = Pair"
  "visible Hew source context around the breakpoint"
)
