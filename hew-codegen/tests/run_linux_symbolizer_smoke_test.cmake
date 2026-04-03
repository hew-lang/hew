# run_linux_symbolizer_smoke_test.cmake - Compile a .hew file to a linked
# binary with debug info, look up a few function entry addresses with nm, and
# assert Linux symbolizers resolve them back to Hew source lines.
#
# Variables:
#   HEW_CLI, NM, SYMBOLIZER, SYMBOLIZER_MODE, HEW_FILE, OUT_BIN,
#   EXPECTED_SYMBOL_LINES

foreach(REQUIRED_VAR HEW_CLI NM SYMBOLIZER SYMBOLIZER_MODE HEW_FILE OUT_BIN EXPECTED_SYMBOL_LINES)
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

execute_process(
  COMMAND ${NM} --defined-only ${OUT_BIN}
  RESULT_VARIABLE nm_result
  OUTPUT_VARIABLE nm_out
  ERROR_VARIABLE nm_err
)
if(NOT nm_result EQUAL 0)
  message(FATAL_ERROR "nm failed:\n${nm_out}\n${nm_err}")
endif()

set(NM_OUTPUT "${nm_out}\n${nm_err}")
get_filename_component(HEW_BASENAME ${HEW_FILE} NAME)
string(REPLACE "." "\\." HEW_BASENAME_REGEX "${HEW_BASENAME}")

function(escape_regex INPUT OUTPUT)
  string(REGEX REPLACE "([][+.*^$(){}|\\\\?-])" "\\\\\\1" ESCAPED "${INPUT}")
  set(${OUTPUT} "${ESCAPED}" PARENT_SCOPE)
endfunction()

function(assert_symbolized_location SYMBOL_NAME EXPECTED_LINE)
  escape_regex("${SYMBOL_NAME}" SYMBOL_NAME_REGEX)
  string(REGEX MATCH "(^|\\n)[ \t]*([0-9A-Fa-f]+)[ \t]+[A-Za-z][ \t]+${SYMBOL_NAME_REGEX}(\\n|$)" SYMBOL_MATCH "${NM_OUTPUT}")
  if(NOT SYMBOL_MATCH)
    message(FATAL_ERROR
      "Failed to find symbol `${SYMBOL_NAME}` in nm output.\n"
      "Expected regex: (^|\\n)[ \\t]*([0-9A-Fa-f]+)[ \\t]+[A-Za-z][ \\t]+${SYMBOL_NAME_REGEX}(\\n|$)\n\n"
      "Full nm output:\n${NM_OUTPUT}"
    )
  endif()

  set(SYMBOL_ADDRESS "${CMAKE_MATCH_2}")
  set(SYMBOLIZER_OUTPUT "")

  if(SYMBOLIZER_MODE STREQUAL "addr2line")
    execute_process(
      COMMAND ${SYMBOLIZER} -e ${OUT_BIN} -C -f 0x${SYMBOL_ADDRESS}
      RESULT_VARIABLE symbolizer_result
      OUTPUT_VARIABLE symbolizer_out
      ERROR_VARIABLE symbolizer_err
    )
  elseif(SYMBOLIZER_MODE STREQUAL "llvm-symbolizer")
    execute_process(
      COMMAND ${SYMBOLIZER} --obj=${OUT_BIN} 0x${SYMBOL_ADDRESS}
      RESULT_VARIABLE symbolizer_result
      OUTPUT_VARIABLE symbolizer_out
      ERROR_VARIABLE symbolizer_err
    )
  else()
    message(FATAL_ERROR "Unsupported SYMBOLIZER_MODE: ${SYMBOLIZER_MODE}")
  endif()

  set(SYMBOLIZER_OUTPUT "${symbolizer_out}\n${symbolizer_err}")
  if(NOT symbolizer_result EQUAL 0)
    message(FATAL_ERROR
      "${SYMBOLIZER_MODE} failed for ${SYMBOL_NAME} @ 0x${SYMBOL_ADDRESS}:\n"
      "${SYMBOLIZER_OUTPUT}"
    )
  endif()

  string(REGEX MATCH "(^|\\n).*/?${HEW_BASENAME_REGEX}:${EXPECTED_LINE}(:[0-9]+)?(\\n|$)" LOCATION_MATCH "${SYMBOLIZER_OUTPUT}")
  if(NOT LOCATION_MATCH)
    message(FATAL_ERROR
      "Expected ${SYMBOLIZER_MODE} to resolve ${SYMBOL_NAME} @ 0x${SYMBOL_ADDRESS} "
      "to ${HEW_BASENAME}:${EXPECTED_LINE}.\n\n"
      "Full ${SYMBOLIZER_MODE} output:\n${SYMBOLIZER_OUTPUT}"
    )
  endif()
endfunction()

string(REPLACE "," ";" SYMBOL_EXPECTATIONS "${EXPECTED_SYMBOL_LINES}")
foreach(ENTRY IN LISTS SYMBOL_EXPECTATIONS)
  string(REPLACE "|" ";" PARTS "${ENTRY}")
  list(LENGTH PARTS PART_COUNT)
  if(NOT PART_COUNT EQUAL 2)
    message(FATAL_ERROR "Malformed EXPECTED_SYMBOL_LINES entry: ${ENTRY}")
  endif()

  list(GET PARTS 0 SYMBOL_NAME)
  list(GET PARTS 1 EXPECTED_LINE)
  assert_symbolized_location("${SYMBOL_NAME}" "${EXPECTED_LINE}")
endforeach()
