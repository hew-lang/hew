# run_atos_smoke_test.cmake - Compile a linked Hew debug binary and confirm
# Darwin's native atos resolves a few known addresses back to .hew:line.
#
# Variables: HEW_CLI, ATOS, NM_TOOL, HEW_FILE, OUT_BIN, EXPECTED_SYMBOL_LINES

foreach(REQUIRED_VAR HEW_CLI ATOS NM_TOOL HEW_FILE OUT_BIN EXPECTED_SYMBOL_LINES)
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
get_filename_component(OUT_BIN_NAME ${OUT_BIN} NAME)

set(ATOS_TARGET ${OUT_BIN})
set(DSYM_DWARF_PATH "${OUT_BIN}.dSYM/Contents/Resources/DWARF/${OUT_BIN_NAME}")
if(EXISTS "${OUT_BIN}.dSYM")
  if(NOT EXISTS "${DSYM_DWARF_PATH}")
    message(FATAL_ERROR "dSYM bundle exists but DWARF binary is missing: ${DSYM_DWARF_PATH}")
  endif()
  set(ATOS_TARGET "${DSYM_DWARF_PATH}")
endif()

execute_process(
  COMMAND ${NM_TOOL} -nm ${OUT_BIN}
  RESULT_VARIABLE nm_result
  OUTPUT_VARIABLE nm_out
  ERROR_VARIABLE nm_err
)
if(NOT nm_result EQUAL 0)
  message(FATAL_ERROR "nm failed:\n${nm_out}\n${nm_err}")
endif()

string(REPLACE "," ";" EXPECTED_ENTRIES "${EXPECTED_SYMBOL_LINES}")

foreach(EXPECTED_ENTRY IN LISTS EXPECTED_ENTRIES)
  string(REPLACE "|" ";" ENTRY_FIELDS "${EXPECTED_ENTRY}")
  list(GET ENTRY_FIELDS 0 SYMBOL_NAME)
  list(GET ENTRY_FIELDS 1 EXPECTED_LINE)

  string(REGEX MATCH "([0-9A-Fa-f]+)[^\n]* ${SYMBOL_NAME}(\n|$)" symbol_match "${nm_out}")
  if(NOT symbol_match)
    message(FATAL_ERROR "Missing symbol address for ${SYMBOL_NAME}:\n${nm_out}")
  endif()
  set(SYMBOL_ADDRESS "0x${CMAKE_MATCH_1}")

  execute_process(
    COMMAND ${ATOS} -o ${ATOS_TARGET} ${SYMBOL_ADDRESS}
    RESULT_VARIABLE atos_result
    OUTPUT_VARIABLE atos_out
    ERROR_VARIABLE atos_err
  )
  if(NOT atos_result EQUAL 0)
    message(FATAL_ERROR
      "atos failed for ${SYMBOL_NAME} at ${SYMBOL_ADDRESS}:\n${atos_out}\n${atos_err}"
    )
  endif()

  string(STRIP "${atos_out}" atos_out)
  string(FIND "${atos_out}" "${HEW_BASENAME}:${EXPECTED_LINE})" file_line_match)
  if(file_line_match EQUAL -1)
    message(FATAL_ERROR
      "atos did not resolve ${SYMBOL_NAME} to ${HEW_BASENAME}:${EXPECTED_LINE}:\n${atos_out}"
    )
  endif()
endforeach()
