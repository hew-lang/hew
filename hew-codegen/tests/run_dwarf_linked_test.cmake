# run_dwarf_linked_test.cmake - Compile a .hew file to a linked binary and
# assert DWARF survives linking (including dSYM generation on macOS).
#
# Variables: HEW_CLI, DWARFDUMP, HEW_FILE, OUT_BIN, EXPECTED_SYMBOL_LINES

foreach(REQUIRED_VAR HEW_CLI DWARFDUMP HEW_FILE OUT_BIN EXPECTED_SYMBOL_LINES)
  if(NOT DEFINED ${REQUIRED_VAR})
    message(FATAL_ERROR "Missing required variable: ${REQUIRED_VAR}")
  endif()
endforeach()

file(REMOVE ${OUT_BIN})

# Compile to a linked binary with debug info (no --emit-obj).
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

# On macOS, dsymutil produces a .dSYM bundle.  dwarfdump can read either the
# binary or the dSYM — prefer the dSYM when it exists since that is the path
# we are specifically validating.
set(DWARF_TARGET ${OUT_BIN})
if(EXISTS "${OUT_BIN}.dSYM")
  set(DWARF_TARGET "${OUT_BIN}.dSYM")
endif()

get_filename_component(HEW_BASENAME ${HEW_FILE} NAME)
string(REPLACE "," ";" EXPECTED_ENTRIES "${EXPECTED_SYMBOL_LINES}")

foreach(ENTRY IN LISTS EXPECTED_ENTRIES)
  string(REPLACE "|" ";" ENTRY_PARTS "${ENTRY}")
  list(LENGTH ENTRY_PARTS ENTRY_PART_COUNT)
  if(NOT ENTRY_PART_COUNT EQUAL 2)
    message(FATAL_ERROR "Expected symbol entry must be symbol|line, got: ${ENTRY}")
  endif()

  list(GET ENTRY_PARTS 0 SYMBOL_NAME)
  list(GET ENTRY_PARTS 1 EXPECTED_LINE)

  execute_process(
    COMMAND ${DWARFDUMP} --debug-info --name=${SYMBOL_NAME} ${DWARF_TARGET}
    RESULT_VARIABLE dwarf_result
    OUTPUT_VARIABLE dwarf_out
    ERROR_VARIABLE dwarf_err
  )
  if(NOT dwarf_result EQUAL 0)
    message(FATAL_ERROR "dwarfdump failed for ${SYMBOL_NAME}:\n${dwarf_out}\n${dwarf_err}")
  endif()

  string(REGEX MATCH "DW_AT_name[^\n]*\\(\"${SYMBOL_NAME}\"\\)" name_match "${dwarf_out}")
  string(REGEX MATCH "DW_AT_decl_line[^\n]*\\(${EXPECTED_LINE}\\)" line_match "${dwarf_out}")
  string(FIND "${dwarf_out}" "${HEW_BASENAME}" file_match)

  if("${name_match}" STREQUAL "")
    message(FATAL_ERROR "Missing DW_AT_name for ${SYMBOL_NAME}:\n${dwarf_out}")
  endif()
  if("${line_match}" STREQUAL "")
    message(FATAL_ERROR "Missing expected DW_AT_decl_line(${EXPECTED_LINE}) for ${SYMBOL_NAME}:\n${dwarf_out}")
  endif()
  if(file_match EQUAL -1)
    message(FATAL_ERROR "Missing source file ${HEW_BASENAME} for ${SYMBOL_NAME}:\n${dwarf_out}")
  endif()
endforeach()
