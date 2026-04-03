# dwarf_assert_symbols.cmake - Shared per-symbol DWARF assertion loop.
#
# Expected variables (must be set before include()):
#   DWARFDUMP            - path to llvm-dwarfdump / dwarfdump
#   DWARF_TARGET         - file or .dSYM bundle to inspect
#   HEW_BASENAME         - source filename for DW_AT_decl_file matching
#   EXPECTED_SYMBOL_LINES - comma-separated "name|line" pairs

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
