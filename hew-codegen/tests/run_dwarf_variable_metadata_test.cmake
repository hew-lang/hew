# run_dwarf_variable_metadata_test.cmake - Assert linked-binary DWARF formal-parameter metadata.
# Variables: HEW_CLI, DWARFDUMP, HEW_FILE, OUT_BIN

foreach(REQUIRED_VAR HEW_CLI DWARFDUMP HEW_FILE OUT_BIN)
  if(NOT DEFINED ${REQUIRED_VAR})
    message(FATAL_ERROR "Missing required variable: ${REQUIRED_VAR}")
  endif()
endforeach()

file(REMOVE ${OUT_BIN})
file(REMOVE_RECURSE "${OUT_BIN}.dSYM")

execute_process(
  COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_BIN} -g
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)
if(NOT compile_result EQUAL 0)
  message(FATAL_ERROR "Compilation failed (${compile_result}):\n${compile_out}\n${compile_err}")
endif()

if(NOT EXISTS ${OUT_BIN})
  message(FATAL_ERROR "Binary was not created: ${OUT_BIN}")
endif()

set(DWARF_TARGET ${OUT_BIN})
if(EXISTS "${OUT_BIN}.dSYM")
  set(DWARF_TARGET "${OUT_BIN}.dSYM")
endif()

execute_process(
  COMMAND ${DWARFDUMP} --debug-info ${DWARF_TARGET}
  RESULT_VARIABLE dwarf_result
  OUTPUT_VARIABLE dwarf_out
  ERROR_VARIABLE dwarf_err
)
if(NOT dwarf_result EQUAL 0)
  message(FATAL_ERROR "dwarfdump failed:\n${dwarf_out}\n${dwarf_err}")
endif()

get_filename_component(HEW_BASENAME ${HEW_FILE} NAME)
string(FIND "${dwarf_out}" "DW_AT_name\t(\"${HEW_BASENAME}\")" unit_name_pos)
if(unit_name_pos EQUAL -1)
  message(FATAL_ERROR "Missing Hew compile unit for ${HEW_BASENAME}:\n${dwarf_out}")
endif()

string(SUBSTRING "${dwarf_out}" ${unit_name_pos} -1 hew_unit_tail)
string(FIND "${hew_unit_tail}" "Compile Unit:" next_cu_pos)
if(next_cu_pos EQUAL -1)
  set(hew_unit_dump "${hew_unit_tail}")
else()
  string(SUBSTRING "${hew_unit_tail}" 0 ${next_cu_pos} hew_unit_dump)
endif()

foreach(REQUIRED_NAME "Pair.sum" "add" "p" "a" "b")
  string(REGEX MATCH "DW_AT_name[^\n]*\\(\"${REQUIRED_NAME}\"\\)" name_match "${hew_unit_dump}")
  if("${name_match}" STREQUAL "")
    message(FATAL_ERROR "Missing ${REQUIRED_NAME} in Hew DWARF unit:\n${hew_unit_dump}")
  endif()
endforeach()

string(REGEX MATCHALL "DW_TAG_formal_parameter" param_tags "${hew_unit_dump}")
list(LENGTH param_tags param_count)
if(NOT param_count EQUAL 3)
  message(FATAL_ERROR "Expected 3 formal parameters in Hew DWARF unit, found ${param_count}.\n${hew_unit_dump}")
endif()

string(REGEX MATCHALL "DW_AT_location" param_locations "${hew_unit_dump}")
list(LENGTH param_locations location_count)
if(location_count LESS 3)
  message(FATAL_ERROR
    "Expected at least 3 parameter locations in Hew DWARF unit, found ${location_count}.\n${hew_unit_dump}"
  )
endif()
