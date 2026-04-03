# run_dwarf_smoke_test.cmake - Compile a .hew file to an object and assert DWARF lines
# Variables: HEW_CLI, DWARFDUMP, HEW_FILE, OUT_OBJ, EXPECTED_SYMBOL_LINES

foreach(REQUIRED_VAR HEW_CLI DWARFDUMP HEW_FILE OUT_OBJ EXPECTED_SYMBOL_LINES)
  if(NOT DEFINED ${REQUIRED_VAR})
    message(FATAL_ERROR "Missing required variable: ${REQUIRED_VAR}")
  endif()
endforeach()

file(REMOVE ${OUT_OBJ})

execute_process(
  COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_OBJ} -g --emit-obj
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)
if(NOT compile_result EQUAL 0)
  message(FATAL_ERROR "Compilation failed:\n${compile_out}\n${compile_err}")
endif()

if(NOT EXISTS ${OUT_OBJ})
  message(FATAL_ERROR "Object file was not created: ${OUT_OBJ}")
endif()

file(SIZE ${OUT_OBJ} obj_size)
if(obj_size EQUAL 0)
  message(FATAL_ERROR "Object file is empty: ${OUT_OBJ}")
endif()

get_filename_component(HEW_BASENAME ${HEW_FILE} NAME)
set(DWARF_TARGET ${OUT_OBJ})

# Shared per-symbol assertion loop (checks DW_AT_name, DW_AT_decl_line, file).
include(${CMAKE_CURRENT_LIST_DIR}/dwarf_assert_symbols.cmake)
