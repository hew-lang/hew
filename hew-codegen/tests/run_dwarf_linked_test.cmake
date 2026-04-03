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

# Shared per-symbol assertion loop (checks DW_AT_name, DW_AT_decl_line, file).
include(${CMAKE_CURRENT_LIST_DIR}/dwarf_assert_symbols.cmake)
