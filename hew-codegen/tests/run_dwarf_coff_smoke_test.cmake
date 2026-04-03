# run_dwarf_coff_smoke_test.cmake - Compile a .hew file targeting a Windows
# COFF triple and assert that the resulting object carries valid DWARF debug
# info (DW_AT_name, DW_AT_decl_line, and source-file attribution).
#
# This is the W-1 "COFF DWARF smoke" slice: it validates the existing
# DWARF-in-COFF path for a Windows target without requiring a Windows host,
# a Windows linker, or a CodeView/PDB toolchain.  Only symbol name,
# declaration line, and source-file presence are checked — the minimal bar for
# debuggability on the COFF path.
#
# Required variables:
#   HEW_CLI              - path to the hew compiler
#   DWARFDUMP            - path to llvm-dwarfdump / dwarfdump
#   HEW_FILE             - .hew source fixture to compile
#   OUT_OBJ              - output object file path (.obj)
#   EXPECTED_SYMBOL_LINES - comma-separated "name|line" pairs
#
# Optional variables:
#   COFF_TARGET_TRIPLE   - LLVM target triple (default: x86_64-pc-windows-msvc)

foreach(REQUIRED_VAR HEW_CLI DWARFDUMP HEW_FILE OUT_OBJ EXPECTED_SYMBOL_LINES)
  if(NOT DEFINED ${REQUIRED_VAR})
    message(FATAL_ERROR "Missing required variable: ${REQUIRED_VAR}")
  endif()
endforeach()

if(NOT DEFINED COFF_TARGET_TRIPLE)
  set(COFF_TARGET_TRIPLE "x86_64-pc-windows-msvc")
endif()

file(REMOVE ${OUT_OBJ})

execute_process(
  COMMAND ${HEW_CLI} ${HEW_FILE} -o ${OUT_OBJ} --target ${COFF_TARGET_TRIPLE} --emit-obj -g
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_out
  ERROR_VARIABLE compile_err
)

if(NOT compile_result EQUAL 0)
  # If the host LLVM was not built with the X86 backend, emit a skip marker
  # rather than a hard failure.  Any CI environment with a full LLVM install
  # will exercise the full path.
  string(FIND "${compile_err}" "No available targets" no_target_idx)
  if(NOT no_target_idx EQUAL -1)
    message(STATUS "COFF_SMOKE_SKIP: LLVM X86 backend not available on this host")
    return()
  endif()
  message(FATAL_ERROR "COFF compilation failed:\n${compile_out}\n${compile_err}")
endif()

if(NOT EXISTS ${OUT_OBJ})
  message(FATAL_ERROR "Object file was not created: ${OUT_OBJ}")
endif()

file(SIZE ${OUT_OBJ} obj_size)
if(obj_size EQUAL 0)
  message(FATAL_ERROR "Object file is empty: ${OUT_OBJ}")
endif()

# Assert COFF-x86-64 format up front so an accidental ELF/Mach-O object
# (wrong target selection) does not silently pass the DWARF checks below.
execute_process(
  COMMAND ${DWARFDUMP} ${OUT_OBJ}
  RESULT_VARIABLE fmt_result
  OUTPUT_VARIABLE fmt_out
  ERROR_VARIABLE fmt_err
)
string(FIND "${fmt_out}" "COFF-x86-64" coff_fmt_idx)
if(coff_fmt_idx EQUAL -1)
  message(FATAL_ERROR
    "Object is not COFF-x86-64 — wrong target was selected or DWARF is absent.\n"
    "dwarfdump header:\n${fmt_out}\n${fmt_err}"
  )
endif()

get_filename_component(HEW_BASENAME ${HEW_FILE} NAME)
set(DWARF_TARGET ${OUT_OBJ})

# Shared per-symbol assertion loop (checks DW_AT_name, DW_AT_decl_line, file).
include(${CMAKE_CURRENT_LIST_DIR}/dwarf_assert_symbols.cmake)
