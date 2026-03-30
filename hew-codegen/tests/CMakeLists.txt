# ── Shared MLIR library (used by tests in non-static builds) ───────────────
if(NOT HEW_STATIC_LINK)
  find_library(MLIR_SHARED_LIB MLIR PATHS ${LLVM_LIBRARY_DIR} NO_DEFAULT_PATH)
endif()

# ── MLIR dialect test ──────────────────────────────────────────────────────
add_executable(test_mlir_dialect test_mlir_dialect.cpp)

target_include_directories(test_mlir_dialect PRIVATE
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

target_link_libraries(test_mlir_dialect PRIVATE
  HewMLIRDialect
  $<$<NOT:$<BOOL:${HEW_STATIC_LINK}>>:${MLIR_SHARED_LIB}>
)

add_test(NAME mlir_dialect COMMAND test_mlir_dialect)

# ── MLIRGen test ──────────────────────────────────────────────────────────
add_executable(test_mlirgen
  test_mlirgen.cpp
)

target_include_directories(test_mlirgen PRIVATE
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

target_link_libraries(test_mlirgen PRIVATE
  HewMLIRDialect
  $<$<NOT:$<BOOL:${HEW_STATIC_LINK}>>:${MLIR_SHARED_LIB}>
)

add_test(NAME mlirgen COMMAND test_mlirgen)
if(HEW_CLI)
  set_tests_properties(mlirgen PROPERTIES ENVIRONMENT "HEW_CLI=${HEW_CLI}")
endif()

# ── Translation test (isolates translateModuleToLLVMIR hang) ──────────────
add_executable(test_translate test_translate.cpp)

target_include_directories(test_translate PRIVATE
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

target_link_libraries(test_translate PRIVATE
  HewCodegen
  HewMLIRDialect
  $<$<NOT:$<BOOL:${HEW_STATIC_LINK}>>:${MLIR_SHARED_LIB}>
)

add_test(NAME translate COMMAND test_translate)

# ── Codegen C API test ────────────────────────────────────────────────────
add_executable(test_codegen_capi test_codegen_capi.cpp)

target_include_directories(test_codegen_capi PRIVATE
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

# Explicitly enable exceptions — HandleLLVMOptions may disable them globally
# when LLVM_ENABLE_EH is OFF, but our tests (and the CAPI they exercise) use
# try/catch for error handling.
target_compile_options(test_codegen_capi PRIVATE
  $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-fexceptions>
  $<$<CXX_COMPILER_ID:MSVC>:/EHsc>
)

target_link_libraries(test_codegen_capi PRIVATE
  HewCodegenCAPI
  $<$<NOT:$<BOOL:${HEW_STATIC_LINK}>>:${MLIR_SHARED_LIB}>
)

add_test(NAME codegen_capi COMMAND test_codegen_capi)
if(HEW_CLI)
  set_tests_properties(codegen_capi PROPERTIES ENVIRONMENT "HEW_CLI=${HEW_CLI}")
endif()

# ── Msgpack reader test ──────────────────────────────────────────────────
add_executable(test_msgpack_reader test_msgpack_reader.cpp)

target_include_directories(test_msgpack_reader PRIVATE
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

target_compile_options(test_msgpack_reader PRIVATE
  $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-fexceptions>
  $<$<CXX_COMPILER_ID:MSVC>:/EHsc>
)

target_link_libraries(test_msgpack_reader PRIVATE
  HewCodegen
  HewMLIRDialect
  $<$<NOT:$<BOOL:${HEW_STATIC_LINK}>>:${MLIR_SHARED_LIB}>
)

add_test(NAME msgpack_reader COMMAND test_msgpack_reader)

# ── End-to-end example tests (compile + run .hew files) ─────────────────────
# E2E tests use the hew CLI (Rust compiler driver) for full-pipeline compilation.
if(NOT HEW_CLI)
  message(STATUS "Skipping E2E tests — hew CLI not found")
  return()
endif()

# Platform-specific executable suffix
if(WIN32)
  set(HEW_EXE_SUFFIX ".exe")
else()
  set(HEW_EXE_SUFFIX "")
endif()

# Helper: compile a .hew file and check output matches expected
function(add_e2e_test TEST_NAME HEW_FILE EXPECTED_OUTPUT)
  add_test(
    NAME e2e_${TEST_NAME}
    COMMAND ${CMAKE_COMMAND}
      -DHEW_CLI=${HEW_CLI}
      -DHEW_FILE=${CMAKE_CURRENT_SOURCE_DIR}/examples/${HEW_FILE}
      -DEXPECTED=${EXPECTED_OUTPUT}
      -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/e2e_${TEST_NAME}${HEW_EXE_SUFFIX}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_e2e_test.cmake
  )
endfunction()

add_e2e_test(struct test_struct.hew "30\n")
add_e2e_test(struct_param test_struct_param.hew "3\n")
add_e2e_test(struct_minimal_impl test_struct_minimal_impl.hew "7\n")
add_e2e_test(struct_simple_method test_struct_simple_method.hew "7\n")
add_e2e_test(struct_methods test_struct_methods.hew "3\n4\n7\n6\n8\n")
add_e2e_test(tuple test_tuple.hew "10\n20\n30\n60\n")
add_e2e_test(tuple_mixed test_tuple_mixed.hew "42\n3.14\ntrue\n300\n")
add_e2e_test(let_destructure test_let_destructure.hew "10\n20\n100\n200\n")
add_e2e_test(array test_array.hew "10\n20\n50\n150\n")
add_e2e_test(dynamic_index e2e_array/dynamic_index.hew "10\n20\n30\n40\n50\n")
add_e2e_test(duration e2e_duration/duration.hew "100000000\n5000000000\n60000000000\n7200000000000\n")
add_e2e_test(enum_basic test_enum_basic.hew "1\n2\n3\n")
add_e2e_test(enum_match_stmt test_enum_match_stmt.hew "3\n10\n")
add_e2e_test(const test_const.hew "100\n42\n200\n3.14159\n")
add_e2e_test(type_alias test_type_alias.hew "30\n")
add_e2e_test(lambda_simple test_lambda_simple.hew "42\n")
add_e2e_test(lambda_basic test_lambda_basic.hew "42\n15\n-7\n")
add_e2e_test(lambda_hof test_lambda_hof.hew "20\n12\n")
add_e2e_test(extern test_extern.hew "42\n7\n")
add_e2e_test(enum_payload test_enum_payload.hew "42\n-1\n")
add_e2e_test(enum_payload_match test_enum_payload_match.hew "100\n-1\n10\n")
add_e2e_test(enum_wildcard test_enum_wildcard.hew "75\n16\n0\n")
add_e2e_test(match_fn_call test_match_fn_call.hew "10\n0\n200\n301\n404\n")
add_e2e_test(actor_basic test_actor_basic.hew "8\n")
add_e2e_test(actor_lambda test_actor_lambda.hew "42\n100\n")
add_e2e_test(arrow_send test_arrow_send.hew "42\n100\n")
add_e2e_test(actor_state test_actor_state.hew "60\n3\n")
add_e2e_test(actor_hashmap_ownership test_actor_hashmap_ownership.hew "true\ntrue\n")
add_e2e_test(vec_basic test_vec_basic.hew "3\n10\n20\n30\n60\n")
add_e2e_test(vec_append test_vec_append.hew "5\n1\n4\n5\n1\n99\n")
add_e2e_test(else_if test_else_if.hew "2\n-1\n0\n")
add_e2e_test(vec_loop test_vec_loop.hew "5\n100\n40\n4\n")
add_e2e_test(hashmap_basic test_hashmap_basic.hew "3\n10\n20\n30\ntrue\nfalse\n")
add_e2e_test(early_return_in_loop test_early_return_in_loop.hew "7\n-1\n")
add_e2e_test(indirect_call test_indirect_call.hew "42\n100\n")
add_e2e_test(pattern_guard test_pattern_guard.hew "big\npositive\nnon-positive\n")
add_e2e_test(string_interpolation test_string_interpolation.hew "hello world, x is 42\n")
add_e2e_test(for_vec test_for_vec.hew "10\n20\n30\n")
add_e2e_test(trait_default test_trait_default.hew "10\n42\n")
add_e2e_test(labeled_break test_labeled_break.hew "2\n1\n")
add_e2e_test(labeled_continue test_labeled_continue.hew "15\n")
add_e2e_test(break_with_value test_break_with_value.hew "7\n")
add_e2e_test(break_in_for_vec test_break_in_for_vec.hew "42\n3\n")
add_e2e_test(break_in_for_hashmap test_break_in_for_hashmap.hew "1\n2\n")
add_e2e_test(cooperate test_cooperate.hew "before cooperate\nafter cooperate\n")
add_e2e_test(vec_from_enum_array test_vec_from_enum_array.hew "3\n1\n2\n3\n3\n60\n")

# ── Migrated E2E tests (compile + run, compare against .expected file) ───────
# Helper: compile a .hew file and check output matches .expected file
function(add_e2e_file_test TEST_NAME CATEGORY HEW_NAME)
  set(HEW_FILE ${CMAKE_CURRENT_SOURCE_DIR}/examples/${CATEGORY}/${HEW_NAME}.hew)
  set(EXPECTED_FILE ${CMAKE_CURRENT_SOURCE_DIR}/examples/${CATEGORY}/${HEW_NAME}.expected)
  add_test(
    NAME ${CATEGORY}_${HEW_NAME}
    COMMAND ${CMAKE_COMMAND}
      -DHEW_CLI=${HEW_CLI}
      -DHEW_FILE=${HEW_FILE}
      -DEXPECTED_FILE=${EXPECTED_FILE}
      -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/${CATEGORY}_${HEW_NAME}${HEW_EXE_SUFFIX}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_e2e_test_file.cmake
  )
endfunction()

# Sorted variant for tests with non-deterministic output ordering
function(add_e2e_file_test_sorted TEST_NAME CATEGORY HEW_NAME)
  set(HEW_FILE ${CMAKE_CURRENT_SOURCE_DIR}/examples/${CATEGORY}/${HEW_NAME}.hew)
  set(EXPECTED_FILE ${CMAKE_CURRENT_SOURCE_DIR}/examples/${CATEGORY}/${HEW_NAME}.expected)
  add_test(
    NAME ${CATEGORY}_${HEW_NAME}
    COMMAND ${CMAKE_COMMAND}
      -DHEW_CLI=${HEW_CLI}
      -DHEW_FILE=${HEW_FILE}
      -DEXPECTED_FILE=${EXPECTED_FILE}
      -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/${CATEGORY}_${HEW_NAME}${HEW_EXE_SUFFIX}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_e2e_test_sorted.cmake
  )
endfunction()

# Helper: verify a .hew file compiles but panics at runtime with expected stderr
function(add_e2e_panic_test TEST_NAME CATEGORY HEW_NAME EXPECTED_STDERR)
  set(HEW_FILE ${CMAKE_CURRENT_SOURCE_DIR}/examples/${CATEGORY}/${HEW_NAME}.hew)
  add_test(
    NAME panic_${CATEGORY}_${HEW_NAME}
    COMMAND ${CMAKE_COMMAND}
      -DHEW_CLI=${HEW_CLI}
      -DHEW_FILE=${HEW_FILE}
      -DEXPECTED_STDERR=${EXPECTED_STDERR}
      -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/panic_${CATEGORY}_${HEW_NAME}${HEW_EXE_SUFFIX}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_e2e_panic_test.cmake
  )
endfunction()

# Helper: verify a .hew file FAILS to compile with an expected error message
function(add_e2e_reject_test TEST_NAME CATEGORY HEW_NAME EXPECTED_ERROR)
  set(HEW_FILE ${CMAKE_CURRENT_SOURCE_DIR}/examples/${CATEGORY}/${HEW_NAME}.hew)
  add_test(
    NAME reject_${CATEGORY}_${HEW_NAME}
    COMMAND ${CMAKE_COMMAND}
      -DHEW_CLI=${HEW_CLI}
      -DHEW_FILE=${HEW_FILE}
      -DEXPECTED_ERROR=${EXPECTED_ERROR}
      -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/reject_${CATEGORY}_${HEW_NAME}${HEW_EXE_SUFFIX}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_reject_test.cmake
  )
endfunction()

# Helper: verify a .hew file SUCCEEDS compilation (with a warning), emits the expected
# warning text, then runs and matches .expected output.
function(add_e2e_warn_test TEST_NAME CATEGORY HEW_NAME EXPECTED_WARNING)
  set(HEW_FILE ${CMAKE_CURRENT_SOURCE_DIR}/examples/${CATEGORY}/${HEW_NAME}.hew)
  set(EXPECTED_FILE ${CMAKE_CURRENT_SOURCE_DIR}/examples/${CATEGORY}/${HEW_NAME}.expected)
  add_test(
    NAME warn_${CATEGORY}_${HEW_NAME}
    COMMAND ${CMAKE_COMMAND}
      -DHEW_CLI=${HEW_CLI}
      -DHEW_FILE=${HEW_FILE}
      -DEXPECTED_WARNING=${EXPECTED_WARNING}
      -DEXPECTED_FILE=${EXPECTED_FILE}
      -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/warn_${CATEGORY}_${HEW_NAME}${HEW_EXE_SUFFIX}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_warn_test.cmake
  )
endfunction()

# ── Negative tests (verify compile-time error detection) ─────────────────────
add_e2e_reject_test(undefined_var        e2e_negative undefined_var        "undefined variable")
add_e2e_reject_test(type_mismatch_fn_arg e2e_negative type_mismatch_fn_arg "type mismatch")
add_e2e_reject_test(mutability_assign_let e2e_negative mutability_assign_let "cannot assign to immutable")
add_e2e_reject_test(return_type_wrong    e2e_negative return_type_wrong    "type mismatch")
add_e2e_reject_test(arity_mismatch       e2e_negative arity_mismatch       "takes 2 argument(s) but 1")
add_e2e_reject_test(self_keyword_reject  e2e_negative self_keyword_reject  "not a valid parameter name")
add_e2e_reject_test(self_typed_param_reject e2e_negative self_typed_param_reject "not a valid parameter name")
add_e2e_reject_test(self_expr_reject     e2e_negative self_expr_reject     "not a valid identifier")
add_e2e_reject_test(self_let_binding_reject e2e_negative self_let_binding_reject "not a valid identifier")
add_e2e_reject_test(self_return_reject   e2e_negative self_return_reject   "not a valid identifier")
add_e2e_reject_test(actor_field_unknown  e2e_negative actor_field_unknown  "undefined variable")
add_e2e_reject_test(machine_one_state    e2e_negative machine_one_state    "must declare at least 2 states")
add_e2e_reject_test(machine_no_events    e2e_negative machine_no_events    "must declare at least 1 event")
add_e2e_reject_test(machine_unknown_event e2e_negative machine_unknown_event "references unknown event")
add_e2e_reject_test(machine_unknown_state e2e_negative machine_unknown_state "references unknown state")
add_e2e_reject_test(machine_dup_wildcard e2e_negative machine_dup_wildcard "duplicate wildcard transition")
add_e2e_reject_test(machine_dup_transition e2e_negative machine_dup_transition "duplicate transition for event")
add_e2e_reject_test(bounds_not_satisfied e2e_negative bounds_not_satisfied "does not implement trait")
add_e2e_reject_test(shadowing_same_scope  e2e_negative shadowing_same_scope  "already defined in this scope")
add_e2e_reject_test(shadowing_actor_field e2e_negative shadowing_actor_field "shadows a binding in an outer scope")
add_e2e_reject_test(channel_unsupported_type e2e_negative channel_unsupported_type "is not supported")
add_e2e_reject_test(extern_infer_param e2e_negative extern_infer_param "cannot resolve parameter type in extern function")
add_e2e_reject_test(stream_int_reject    e2e_negative stream_int_reject    "Stream<T> is currently only implemented for String and bytes")
add_e2e_reject_test(stream_named_type    e2e_negative stream_named_type    "Stream<T> is currently only implemented for String and bytes")
add_e2e_reject_test(stream_bytes_lines_reject e2e_negative stream_bytes_lines_reject "lines()` is only supported on `Stream<String>`")

# ── Runtime panic tests (compile succeeds, runtime panics cleanly) ────────────
add_e2e_panic_test(panic_msg e2e_panic panic_msg "something went wrong")

# ── WASM target tests (compile with --target=wasm32-wasi, run with wasmtime) ──
# These validate that Hew programs produce identical output on native and WASM.
find_program(WASMTIME wasmtime)
if(WASMTIME)
  message(STATUS "WASM tests enabled — wasmtime found at ${WASMTIME}")
else()
  message(STATUS "WASM tests disabled — wasmtime not found")
endif()

# Helper: compile a .hew file for WASM and check output matches .expected file
function(add_wasm_file_test TEST_NAME CATEGORY HEW_NAME)
  if(NOT WASMTIME)
    return()
  endif()
  set(HEW_FILE ${CMAKE_CURRENT_SOURCE_DIR}/examples/${CATEGORY}/${HEW_NAME}.hew)
  set(EXPECTED_FILE ${CMAKE_CURRENT_SOURCE_DIR}/examples/${CATEGORY}/${HEW_NAME}.expected)
  add_test(
    NAME wasm_${CATEGORY}_${HEW_NAME}
    COMMAND ${CMAKE_COMMAND}
      -DHEW_CLI=${HEW_CLI}
      -DHEW_FILE=${HEW_FILE}
      -DEXPECTED_FILE=${EXPECTED_FILE}
      -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/wasm_${CATEGORY}_${HEW_NAME}.wasm
      -DWASMTIME=${WASMTIME}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_e2e_test_wasm.cmake
  )
  set_tests_properties(wasm_${CATEGORY}_${HEW_NAME} PROPERTIES
    LABELS "wasm"
  )
endfunction()

# Helper: WASM test for legacy inline-expected tests
function(add_wasm_test TEST_NAME HEW_FILE EXPECTED_OUTPUT)
  if(NOT WASMTIME)
    return()
  endif()
  add_test(
    NAME wasm_e2e_${TEST_NAME}
    COMMAND ${CMAKE_COMMAND}
      -DHEW_CLI=${HEW_CLI}
      -DHEW_FILE=${CMAKE_CURRENT_SOURCE_DIR}/examples/${HEW_FILE}
      -DEXPECTED=${EXPECTED_OUTPUT}
      -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/wasm_e2e_${TEST_NAME}.wasm
      -DWASMTIME=${WASMTIME}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_e2e_test_wasm_inline.cmake
  )
  set_tests_properties(wasm_e2e_${TEST_NAME} PROPERTIES
    LABELS "wasm"
  )
endfunction()

# Helper: WASM reject test — verify a .hew file FAILS to compile for WASM
function(add_wasm_reject_test TEST_NAME CATEGORY HEW_NAME EXPECTED_ERROR)
  if(NOT WASMTIME)
    return()
  endif()
  set(HEW_FILE ${CMAKE_CURRENT_SOURCE_DIR}/examples/${CATEGORY}/${HEW_NAME}.hew)
  add_test(
    NAME wasm_reject_${TEST_NAME}
    COMMAND ${CMAKE_COMMAND}
      -DHEW_CLI=${HEW_CLI}
      -DHEW_FILE=${HEW_FILE}
      -DEXPECTED_ERROR=${EXPECTED_ERROR}
      -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/wasm_reject_${TEST_NAME}.wasm
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_wasm_reject_test.cmake
  )
  set_tests_properties(wasm_reject_${TEST_NAME} PROPERTIES
    LABELS "wasm"
  )
endfunction()

# ── Auto-discovered E2E file tests ────────────────────────────────────────────
# Glob all e2e_*/*.hew files with matching .expected files and register them
# automatically.  Tests that need special runners (sorted output, reject,
# panic, warn) or WASM-only tests are excluded and registered manually below.
#
# To add a new E2E test: create <category>/<name>.hew + <name>.expected in
# examples/ — no CMakeLists.txt editing needed.

set(_E2E_MANUAL_EXCLUSIONS
  # Sorted-output tests (use run_e2e_test_sorted.cmake)
  "e2e_concurrency/scheduler_round_robin"
  "e2e_concurrency/scope_structured"
  "e2e_hashmap_forin/hashmap_forin"
  # Warn test (uses run_e2e_warn_test.cmake)
  "e2e_coverage/shadowing_nested_warns"
  # Legacy inline tests kept for backward compatibility (also auto-discovered
  # as file-based tests under their e2e_* category)
  "e2e_array/dynamic_index"
  "e2e_duration/duration"
  # Broken .expected files — test exists but output doesn't match
  "e2e_hashset/hashset_no_double_eval"
  "e2e_memory/rc_basic"
  "e2e_multi_dyn/multi_dyn"
  "e2e_trait_bounds/trait_bounds"
)

file(GLOB _E2E_CATEGORIES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/examples
     examples/e2e_*)
foreach(_cat ${_E2E_CATEGORIES})
  file(GLOB _hew_files
       ${CMAKE_CURRENT_SOURCE_DIR}/examples/${_cat}/*.hew)
  foreach(_hew_path ${_hew_files})
    get_filename_component(_test_base ${_hew_path} NAME_WE)
    # Skip WASM-only test files (registered separately in the WASM section)
    string(FIND "${_test_base}" "wasm_" _wasm_pos)
    if(_wasm_pos EQUAL 0)
      continue()
    endif()
    # Skip tests that require manual registration
    set(_rel_key "${_cat}/${_test_base}")
    if("${_rel_key}" IN_LIST _E2E_MANUAL_EXCLUSIONS)
      continue()
    endif()
    # Only register if a matching .expected file exists
    set(_expected ${CMAKE_CURRENT_SOURCE_DIR}/examples/${_cat}/${_test_base}.expected)
    if(EXISTS ${_expected})
      add_e2e_file_test(${_cat}_${_test_base} ${_cat} ${_test_base})
    endif()
  endforeach()
endforeach()

# ── Sorted-output E2E tests (non-deterministic output ordering) ───────────────
add_e2e_file_test_sorted(scheduler_rr        e2e_concurrency scheduler_round_robin)
add_e2e_file_test_sorted(scope_structured    e2e_concurrency scope_structured)
add_e2e_file_test_sorted(hashmap_forin       e2e_hashmap_forin hashmap_forin)


# ── Reject tests in non-negative categories ───────────────────────────────────
add_e2e_reject_test(machine_exhaustive e2e_machine machine_exhaustive_fail "does not handle event")
add_e2e_reject_test(module_private     e2e_modules module_private_reject   "no function")

# ── Warn tests (compile succeeds with expected warning, then run + compare) ───
add_e2e_warn_test(coverage_shadowing_nested e2e_coverage shadowing_nested_warns "shadows a binding in an outer scope")

# ── Panic tests in coverage category ──────────────────────────────────────────
add_e2e_panic_test(testing_assert_fail e2e_coverage testing_assert_fail "assertion failed: expected 2, got 1")

# ── Special test properties ───────────────────────────────────────────────────
# Force single scheduler worker so mailbox fill/drop is deterministic
set_tests_properties(e2e_examples_backpressure_test PROPERTIES ENVIRONMENT "HEW_WORKERS=1")

# Stress tests spawn many worker threads and are sensitive to extreme CPU/memory
# contention from parallel test execution.  Serialize them against each other and
# reserve enough ctest job slots so the OS scheduler isn't overwhelmed.
set_tests_properties(
  e2e_stress_stress_mass_spawn
  e2e_stress_stress_ring
  e2e_stress_stress_message_burst
  PROPERTIES
    RESOURCE_LOCK actor_crash_tests
    PROCESSORS   4
)

# Supervisor tests intentionally crash and restart actors — sensitive to
# CPU contention from parallel test execution.
set_tests_properties(
  e2e_supervisor_supervisor_basic
  e2e_supervisor_supervisor_strategy
  e2e_supervisor_supervisor_child
  e2e_supervisor_supervisor_restart
  e2e_supervisor_supervisor_nested
  e2e_supervisor_supervisor_named_child
  PROPERTIES
    RESOURCE_LOCK actor_crash_tests
    PROCESSORS   2
)

# ── Learning/playground example tests (auto-discovered) ─────────────────────
# Any .hew file in examples/playground/ or examples/junior/ with a matching
# .expected file is automatically registered as a test. Adding a new example +
# .expected file creates a test automatically — no CMakeLists.txt editing needed.
# examples/junior/12_errors.hew is intentionally excluded (compile-failure demo).

set(EXAMPLES_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/../../examples")
cmake_path(ABSOLUTE_PATH EXAMPLES_ROOT NORMALIZE)

# Helper: register a test from absolute .hew and .expected paths
function(add_example_test TEST_NAME HEW_FILE_ABS EXPECTED_FILE_ABS)
  add_test(
    NAME ${TEST_NAME}
    COMMAND ${CMAKE_COMMAND}
      -DHEW_CLI=${HEW_CLI}
      -DHEW_FILE=${HEW_FILE_ABS}
      -DEXPECTED_FILE=${EXPECTED_FILE_ABS}
      -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/${TEST_NAME}${HEW_EXE_SUFFIX}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/run_e2e_test_file.cmake
  )
endfunction()

# Playground examples (subcategories: basics, concurrency, types)
file(GLOB_RECURSE PLAYGROUND_HEWS "${EXAMPLES_ROOT}/playground/*.hew")
foreach(HEW_FILE ${PLAYGROUND_HEWS})
  get_filename_component(HEW_NAME ${HEW_FILE} NAME_WE)
  get_filename_component(HEW_DIR ${HEW_FILE} DIRECTORY)
  get_filename_component(CATEGORY ${HEW_DIR} NAME)

  set(EXPECTED_FILE "${HEW_DIR}/${HEW_NAME}.expected")
  if(EXISTS ${EXPECTED_FILE})
    set(TEST_NAME "playground_${CATEGORY}_${HEW_NAME}")
    add_example_test(${TEST_NAME} ${HEW_FILE} ${EXPECTED_FILE})
  else()
    message(VERBOSE "Skipping playground/${CATEGORY}/${HEW_NAME} — no .expected file")
  endif()
endforeach()

# Force single scheduler worker for multi-actor tests (non-deterministic ordering)
set_tests_properties(playground_concurrency_supervisor PROPERTIES ENVIRONMENT "HEW_WORKERS=1")

# Junior examples
file(GLOB JUNIOR_HEWS "${EXAMPLES_ROOT}/junior/*.hew")
foreach(HEW_FILE ${JUNIOR_HEWS})
  get_filename_component(HEW_NAME ${HEW_FILE} NAME_WE)

  set(EXPECTED_FILE "${EXAMPLES_ROOT}/junior/${HEW_NAME}.expected")
  if(EXISTS ${EXPECTED_FILE})
    set(TEST_NAME "junior_${HEW_NAME}")
    add_example_test(${TEST_NAME} ${HEW_FILE} ${EXPECTED_FILE})
  else()
    message(VERBOSE "Skipping junior/${HEW_NAME} — no .expected file")
  endif()
endforeach()
# ── Coroutine prototype tests ────────────────────────────────────────────────
# Validates LLVM coroutine intrinsic integration (CoroSplit pass).
# These compile raw .ll files with coroutine intrinsics and verify output.
add_test(
  NAME coro_generator
  COMMAND ${CMAKE_COMMAND}
    -DLL_FILE=${CMAKE_CURRENT_SOURCE_DIR}/test_coro_generator.ll
    -DEXPECTED=1\n2\n3\ndone\n
    -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/test_coro_generator${HEW_EXE_SUFFIX}
    -P ${CMAKE_CURRENT_SOURCE_DIR}/run_coro_test.cmake
)

add_test(
  NAME coro_fib_generator
  COMMAND ${CMAKE_COMMAND}
    -DLL_FILE=${CMAKE_CURRENT_SOURCE_DIR}/test_coro_fib_generator.ll
    -DEXPECTED=0\n1\n1\n2\n3\n5\n8\n13\n
    -DOUT_BIN=${CMAKE_CURRENT_BINARY_DIR}/test_coro_fib_generator${HEW_EXE_SUFFIX}
    -P ${CMAKE_CURRENT_SOURCE_DIR}/run_coro_test.cmake
)

# ── WASM target tests ─────────────────────────────────────────────────────────
# Mirror of native E2E tests compiled for wasm32-wasi and run with wasmtime.
# WASM tests: covers all features that compile and run under wasm32-wasi.
# Excluded: supervision, scopes, select, linking (require OS threads), and
# ecosystem FFI that is not linked into the WASM runtime (regex, json/yaml
# serialization, crypto, sort, file I/O, networking).

# Core language features
add_wasm_test(struct test_struct.hew "30\n")
add_wasm_test(struct_param test_struct_param.hew "3\n")
add_wasm_test(struct_minimal_impl test_struct_minimal_impl.hew "7\n")
add_wasm_test(struct_simple_method test_struct_simple_method.hew "7\n")
add_wasm_test(struct_methods test_struct_methods.hew "3\n4\n7\n6\n8\n")
add_wasm_test(tuple test_tuple.hew "10\n20\n30\n60\n")
add_wasm_test(tuple_mixed test_tuple_mixed.hew "42\n3.14\ntrue\n300\n")
add_wasm_test(let_destructure test_let_destructure.hew "10\n20\n100\n200\n")
add_wasm_test(array test_array.hew "10\n20\n50\n150\n")
add_wasm_test(enum_basic test_enum_basic.hew "1\n2\n3\n")
add_wasm_test(enum_match_stmt test_enum_match_stmt.hew "3\n10\n")
add_wasm_test(enum_payload test_enum_payload.hew "42\n-1\n")
add_wasm_test(enum_payload_match test_enum_payload_match.hew "100\n-1\n10\n")
add_wasm_test(enum_wildcard test_enum_wildcard.hew "75\n16\n0\n")
add_wasm_test(const test_const.hew "100\n42\n200\n3.14159\n")
add_wasm_test(type_alias test_type_alias.hew "30\n")
add_wasm_test(vec_basic test_vec_basic.hew "3\n10\n20\n30\n60\n")
add_wasm_test(vec_append test_vec_append.hew "5\n1\n4\n5\n1\n99\n")
add_wasm_test(vec_loop test_vec_loop.hew "5\n100\n40\n4\n")
add_wasm_test(for_vec test_for_vec.hew "10\n20\n30\n")
add_wasm_test(hashmap_basic test_hashmap_basic.hew "3\n10\n20\n30\ntrue\nfalse\n")
add_wasm_test(lambda_simple test_lambda_simple.hew "42\n")
add_wasm_test(lambda_basic test_lambda_basic.hew "42\n15\n-7\n")
add_wasm_test(lambda_hof test_lambda_hof.hew "20\n12\n")
add_wasm_test(indirect_call test_indirect_call.hew "42\n100\n")
add_wasm_test(else_if test_else_if.hew "2\n-1\n0\n")
add_wasm_test(early_return_in_loop test_early_return_in_loop.hew "7\n-1\n")
add_wasm_test(pattern_guard test_pattern_guard.hew "big\npositive\nnon-positive\n")
add_wasm_test(string_interpolation test_string_interpolation.hew "hello world, x is 42\n")
add_wasm_test(trait_default test_trait_default.hew "10\n42\n")
add_wasm_test(labeled_break test_labeled_break.hew "2\n1\n")
add_wasm_test(labeled_continue test_labeled_continue.hew "15\n")
add_wasm_test(break_with_value test_break_with_value.hew "7\n")

# File-based WASM tests (category/name with .expected file)
add_wasm_file_test(type_aliases         e2e_type_aliases type_aliases)
add_wasm_file_test(type_alias_signatures e2e_type_aliases type_alias_signatures)
add_wasm_file_test(type_alias_tuple    e2e_type_aliases type_alias_tuple)
add_wasm_file_test(bitwise_ops          e2e_bitwise bitwise_ops)
add_wasm_file_test(hex_literals         e2e_hex_literals hex_literals)
add_wasm_file_test(array_repeat         e2e_array array_repeat)
add_wasm_file_test(dynamic_index        e2e_array dynamic_index)
add_wasm_file_test(duration             e2e_duration duration)
add_wasm_file_test(duration_methods    e2e_duration duration_methods)
add_wasm_file_test(duration_arithmetic e2e_duration duration_arithmetic)
add_wasm_file_test(duration_from_nanos e2e_duration duration_from_nanos)
add_wasm_file_test(duration_suffixes   e2e_duration duration_suffixes)

# Algorithms (all pure computation — ideal WASM tests)
add_wasm_file_test(binary_search        e2e_algorithms binary_search)
add_wasm_file_test(fibonacci            e2e_algorithms fibonacci)
add_wasm_file_test(fizzbuzz             e2e_algorithms fizzbuzz)
add_wasm_file_test(gcd_lcm              e2e_algorithms gcd_lcm)
add_wasm_file_test(lcs                  e2e_algorithms longest_common_subsequence)
add_wasm_file_test(matrix_multiply      e2e_algorithms matrix_multiply)
add_wasm_file_test(merge_sort           e2e_algorithms merge_sort)
add_wasm_file_test(palindrome           e2e_algorithms palindrome_check)
add_wasm_file_test(roman_to_integer     e2e_algorithms roman_to_integer)
add_wasm_file_test(sieve                e2e_algorithms sieve_of_eratosthenes)
add_wasm_file_test(two_sum              e2e_algorithms two_sum)
add_wasm_file_test(valid_parens         e2e_algorithms valid_parentheses)

# Data structures
add_wasm_file_test(enum_state_machine   e2e_data_structures enum_state_machine)
add_wasm_file_test(hashmap_frequency    e2e_data_structures hashmap_frequency)
add_wasm_file_test(hashset_basic        e2e_hashset         hashset_basic)
add_wasm_file_test(hashset_strings      e2e_hashset         hashset_strings)
add_wasm_file_test(option_result        e2e_data_structures option_result_chain)
add_wasm_file_test(queue_via_stacks     e2e_data_structures queue_via_stacks)
add_wasm_file_test(stack_operations     e2e_data_structures stack_operations)
add_wasm_file_test(string_builder       e2e_data_structures string_builder)
add_wasm_file_test(struct_methods_ds    e2e_data_structures struct_methods)
add_wasm_file_test(match_or_pattern     e2e_data_structures match_or_pattern)
add_wasm_file_test(match_struct_pat     e2e_data_structures match_struct_pattern)
add_wasm_file_test(vec_operations       e2e_data_structures vec_operations)
add_wasm_file_test(vec_f64_ops          e2e_data_structures vec_f64_ops)

# Generics
add_wasm_file_test(generic_identity     e2e_generics generic_identity)
add_wasm_file_test(generic_pair         e2e_generics generic_pair)
add_wasm_file_test(generic_fn           e2e_generics generic_fn)
add_wasm_file_test(generic_struct       e2e_generics generic_struct)
add_wasm_file_test(generic_multi_spec   e2e_generics generic_multi_specialize)
add_wasm_file_test(generic_multi_param  e2e_generics generic_multi_param)
add_wasm_file_test(generic_struct_param e2e_generics generic_struct_param)
add_wasm_file_test(generic_all_types    e2e_generics generic_all_types)
add_wasm_file_test(generic_nested       e2e_generics generic_nested)

# Traits and impl
add_wasm_file_test(trait_impl           e2e_traits trait_impl)
add_wasm_file_test(trait_multi_impl     e2e_traits trait_multi_impl)
add_wasm_file_test(struct_impl_method   e2e_impl struct_impl_method)
add_wasm_file_test(dyn_dispatch         e2e_dyn_trait dyn_dispatch)
add_wasm_file_test(dyn_dispatch_string  e2e_dyn_trait dyn_dispatch_string)
add_wasm_file_test(dyn_dispatch_args    e2e_dyn_trait dyn_dispatch_args)
add_wasm_file_test(dyn_dispatch_void    e2e_dyn_trait dyn_dispatch_void)
add_wasm_file_test(trait_inherit        e2e_trait_inherit trait_inherit)

# Closures
add_wasm_file_test(closure_firstclass   e2e_closures closure_firstclass)
add_wasm_file_test(closure_env_rc       e2e_closures closure_env_rc)
add_wasm_file_test(closure_mutable      e2e_closures closure_mutable_capture)
add_wasm_file_test(closure_read_mutable e2e_closures closure_read_mutable)
add_wasm_file_test(closure_match_capture e2e_closures closure_match_capture)
add_wasm_file_test(move_closure         e2e_closures move_closure)

# Struct/tuple features
add_wasm_file_test(struct_assign        e2e_structs struct_assign)
add_wasm_file_test(tuple_destruct       e2e_tuples tuple_destruct)

# Error handling
add_wasm_file_test(error_return_codes   e2e_error_handling error_return_codes)
add_wasm_file_test(result_pattern       e2e_error_handling result_pattern)
add_wasm_file_test(option_pattern       e2e_error_handling option_pattern)
add_wasm_file_test(enum_error_chain     e2e_error_handling enum_error_chain)
add_wasm_file_test(try_operator         e2e_try_operator try_operator)

# Loops and defer
add_wasm_file_test(for_range_break      e2e_loops for_range_break)
add_wasm_file_test(loop_return          e2e_loops loop_return)
add_wasm_file_test(defer                e2e_defer defer)
add_wasm_file_test(defer_multi_return   e2e_defer defer_multi_return)
add_wasm_file_test(defer_cleanup_call   e2e_defer defer_cleanup_call)

# Const
add_wasm_file_test(const_expressions    e2e_const const_expressions)

# Canonicalization
add_wasm_file_test(string_concat_id     e2e_canonicalize string_concat_identity)
add_wasm_file_test(cast_chain           e2e_canonicalize cast_chain)

# Drop/cleanup
add_wasm_file_test(drop_trait           e2e_drop_trait drop_trait)
add_wasm_file_test(drop_scope           e2e_drop_scope drop_scope)
add_wasm_file_test(string_drop          e2e_strings string_drop)

# Named args, coverage
add_wasm_file_test(named_args           e2e_named_args main)
add_wasm_file_test(cast_numeric         e2e_coverage cast_numeric)
add_wasm_file_test(struct_field_set     e2e_coverage struct_field_set)

# Example programs (non-actor subset)
add_wasm_file_test(ex_fibonacci         e2e_examples fibonacci)
add_wasm_file_test(ex_basic_types       e2e_examples test_basic_types)
add_wasm_file_test(ex_block             e2e_examples test_block)
add_wasm_file_test(ex_enum              e2e_examples enum_test)
add_wasm_file_test(ex_string_ops        e2e_examples string_ops_test)
add_wasm_file_test(ex_comprehensive     e2e_examples comprehensive_test_simple)
add_wasm_file_test(ex_vec_hashmap       e2e_examples vec_hashmap_test)
add_wasm_file_test(ex_result_option     e2e_examples result_option_test)
add_wasm_file_test(ex_enums_options     e2e_examples enums_and_options)
add_wasm_file_test(ex_string            e2e_examples string_test)
add_wasm_file_test(ex_stress_match      e2e_examples stress_match)
add_wasm_file_test(ex_full_validation   e2e_examples full_validation)
add_wasm_file_test(ex_vec_set_ops       e2e_examples vec_set_operations)
add_wasm_file_test(ex_for_loop_patterns e2e_examples for_loop_patterns)
add_wasm_file_test(ex_type_inference    e2e_examples type_inference)
add_wasm_file_test(ex_operators         e2e_examples test_operators_comprehensive)
add_wasm_file_test(ex_vec_complete      e2e_examples test_vec_complete)
add_wasm_file_test(ex_hashmap_complete  e2e_examples test_hashmap_complete)
add_wasm_file_test(ex_float_arith       e2e_examples test_float_arithmetic)
add_wasm_file_test(ex_loop_patterns     e2e_examples test_loop_patterns)
add_wasm_file_test(ex_nested_structs    e2e_examples test_nested_structs)
add_wasm_file_test(ex_string_edges      e2e_examples test_string_edge_cases)
add_wasm_file_test(ex_match_patterns    e2e_examples test_match_patterns)
add_wasm_file_test(ex_lambda_patterns   e2e_examples test_lambda_patterns)

# Generators (unlocked by the coro malloc/size_t fix for wasm32)
add_wasm_file_test(simple_generator       e2e_generators       simple_generator)
add_wasm_file_test(gen_fib                e2e_generators       gen_fib)
add_wasm_file_test(gen_loop               e2e_generators       gen_loop)
add_wasm_file_test(for_over_generator     e2e_generators       for_over_generator)
add_wasm_file_test(async_gen              e2e_async_gen        async_gen)
add_wasm_file_test(golden_generator       e2e_golden           golden_generator)

# State machines
add_wasm_file_test(machine_basic          e2e_machine          machine_basic)
add_wasm_file_test(machine_circuit_br     e2e_machine          machine_circuit_breaker)
add_wasm_file_test(machine_counter        e2e_machine          machine_counter)
add_wasm_file_test(machine_event_pay      e2e_machine          machine_event_payload)
add_wasm_file_test(machine_guard          e2e_machine          machine_guard)
add_wasm_file_test(machine_match          e2e_machine          machine_match)
add_wasm_file_test(machine_payload_tr     e2e_machine          machine_payload_transition)
add_wasm_file_test(machine_qualified      e2e_machine          machine_qualified_event)
add_wasm_file_test(machine_self_trans     e2e_machine          machine_self_transition)
add_wasm_file_test(machine_systematic     e2e_machine          machine_systematic)
add_wasm_file_test(machine_traffic        e2e_machine          machine_traffic)
add_wasm_file_test(machine_wildcard       e2e_machine          machine_wildcard_ordering)

# Modules
add_wasm_file_test(module_actor           e2e_modules          module_actor)
add_wasm_file_test(module_alias           e2e_modules          module_alias)
add_wasm_file_test(module_basic           e2e_modules          module_basic)
add_wasm_file_test(module_chain           e2e_modules          module_chain)
add_wasm_file_test(module_consts          e2e_modules          module_consts)
add_wasm_file_test(module_diamond         e2e_modules          module_diamond)
add_wasm_file_test(module_enum            e2e_modules          module_enum)
add_wasm_file_test(module_glob            e2e_modules          module_glob)
add_wasm_file_test(module_multi_import    e2e_modules          module_multi_import)
add_wasm_file_test(module_multifile       e2e_modules          module_multifile)
add_wasm_file_test(module_named           e2e_modules          module_named)
add_wasm_file_test(module_qualified       e2e_modules          module_qualified)
add_wasm_file_test(module_reexport        e2e_modules          module_reexport)
add_wasm_file_test(module_spawn_qualified e2e_modules          module_spawn_qualified)
add_wasm_file_test(module_trait_dispatch  e2e_modules          module_trait_dispatch)
add_wasm_file_test(module_trait_inherit   e2e_modules          module_trait_inherit)
add_wasm_file_test(module_types           e2e_modules          module_types)

# Control flow: while-let, if-let, for-labels, ranges
add_wasm_file_test(while_let_basic        e2e_while_let        while_let_basic)
add_wasm_file_test(if_let_basic           e2e_if_let           if_let_basic)
add_wasm_file_test(if_let_return          e2e_if_let           if_let_return)
add_wasm_file_test(for_labels             e2e_for_labels       for_labels)
add_wasm_file_test(ranges                 e2e_ranges           range_value)

# Pattern matching
add_wasm_file_test(match_nested_pattern         e2e_match   match_nested_pattern)
add_wasm_file_test(match_struct_variant_last_arm e2e_match   match_struct_variant_last_arm)
add_wasm_file_test(tuple_pattern                e2e_match   tuple_pattern)

# Generics (extended)
add_wasm_file_test(generic_implicit_identity  e2e_generics  generic_implicit_identity)
add_wasm_file_test(generic_implicit_multi     e2e_generics  generic_implicit_multi)
add_wasm_file_test(generic_receiver_method    e2e_generics  generic_receiver_method)
add_wasm_file_test(generic_trait_bound_dispatch e2e_generics generic_trait_bound_dispatch)
add_wasm_file_test(generic_impl_method        e2e_impl      generic_impl_method)
add_wasm_file_test(generic_impl_multi         e2e_impl      generic_impl_multi)

# Traits (extended)
add_wasm_file_test(trait_default_override     e2e_traits              trait_default_override)
add_wasm_file_test(trait_inherit_diamond      e2e_traits              trait_inherit_diamond)
add_wasm_file_test(trait_bound_method         e2e_trait_bound_dispatch trait_bound_method)
add_wasm_file_test(trait_bound_multi          e2e_trait_bound_dispatch trait_bound_multi)
add_wasm_file_test(dyn_trait_with_generics    e2e_dyn_trait           dyn_trait_with_generics)

# Closures (extended)
add_wasm_file_test(closure_return_coercion    e2e_closures closure_return_coercion)

# Self sugar
add_wasm_file_test(self_sugar                 e2e_self_sugar       self_sugar)
add_wasm_file_test(self_sugar_mixed           e2e_self_sugar       self_sugar_mixed)

# Defer (extended)
add_wasm_file_test(defer_block                e2e_defer            defer_block)

# Loops (extended)
add_wasm_file_test(labeled_break_drops        e2e_loops            labeled_break_drops)

# Collections
add_wasm_file_test(deque                      e2e_collections      deque)
add_wasm_file_test(empty_map_literal          e2e_collections      empty_map_literal)
add_wasm_file_test(empty_vec_literal          e2e_collections      empty_vec_literal)
add_wasm_file_test(hashmap_keys               e2e_collections      hashmap_keys)
add_wasm_file_test(hashmap_keys_method        e2e_collections      hashmap_keys_method)
add_wasm_file_test(map_literal                e2e_collections      map_literal)
add_wasm_file_test(vec_filter                 e2e_collections      vec_filter)
add_wasm_file_test(vec_fold                   e2e_collections      vec_fold)
add_wasm_file_test(vec_join                   e2e_collections      vec_join)
add_wasm_file_test(vec_map                    e2e_collections      vec_map)
add_wasm_file_test(hashset_stdlib             e2e_hashset          hashset_stdlib)

# Bytes
add_wasm_file_test(bytes_append               e2e_bytes            bytes_append)
add_wasm_file_test(bytes_basic                e2e_bytes            bytes_basic)
add_wasm_file_test(bytes_from                 e2e_bytes            bytes_from)
add_wasm_file_test(bytes_literal              e2e_bytes            bytes_literal)

# Vec (extended)
add_wasm_file_test(vec_bool                   e2e_vec              vec_bool)

# Indirect enums
add_wasm_file_test(indirect_expr              e2e_indirect_enum    indirect_expr)
add_wasm_file_test(indirect_list              e2e_indirect_enum    indirect_list)
add_wasm_file_test(indirect_option_tree       e2e_indirect_enum    indirect_option_tree)

# Strings (extended)
add_wasm_file_test(string_lines               e2e_strings          string_lines)
add_wasm_file_test(string_predicates          e2e_strings          string_predicates)
add_wasm_file_test(string_ordering            e2e_strings  string_ordering)

# Struct/enum (extended)
add_wasm_file_test(struct_destruct            e2e_structs  struct_destruct)
add_wasm_file_test(struct_enum                e2e_structs      struct_enum)
add_wasm_file_test(nested_tuple_destruct      e2e_tuples nested_tuple_destruct)

# Error handling (extended)
add_wasm_file_test(result_stdlib              e2e_error_handling   result_stdlib)
add_wasm_file_test(structured_errors          e2e_structured_errors structured_errors)

# Drop/memory (extended)
add_wasm_file_test(drop_early_return          e2e_drop_scope       drop_early_return)
add_wasm_file_test(drop_nested_struct         e2e_drop_trait       drop_nested_struct)
add_wasm_file_test(return_after_drop          e2e_return           return_after_drop)
add_wasm_file_test(string_lifecycle           e2e_memory           string_lifecycle)
add_wasm_file_test(large_vec                  e2e_memory           large_vec_lifecycle)
add_wasm_file_test(closure_capture            e2e_memory           closure_capture_safety)
add_wasm_file_test(deep_recursion             e2e_memory           deep_recursion)
add_wasm_file_test(actor_str_owner            e2e_memory           actor_field_string_ownership)

# Types
add_wasm_file_test(type_coverage              e2e_types            type_coverage)
add_wasm_file_test(type_dispatch_narrow       e2e_types            type_dispatch_narrow)
add_wasm_file_test(literal_coercion           e2e_literal_coercion literal_coercion)
add_wasm_file_test(char_literals              e2e_char_literals    char_literals)
add_wasm_file_test(indexed_compound           e2e_compound_assign  indexed_compound)

# Tail call, math, iterators
add_wasm_file_test(tail_call                  e2e_tail_call        tail_call)
add_wasm_file_test(math_basic                 e2e_math             math_basic)
add_wasm_file_test(iter_stdlib                e2e_iter_stdlib      iter_stdlib)
add_wasm_file_test(option_stdlib              e2e_option_stdlib    option_stdlib)

# Stdlib imports (string-only, no FS/IO/OS)
add_wasm_file_test(import_string              e2e_imports          test_import_string)
add_wasm_file_test(import_string_ops          e2e_imports          test_import_string_ops)

# Wire (subset that compiles without json/yaml FFI)
add_wasm_file_test(wire_basic                 e2e_wire             wire_basic)
add_wasm_file_test(wire_encode_decode         e2e_wire             wire_encode_decode)

# Semver, fmt, csv (pure Hew implementations)
add_wasm_file_test(semver_test                e2e_semver           semver_test)
add_wasm_file_test(fmt_test                   e2e_fmt              fmt_test)
add_wasm_file_test(csv_test                   e2e_csv              csv_test)

# Golden tests (comprehensive language feature exercises)
add_wasm_file_test(golden_closure             e2e_golden           golden_closure)
add_wasm_file_test(golden_enum                e2e_golden           golden_enum)
add_wasm_file_test(golden_hashmap             e2e_golden           golden_hashmap)
add_wasm_file_test(golden_option_result       e2e_golden           golden_option_result)
add_wasm_file_test(golden_string              e2e_golden           golden_string)
add_wasm_file_test(golden_struct              e2e_golden           golden_struct)
add_wasm_file_test(golden_vec                 e2e_golden           golden_vec)

# Coverage tests (edge cases, integration)
add_wasm_file_test(coverage_actor_ask_reply       e2e_coverage  actor_ask_reply)
add_wasm_file_test(coverage_actor_multi_handler   e2e_coverage  actor_multi_handler)
add_wasm_file_test(coverage_assert_ops            e2e_coverage  assert_ops)
add_wasm_file_test(coverage_closure_capture       e2e_coverage  closure_capture_types)
add_wasm_file_test(coverage_closure_edge_cases    e2e_coverage  closure_edge_cases)
add_wasm_file_test(coverage_dyn_param             e2e_coverage  dyn_param_dispatch)
add_wasm_file_test(coverage_enum_patterns         e2e_coverage  enum_complex_patterns)
add_wasm_file_test(coverage_enum_data             e2e_coverage  enum_data_methods)
add_wasm_file_test(coverage_expr_deep_nesting     e2e_coverage  expr_deep_nesting)
add_wasm_file_test(coverage_hashmap_option        e2e_coverage  hashmap_option_integration)
add_wasm_file_test(coverage_hashmap_string_vals   e2e_coverage  hashmap_string_vals)
add_wasm_file_test(coverage_if_let_nested         e2e_coverage  if_let_nested)
add_wasm_file_test(coverage_loop_control          e2e_coverage  loop_control_flow)
add_wasm_file_test(coverage_loop_edge_cases       e2e_coverage  loop_edge_cases)
add_wasm_file_test(coverage_match_exhaustive      e2e_coverage  match_exhaustive)
add_wasm_file_test(coverage_match_nested_enum     e2e_coverage  match_nested_enum)
add_wasm_file_test(coverage_match_tuple_guard     e2e_coverage  match_tuple_guard)
add_wasm_file_test(coverage_narrowing_check       e2e_coverage  narrowing_check)
add_wasm_file_test(coverage_nested_match          e2e_coverage  nested_match_exhaust)
add_wasm_file_test(coverage_option_chain          e2e_coverage  option_chain)
add_wasm_file_test(coverage_result_error          e2e_coverage  result_error_match)
add_wasm_file_test(coverage_result_propagation    e2e_coverage  result_propagation)
add_wasm_file_test(coverage_return_in_loop        e2e_coverage  return_in_loop)
add_wasm_file_test(coverage_shadowing_scopes      e2e_coverage  shadowing_error_recovery)
add_wasm_file_test(coverage_std_testing           e2e_coverage  std_testing)
add_wasm_file_test(coverage_string_case           e2e_coverage  string_case_methods)
add_wasm_file_test(coverage_string_interp         e2e_coverage  string_interp_complex)
add_wasm_file_test(coverage_string_methods        e2e_coverage  string_method_calls)
add_wasm_file_test(coverage_to_string             e2e_coverage  to_string_op)
add_wasm_file_test(coverage_trait_dispatch        e2e_coverage  trait_dispatch_generic)
add_wasm_file_test(coverage_var_reassign_drop     e2e_coverage  var_reassign_drop)
add_wasm_file_test(coverage_vec_functional        e2e_coverage  vec_functional_ops)
add_wasm_file_test(coverage_vec_remove_clear      e2e_coverage  vec_remove_clear)
add_wasm_file_test(coverage_vec_string_ops        e2e_coverage  vec_string_ops)
add_wasm_file_test(coverage_where_clause_gen      e2e_coverage  where_clause_generics)
add_wasm_file_test(log_basic_cov                  e2e_coverage  log_basic)

# Performance benchmarks
add_wasm_file_test(collection_bench               e2e_performance  collection_ops_bench)
add_wasm_file_test(fib_iter_bench                 e2e_performance  fibonacci_iterative_bench)
add_wasm_file_test(string_bench                   e2e_performance  string_concat_bench)

# Concurrency (cooperative — no OS threads)
add_wasm_file_test(async_sequential               e2e_concurrency  async_sequential)

# Actors (native tests that also pass under WASM single-threaded scheduler)
add_wasm_file_test(actor_counter              e2e_actors       actor_counter)
add_wasm_file_test(actor_lifecycle            e2e_actors       actor_lifecycle)
add_wasm_file_test(actor_orchestration        e2e_actors       actor_orchestration)
add_wasm_file_test(actor_pipeline             e2e_actors       actor_pipeline)
add_wasm_file_test(await_actor_ask            e2e_actors       await_actor_ask)
add_wasm_file_test(ex_actor_lifecycle         e2e_examples     test_actor_lifecycle)
add_wasm_file_test(ex_struct_spawn            e2e_examples     struct_spawn_test)
add_wasm_file_test(ex_lambda_capture          e2e_examples     lambda_capture_basic)
add_wasm_file_test(ex_showcase                e2e_examples     showcase)
add_wasm_file_test(enum_qualified_construct   e2e_enum_qualified enum_qualified_construct)

# WASM actor tests (dedicated WASM-specific test files)
add_wasm_file_test(actor_basic          e2e_actors wasm_actor_basic)
add_wasm_file_test(actor_ask            e2e_actors wasm_actor_ask)
add_wasm_file_test(actor_lambda         e2e_actors wasm_actor_lambda)
add_wasm_file_test(actor_multi          e2e_actors wasm_actor_multi)
add_wasm_file_test(actor_stop_close    e2e_actors wasm_actor_stop_close)
add_wasm_file_test(actor_ref_passing   e2e_actors wasm_actor_ref_passing)
add_wasm_file_test(actor_multi_receive e2e_actors wasm_actor_multi_receive)
add_wasm_file_test(actor_self          e2e_actors wasm_actor_self)
add_wasm_file_test(actor_capture       e2e_actors wasm_actor_capture)

# WASM reject tests (verify unsupported ops produce compile errors)
add_wasm_reject_test(supervisor  e2e_actors wasm_reject_supervisor "is not supported on WASM")
add_wasm_reject_test(scope       e2e_actors wasm_reject_scope      "is not supported on WASM")
add_wasm_reject_test(link        e2e_actors wasm_reject_link       "is not supported on WASM")
