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
  # Each E2E test invokes the full compiler pipeline (parse → MLIR → LLVM → link).
  # Serialise via RESOURCE_LOCK to prevent resource contention under parallel ctest.
  set_tests_properties(e2e_${TEST_NAME} PROPERTIES
    RESOURCE_LOCK hew_compile
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
  set_tests_properties(${CATEGORY}_${HEW_NAME} PROPERTIES
    RESOURCE_LOCK hew_compile
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
  set_tests_properties(${CATEGORY}_${HEW_NAME} PROPERTIES
    RESOURCE_LOCK hew_compile
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
  set_tests_properties(reject_${CATEGORY}_${HEW_NAME} PROPERTIES
    RESOURCE_LOCK hew_compile
  )
endfunction()

# ── Negative tests (verify compile-time error detection) ─────────────────────
add_e2e_reject_test(undefined_var  e2e_negative undefined_var "undefined variable")

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
    RESOURCE_LOCK hew_compile
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
    RESOURCE_LOCK hew_compile
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
    RESOURCE_LOCK hew_compile
    LABELS "wasm"
  )
endfunction()

# Type aliases (1 test)
add_e2e_file_test(type_aliases         e2e_type_aliases type_aliases)

# Type coverage (1 test)
add_e2e_file_test(type_coverage        e2e_types type_coverage)
add_e2e_file_test(type_dispatch_narrow e2e_types type_dispatch_narrow)

# Bitwise operators (1 test)
add_e2e_file_test(bitwise_ops          e2e_bitwise bitwise_ops)

# Hex/octal/binary literals (1 test)
add_e2e_file_test(hex_literals         e2e_hex_literals hex_literals)

# Arrays (1 test)
add_e2e_file_test(array_repeat        e2e_array array_repeat)

# Algorithms (12 tests)
add_e2e_file_test(binary_search       e2e_algorithms binary_search)
add_e2e_file_test(fibonacci           e2e_algorithms fibonacci)
add_e2e_file_test(fizzbuzz            e2e_algorithms fizzbuzz)
add_e2e_file_test(gcd_lcm             e2e_algorithms gcd_lcm)
add_e2e_file_test(lcs                 e2e_algorithms longest_common_subsequence)
add_e2e_file_test(matrix_multiply     e2e_algorithms matrix_multiply)
add_e2e_file_test(merge_sort          e2e_algorithms merge_sort)
add_e2e_file_test(palindrome          e2e_algorithms palindrome_check)
add_e2e_file_test(roman_to_integer    e2e_algorithms roman_to_integer)
add_e2e_file_test(sieve               e2e_algorithms sieve_of_eratosthenes)
add_e2e_file_test(two_sum             e2e_algorithms two_sum)
add_e2e_file_test(valid_parens        e2e_algorithms valid_parentheses)

# Actors (10 tests)
add_e2e_file_test(actor_counter       e2e_actors actor_counter)
add_e2e_file_test(actor_lifecycle     e2e_actors actor_lifecycle)
add_e2e_file_test(actor_pipeline      e2e_actors actor_pipeline)
add_e2e_file_test(actor_ring          e2e_actors actor_ring)
add_e2e_file_test(fan_out_fan_in      e2e_actors fan_out_fan_in)
add_e2e_file_test(hot_code_path       e2e_actors hot_code_path)
add_e2e_file_test(lambda_actor        e2e_actors lambda_actor_patterns)
add_e2e_file_test(lambda_actor_capture e2e_actors lambda_actor_capture)
add_e2e_file_test(ping_pong           e2e_actors ping_pong)
add_e2e_file_test(producer_consumer   e2e_actors producer_consumer)
add_e2e_file_test(request_response    e2e_actors request_response)
add_e2e_file_test(actor_ref_passing   e2e_actors actor_ref_passing)
add_e2e_file_test(await_actor_ask     e2e_actors await_actor_ask)
add_e2e_file_test(actor_orchestration e2e_actors actor_orchestration)

# Concurrency (8 tests)
add_e2e_file_test(supervision_basic   e2e_concurrency actor_supervision_basic)
add_e2e_file_test(async_parallel      e2e_concurrency async_parallel)
add_e2e_file_test(async_sequential    e2e_concurrency async_sequential)
add_e2e_file_test(concurrent_counter  e2e_concurrency concurrent_counter)
add_e2e_file_test(deadlock_avoidance  e2e_concurrency deadlock_avoidance)
add_e2e_file_test(message_ordering    e2e_concurrency message_ordering)
add_e2e_file_test_sorted(scheduler_rr        e2e_concurrency scheduler_round_robin)
add_e2e_file_test_sorted(scope_structured    e2e_concurrency scope_structured)
add_e2e_file_test(select_basic        e2e_concurrency select_basic)
add_e2e_file_test(join_basic          e2e_concurrency join_basic)

# Timeout (2 tests)
add_e2e_file_test(select_timeout      e2e_timeout timeout)
add_e2e_file_test(timeout_basic       e2e_timeout timeout_basic)

# Data Structures (8 tests)
add_e2e_file_test(enum_state_machine  e2e_data_structures enum_state_machine)
add_e2e_file_test(hashmap_frequency   e2e_data_structures hashmap_frequency)
add_e2e_file_test(hashset_basic       e2e_hashset         hashset_basic)
add_e2e_file_test(hashset_strings     e2e_hashset         hashset_strings)
add_e2e_file_test(option_result       e2e_data_structures option_result_chain)
add_e2e_file_test(queue_via_stacks    e2e_data_structures queue_via_stacks)
add_e2e_file_test(stack_operations    e2e_data_structures stack_operations)
add_e2e_file_test(string_builder      e2e_data_structures string_builder)
add_e2e_file_test(struct_methods_ds   e2e_data_structures struct_methods)
add_e2e_file_test(match_or_pattern    e2e_data_structures match_or_pattern)
add_e2e_file_test(match_struct_pat    e2e_data_structures match_struct_pattern)
add_e2e_file_test(struct_enum         e2e_struct_enum struct_enum)
add_e2e_file_test(vec_operations      e2e_data_structures vec_operations)
add_e2e_file_test(vec_f64_ops         e2e_data_structures vec_f64_ops)
add_e2e_file_test(vec_bool_pushget    e2e_data_structures vec_bool)
add_e2e_file_test(bytes_basic         e2e_bytes           bytes_basic)
add_e2e_file_test(bytes_from          e2e_bytes           bytes_from)
add_e2e_file_test(bytes_append        e2e_bytes           bytes_append)
add_e2e_file_test(hex_test            e2e_hex             hex_test)
add_e2e_file_test(base64_test         e2e_base64          base64_test)
add_e2e_file_test(crypto_test         e2e_crypto          crypto_test)

# HashMap for-in iteration (sorted because HashMap order is non-deterministic)
add_e2e_file_test_sorted(hashmap_forin    e2e_hashmap_forin hashmap_forin)

# HashMapKeysOp: for-in over a HashMap produces key iteration via HashMapKeysOp
add_e2e_file_test(hashmap_keys    e2e_collections hashmap_keys)

# Compound assignment on indexed targets
add_e2e_file_test(indexed_compound  e2e_compound_assign indexed_compound)

# Memory (7 tests)
add_e2e_file_test(actor_termination   e2e_memory actor_termination_cleanup)
add_e2e_file_test(closure_capture     e2e_memory closure_capture_safety)
add_e2e_file_test(deep_recursion      e2e_memory deep_recursion)
add_e2e_file_test(large_vec           e2e_memory large_vec_lifecycle)
add_e2e_file_test(scope_cleanup       e2e_memory scope_cleanup)
add_e2e_file_test(string_lifecycle    e2e_memory string_lifecycle)
add_e2e_file_test(vec_get_borrowed    e2e_memory vec_get_borrowed_string)

# String Drop (RAII cleanup for heap-allocated strings)
add_e2e_file_test(string_drop         e2e_string_drop string_drop)

# String Ordering (lexicographic comparison)
add_e2e_file_test(string_ordering     e2e_string_ordering string_ordering)

# Move Closures
add_e2e_file_test(move_closure        e2e_move_closure move_closure)

# Performance (6 tests)
add_e2e_file_test(actor_spawn_rate    e2e_performance actor_spawn_rate)
add_e2e_file_test(collection_bench    e2e_performance collection_ops_bench)
add_e2e_file_test(fib_iter_bench      e2e_performance fibonacci_iterative_bench)
add_e2e_file_test(msg_throughput      e2e_performance message_throughput)
add_e2e_file_test(sched_throughput    e2e_performance scheduler_throughput)
add_e2e_file_test(string_bench        e2e_performance string_concat_bench)

# ── Example programs as E2E tests (from examples/) ───────────────────────────
# These validate that top-level example programs compile and produce correct output.
add_e2e_file_test(ex_fibonacci          e2e_examples fibonacci)
add_e2e_file_test(ex_basic_types        e2e_examples test_basic_types)
add_e2e_file_test(ex_block              e2e_examples test_block)
add_e2e_file_test(ex_enum               e2e_examples enum_test)
add_e2e_file_test(ex_string_ops         e2e_examples string_ops_test)
add_e2e_file_test(ex_comprehensive      e2e_examples comprehensive_test_simple)
add_e2e_file_test(ex_vec_hashmap        e2e_examples vec_hashmap_test)
add_e2e_file_test(ex_result_option      e2e_examples result_option_test)
add_e2e_file_test(ex_enums_options      e2e_examples enums_and_options)
add_e2e_file_test(ex_struct_spawn       e2e_examples struct_spawn_test)
add_e2e_file_test(ex_string             e2e_examples string_test)
add_e2e_file_test(ex_stress_match       e2e_examples stress_match)
add_e2e_file_test(ex_showcase           e2e_examples showcase)
add_e2e_file_test(ex_full_validation    e2e_examples full_validation)
add_e2e_file_test(ex_backpressure       e2e_examples backpressure_test)
add_e2e_file_test(ex_actor_pipeline_multi e2e_examples actor_pipeline_multi)
add_e2e_file_test(ex_lambda_capture     e2e_examples lambda_capture_basic)
add_e2e_file_test(ex_vec_set_ops        e2e_examples vec_set_operations)
add_e2e_file_test(ex_for_loop_patterns  e2e_examples for_loop_patterns)
add_e2e_file_test(ex_type_inference     e2e_examples type_inference)
add_e2e_file_test(ex_operators         e2e_examples test_operators_comprehensive)
add_e2e_file_test(ex_vec_complete      e2e_examples test_vec_complete)
add_e2e_file_test(ex_hashmap_complete  e2e_examples test_hashmap_complete)
add_e2e_file_test(ex_float_arith       e2e_examples test_float_arithmetic)
add_e2e_file_test(ex_loop_patterns     e2e_examples test_loop_patterns)
add_e2e_file_test(ex_nested_structs    e2e_examples test_nested_structs)
add_e2e_file_test(ex_string_edges      e2e_examples test_string_edge_cases)
add_e2e_file_test(ex_match_patterns    e2e_examples test_match_patterns)
add_e2e_file_test(ex_actor_lifecycle   e2e_examples test_actor_lifecycle)
add_e2e_file_test(ex_lambda_patterns   e2e_examples test_lambda_patterns)
# Force single scheduler worker so mailbox fill/drop is deterministic
set_tests_properties(e2e_examples_backpressure_test PROPERTIES ENVIRONMENT "HEW_WORKERS=1")

# ── Wire type tests ──────────────────────────────────────────────────────────
add_e2e_file_test(wire_basic            e2e_wire wire_basic)
add_e2e_file_test(wire_encode_decode    e2e_wire wire_encode_decode)
add_e2e_file_test(wire_json             e2e_wire wire_json)
add_e2e_file_test(wire_json_scalars     e2e_wire wire_json_scalars)
add_e2e_file_test(wire_json_naming      e2e_wire wire_json_naming)
add_e2e_file_test(wire_json_override    e2e_wire wire_json_override)
add_e2e_file_test(wire_json_roundtrip   e2e_wire wire_json_roundtrip)
add_e2e_file_test(wire_yaml_scalars     e2e_wire wire_yaml_scalars)
add_e2e_file_test(wire_yaml_naming      e2e_wire wire_yaml_naming)
add_e2e_file_test(wire_yaml_roundtrip   e2e_wire wire_yaml_roundtrip)

# ── Generics monomorphization tests ──────────────────────────────────────────
add_e2e_file_test(generic_identity      e2e_generics generic_identity)
add_e2e_file_test(generic_pair          e2e_generics generic_pair)
add_e2e_file_test(generic_fn            e2e_generics generic_fn)
add_e2e_file_test(generic_struct        e2e_generics generic_struct)
add_e2e_file_test(generic_multi_specialize e2e_generics generic_multi_specialize)
add_e2e_file_test(generic_multi_param   e2e_generics generic_multi_param)
add_e2e_file_test(generic_struct_param  e2e_generics generic_struct_param)
add_e2e_file_test(generic_all_types    e2e_generics generic_all_types)
add_e2e_file_test(generic_nested       e2e_generics generic_nested)

# ── Regex tests ───────────────────────────────────────────────────────────────
add_e2e_file_test(regex_is_match        e2e_regex regex_is_match)
add_e2e_file_test(regex_find_replace    e2e_regex regex_find_replace)

# RegexReplaceOp: focused test for regex pattern .replace() calls
add_e2e_file_test(regex_replace         e2e_text  regex_replace)

# ── Canonicalization tests ──────────────────────────────────────────────────
add_e2e_file_test(string_concat_identity  e2e_canonicalize string_concat_identity)

# ── Generator tests ──────────────────────────────────────────────────────────
add_e2e_file_test(simple_generator      e2e_generators simple_generator)
add_e2e_file_test(for_over_generator    e2e_generators for_over_generator)

# ── Async generator tests ────────────────────────────────────────────────────
add_e2e_file_test(async_gen             e2e_async_gen  async_gen)

# ── Receive gen fn tests (cross-actor streaming) ─────────────────────────────
add_e2e_file_test(receive_gen_basic     e2e_receive_gen receive_gen_basic)
add_e2e_file_test(receive_gen_empty     e2e_receive_gen receive_gen_empty)
add_e2e_file_test(receive_gen_fib       e2e_receive_gen receive_gen_fib)
add_e2e_file_test(receive_gen_params    e2e_receive_gen receive_gen_params)

# ── Coalesce overflow tests ──────────────────────────────────────────────────
add_e2e_file_test(coalesce_basic        e2e_coalesce coalesce_basic)

# ── Closure first-class tests ─────────────────────────────────────────────────
add_e2e_file_test(closure_firstclass    e2e_closure_firstclass closure_firstclass)

# ── Closure env Rc memory management tests ────────────────────────────────────
add_e2e_file_test(closure_env_rc        e2e_closure_env_rc closure_env_rc)

# ── Mutable variable capture in closures ──────────────────────────────────────
add_e2e_file_test(closure_mutable_capture  e2e_closures closure_mutable_capture)
add_e2e_file_test(closure_read_mutable     e2e_closures closure_read_mutable)
add_e2e_file_test(closure_match_capture    e2e_closures closure_match_capture)

# ── dyn Trait tests ──────────────────────────────────────────────────────────
add_e2e_file_test(dyn_dispatch          e2e_dyn_trait dyn_dispatch)
add_e2e_file_test(dyn_dispatch_string   e2e_dyn_trait dyn_dispatch_string)
add_e2e_file_test(dyn_dispatch_args     e2e_dyn_trait dyn_dispatch_args)
add_e2e_file_test(dyn_dispatch_void     e2e_dyn_trait dyn_dispatch_void)
add_e2e_file_test(trait_inherit         e2e_trait_inherit trait_inherit)

# ── Trait impl tests ─────────────────────────────────────────────────────────
add_e2e_file_test(trait_impl            e2e_traits trait_impl)
add_e2e_file_test(trait_multi_impl      e2e_traits trait_multi_impl)

# ── Inherent impl tests ─────────────────────────────────────────────────────
add_e2e_file_test(struct_impl_method    e2e_impl struct_impl_method)
add_e2e_file_test(self_sugar            e2e_self_sugar self_sugar)
add_e2e_file_test(self_sugar_mixed      e2e_self_sugar self_sugar_mixed)

# ── Struct field assignment tests ────────────────────────────────────────────
add_e2e_file_test(struct_assign         e2e_struct_assign struct_assign)

# ── Tuple destructuring tests ────────────────────────────────────────────────
add_e2e_file_test(tuple_destruct        e2e_tuple_destruct tuple_destruct)

# ── Bench tests ──────────────────────────────────────────────────────────────
add_e2e_file_test(bench_basic           e2e_bench bench_basic)

# ── Import tests ─────────────────────────────────────────────────────────────
add_e2e_file_test(import_stdlib         e2e_imports import_stdlib)
add_e2e_file_test(import_fs             e2e_imports test_import_fs)
add_e2e_file_test(import_string         e2e_imports test_import_string)
add_e2e_file_test(import_os             e2e_imports test_import_os)
add_e2e_file_test(import_multi          e2e_imports test_import_multi)
add_e2e_file_test(import_string_ops     e2e_imports test_import_string_ops)
add_e2e_file_test(import_multi_stdlib   e2e_imports test_import_multi_stdlib)

# ── Try operator tests ──────────────────────────────────────────────────────
add_e2e_file_test(try_operator          e2e_try_operator try_operator)

# Error handling patterns (4 tests)
add_e2e_file_test(error_return_codes    e2e_error_handling error_return_codes)
add_e2e_file_test(result_pattern        e2e_error_handling result_pattern)
add_e2e_file_test(option_pattern        e2e_error_handling option_pattern)
add_e2e_file_test(option_stdlib         e2e_option_stdlib option_stdlib)
add_e2e_file_test(enum_error_chain      e2e_error_handling enum_error_chain)
add_e2e_file_test(result_stdlib         e2e_error_handling result_stdlib)

# ── Structured concurrency tests ─────────────────────────────────────────────
add_e2e_file_test(scope_launch          e2e_concurrency scope_launch)
add_e2e_file_test(scope_launch_await    e2e_concurrency scope_launch_await)
add_e2e_file_test(scope_launch_await_bool e2e_concurrency scope_launch_await_bool)
add_e2e_file_test(scope_spawn           e2e_scope_spawn scope_spawn)

# ── Module/import tests ──────────────────────────────────────────────────────
add_e2e_file_test(module_basic          e2e_modules module_basic)
add_e2e_file_test(module_qualified      e2e_modules module_qualified)
add_e2e_file_test(module_glob           e2e_modules module_glob)
add_e2e_file_test(module_named          e2e_modules module_named)
add_e2e_reject_test(module_private      e2e_modules module_private_reject "no function")
add_e2e_file_test(module_alias          e2e_modules module_alias)
add_e2e_file_test(module_multi_import   e2e_modules module_multi_import)
add_e2e_file_test(module_types          e2e_modules module_types)
add_e2e_file_test(module_consts         e2e_modules module_consts)
add_e2e_file_test(module_chain          e2e_modules module_chain)
add_e2e_file_test(module_diamond        e2e_modules module_diamond)
add_e2e_file_test(module_enum           e2e_modules module_enum)
add_e2e_file_test(module_trait_dispatch e2e_modules module_trait_dispatch)
add_e2e_file_test(module_trait_inherit  e2e_modules module_trait_inherit)

# ── Drop trait tests ─────────────────────────────────────────────────────────
add_e2e_file_test(drop_trait            e2e_drop_trait drop_trait)
add_e2e_file_test(drop_scope            e2e_drop_scope drop_scope)

# ── Return statement tests ───────────────────────────────────────────────────
add_e2e_file_test(return_after_drop     e2e_return return_after_drop)

# ── Stress tests ─────────────────────────────────────────────────────────────
add_e2e_file_test(stress_mass_spawn     e2e_stress stress_mass_spawn)
add_e2e_file_test(stress_ring           e2e_stress stress_ring)
add_e2e_file_test(stress_message_burst  e2e_stress stress_message_burst)

# ── Bytes stream tests (Stream<T> / Sink<T> first-class streams) ─────────────
add_e2e_file_test(stream_file_count      e2e_bytes stream_file_count)
add_e2e_file_test(stream_channel_count   e2e_bytes stream_channel_count)
add_e2e_file_test(stream_channel_data    e2e_bytes stream_channel_data)
add_e2e_file_test(stream_file_write_read e2e_bytes stream_file_write_read)
add_e2e_file_test(stream_chunks          e2e_bytes stream_chunks)
add_e2e_file_test(stream_multi_channel   e2e_bytes stream_multi_channel)
add_e2e_file_test(stream_inline_call        e2e_bytes stream_inline_call)
add_e2e_file_test(stream_method_forawait    e2e_bytes stream_method_forawait)
add_e2e_file_test(stream_producer_consumer e2e_bytes stream_producer_consumer)
add_e2e_file_test(stream_file_relay      e2e_bytes stream_file_relay)
add_e2e_file_test(stream_actor_sink      e2e_bytes stream_actor_sink)
add_e2e_file_test(http_actor_handler     e2e_bytes http_actor_handler)
add_e2e_file_test(stream_break           e2e_bytes stream_break)
add_e2e_file_test(stream_collect         e2e_bytes stream_collect)
add_e2e_file_test(stream_write_string    e2e_bytes stream_write_string)
add_e2e_file_test(stream_lines_no        e2e_bytes stream_lines_no_newline)
add_e2e_file_test(stream_idiomatic       e2e_bytes stream_idiomatic)
add_e2e_file_test(stream_actor_fs        e2e_bytes stream_actor_fileserver)
add_e2e_file_test(stream_pipe            e2e_bytes stream_pipe)
add_e2e_file_test(stream_result          e2e_bytes stream_result)

# ── Link/Monitor tests ──────────────────────────────────────────────────────
add_e2e_file_test(link_monitor          e2e_link_monitor link_monitor)

# ── Supervisor tests ───────────────────────────────────────────────────────
add_e2e_file_test(supervisor_basic      e2e_supervisor_basic supervisor_basic)
add_e2e_file_test(supervisor_strategy   e2e_supervisor_strategy supervisor_strategy)
add_e2e_file_test(supervisor_child      e2e_supervisor_child supervisor_child)
add_e2e_file_test(supervisor_restart    e2e_supervisor_restart supervisor_restart)
add_e2e_file_test(supervisor_nested     e2e_supervisor_nested supervisor_nested)

# ── Defer tests ────────────────────────────────────────────────────────────
add_e2e_file_test(defer                 e2e_defer defer)

# ── Loop tests ─────────────────────────────────────────────────────────────
add_e2e_file_test(for_range_break       e2e_loops for_range_break)
add_e2e_file_test(labeled_break_drops   e2e_loops labeled_break_drops)
add_e2e_file_test(loop_return           e2e_loops loop_return)

# ── Named arguments tests ─────────────────────────────────────────────────
add_e2e_file_test(named_args            e2e_named_args main)

# ── Logging tests ─────────────────────────────────────────────────────────
add_e2e_file_test(log_levels            e2e_log_levels main)
add_e2e_file_test(log_kwargs            e2e_log_kwargs main)

# ── Canonicalization tests ────────────────────────────────────────────────
add_e2e_file_test(cast_chain             e2e_canonicalize cast_chain)

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
  set_tests_properties(${TEST_NAME} PROPERTIES
    RESOURCE_LOCK hew_compile
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

# ── Missing E2E test registrations ────────────────────────────────────────────
add_e2e_file_test(ranges               e2e_ranges range_value)
add_e2e_file_test(char_literals        e2e_char_literals char_literals)
add_e2e_file_test(for_labels           e2e_for_labels for_labels)
add_e2e_file_test(tail_call            e2e_tail_call tail_call)
add_e2e_file_test(vec_bool             e2e_vec vec_bool)
add_e2e_file_test(tuple_pattern        e2e_match tuple_pattern)
add_e2e_file_test(match_struct_variant_last_arm e2e_match match_struct_variant_last_arm)
add_e2e_file_test(match_nested_pattern  e2e_match match_nested_pattern)
add_e2e_file_test(if_let_basic         e2e_if_let if_let_basic)
add_e2e_file_test(if_let_return        e2e_if_let if_let_return)

# ── Coverage tests (ops with zero prior E2E coverage) ────────────────────────
add_e2e_file_test(coverage_assert_ops         e2e_coverage assert_ops)
add_e2e_file_test(coverage_string_case        e2e_coverage string_case_methods)
add_e2e_file_test(coverage_string_methods     e2e_coverage string_method_calls)
add_e2e_file_test(coverage_to_string          e2e_coverage to_string_op)
add_e2e_file_test(coverage_sleep              e2e_coverage sleep_op)
add_e2e_file_test(coverage_return_in_loop     e2e_coverage return_in_loop)
add_e2e_file_test(coverage_actor_stop_close  e2e_coverage actor_stop_close)
add_e2e_file_test(coverage_cooperate         e2e_coverage cooperate_op)
add_e2e_file_test(coverage_vec_remove_clear  e2e_coverage vec_remove_clear)
add_e2e_file_test(coverage_struct_field_set  e2e_coverage struct_field_set)
add_e2e_file_test(scope_cancel_check         e2e_coverage scope_cancel_check)
add_e2e_file_test(log_basic_cov              e2e_coverage log_basic)
add_e2e_file_test(supervisor_stop_cov       e2e_coverage supervisor_stop)
add_e2e_file_test(lambda_send               e2e_coverage lambda_send)
add_e2e_file_test(regex_literal             e2e_coverage regex_literal)
add_e2e_file_test(cast_numeric              e2e_coverage cast_numeric)

# ── Golden dialect IR validation tests ────────────────────────────────────────
# Each test targets specific Hew MLIR dialect ops to ensure the compiler
# produces correct IR for known source constructs.
add_e2e_file_test(golden_struct              e2e_golden golden_struct)
add_e2e_file_test(golden_enum                e2e_golden golden_enum)
add_e2e_file_test(golden_vec                 e2e_golden golden_vec)
add_e2e_file_test(golden_hashmap             e2e_golden golden_hashmap)
add_e2e_file_test(golden_closure             e2e_golden golden_closure)
add_e2e_file_test(golden_actor               e2e_golden golden_actor)
add_e2e_file_test(golden_scope               e2e_golden golden_scope)
add_e2e_file_test(golden_option_result       e2e_golden golden_option_result)
add_e2e_file_test(golden_string              e2e_golden golden_string)
add_e2e_file_test(golden_generator           e2e_golden golden_generator)

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
# Only tests that use WASM-compatible features (no actors, threads, networking,
# or ecosystem features like regex/json/yaml/crypto/base64/hex).

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
add_wasm_file_test(bitwise_ops          e2e_bitwise bitwise_ops)
add_wasm_file_test(hex_literals         e2e_hex_literals hex_literals)
add_wasm_file_test(array_repeat         e2e_array array_repeat)
add_wasm_file_test(dynamic_index        e2e_array dynamic_index)
add_wasm_file_test(duration             e2e_duration duration)

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
add_wasm_file_test(closure_firstclass   e2e_closure_firstclass closure_firstclass)
add_wasm_file_test(closure_env_rc       e2e_closure_env_rc closure_env_rc)
add_wasm_file_test(closure_mutable      e2e_closures closure_mutable_capture)
add_wasm_file_test(closure_read_mutable e2e_closures closure_read_mutable)
add_wasm_file_test(closure_match_capture e2e_closures closure_match_capture)
add_wasm_file_test(move_closure         e2e_move_closure move_closure)

# Struct/tuple features
add_wasm_file_test(struct_assign        e2e_struct_assign struct_assign)
add_wasm_file_test(tuple_destruct       e2e_tuple_destruct tuple_destruct)

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

# Canonicalization
add_wasm_file_test(string_concat_id     e2e_canonicalize string_concat_identity)
add_wasm_file_test(cast_chain           e2e_canonicalize cast_chain)

# Drop/cleanup
add_wasm_file_test(drop_trait           e2e_drop_trait drop_trait)
add_wasm_file_test(drop_scope           e2e_drop_scope drop_scope)
add_wasm_file_test(string_drop          e2e_string_drop string_drop)

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

# WASM actor tests
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
