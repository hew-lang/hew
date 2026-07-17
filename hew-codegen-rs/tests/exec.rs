// Domain harness for hew-codegen-rs `exec` tests.
// Add new exec-behaviour tests under `tests/exec/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "exec/bytes_index_exec.rs"]
mod bytes_index_exec;
#[path = "exec/coro_emission_exec.rs"]
mod coro_emission_exec;
#[path = "exec/ffi_int_width_exec.rs"]
mod ffi_int_width_exec;
#[path = "exec/float_return_exec.rs"]
mod float_return_exec;
#[path = "exec/for_in_vec_string_exec.rs"]
mod for_in_vec_string_exec;
#[path = "exec/generator_exec.rs"]
mod generator_exec;
#[path = "exec/hashmap_index_exec.rs"]
mod hashmap_index_exec;
#[path = "exec/hashmap_keys_values_exec.rs"]
mod hashmap_keys_values_exec;
#[path = "exec/if_let_exec.rs"]
mod if_let_exec;
#[path = "exec/machine_emit_exec.rs"]
mod machine_emit_exec;
#[path = "exec/machine_exec.rs"]
mod machine_exec;
#[path = "exec/match_literal_guard_default_exec.rs"]
mod match_literal_guard_default_exec;
#[path = "exec/mem_floor_intrinsic_exec.rs"]
mod mem_floor_intrinsic_exec;
#[path = "exec/numeric_methods_exec.rs"]
mod numeric_methods_exec;
#[path = "exec/primitive_trait_bound_exec.rs"]
mod primitive_trait_bound_exec;
#[path = "exec/recursion_exec.rs"]
mod recursion_exec;
#[path = "exec/sink_owned_element_exec.rs"]
mod sink_owned_element_exec;
#[path = "exec/structural_eq_exec.rs"]
mod structural_eq_exec;
#[path = "exec/user_enum_exec.rs"]
mod user_enum_exec;
#[path = "exec/vec_enum_index_exec.rs"]
mod vec_enum_index_exec;
#[path = "exec/vec_i32_index_exec.rs"]
mod vec_i32_index_exec;
#[path = "exec/vec_owned_element_routing_exec.rs"]
mod vec_owned_element_routing_exec;
#[path = "exec/vec_record_contains_exec.rs"]
mod vec_record_contains_exec;
#[path = "exec/vec_record_index_exec.rs"]
mod vec_record_index_exec;
#[path = "exec/wasm_generator_exec.rs"]
mod wasm_generator_exec;
#[path = "exec/wasm_try_to_platform_width_exec.rs"]
mod wasm_try_to_platform_width_exec;
