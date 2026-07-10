// Domain harness for hew-codegen-rs `emission` tests.
// Add new emission-behaviour tests under `tests/emission/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "emission/char_emission.rs"]
mod char_emission;
#[path = "emission/cooperate_emission.rs"]
mod cooperate_emission;
#[path = "emission/div_shift_trap_emission.rs"]
mod div_shift_trap_emission;
#[path = "emission/duration_emission.rs"]
mod duration_emission;
#[path = "emission/dyn_trait_drop_in_place_emission.rs"]
mod dyn_trait_drop_in_place_emission;
#[path = "emission/dyn_trait_thunk_emission.rs"]
mod dyn_trait_thunk_emission;
#[path = "emission/dyn_trait_vtable_emission.rs"]
mod dyn_trait_vtable_emission;
#[path = "emission/float_emission.rs"]
mod float_emission;
#[path = "emission/hashmap_hashset_local_drop_emission.rs"]
mod hashmap_hashset_local_drop_emission;
#[path = "emission/hashmap_layout_emission.rs"]
mod hashmap_layout_emission;
#[path = "emission/identity_emission.rs"]
mod identity_emission;
#[path = "emission/mem_floor_intrinsic_emission.rs"]
mod mem_floor_intrinsic_emission;
#[path = "emission/overflow_trap_emission.rs"]
mod overflow_trap_emission;
#[path = "emission/record_emission.rs"]
mod record_emission;
#[path = "emission/stdlib_builtins_emission.rs"]
mod stdlib_builtins_emission;
#[path = "emission/stdlib_print_emission.rs"]
mod stdlib_print_emission;
#[path = "emission/supervisor_child_get_emission.rs"]
mod supervisor_child_get_emission;
#[path = "emission/supervisor_emission.rs"]
mod supervisor_emission;
#[path = "emission/task_abi_emission.rs"]
mod task_abi_emission;
#[path = "emission/task_entry_cancel_composite_emission.rs"]
mod task_entry_cancel_composite_emission;
#[path = "emission/trap_kind_emission.rs"]
mod trap_kind_emission;
#[path = "emission/tuple_construct_emission.rs"]
mod tuple_construct_emission;
#[path = "emission/unary_emission.rs"]
mod unary_emission;
#[path = "emission/user_fn_emission.rs"]
mod user_fn_emission;
#[path = "emission/vec_index_emission.rs"]
mod vec_index_emission;
#[path = "emission/vec_layout_emission.rs"]
mod vec_layout_emission;
#[path = "emission/vec_slice_emission.rs"]
mod vec_slice_emission;
#[path = "emission/wasm_actor_metadata_emission.rs"]
mod wasm_actor_metadata_emission;
#[path = "emission/wrapping_emission.rs"]
mod wrapping_emission;
