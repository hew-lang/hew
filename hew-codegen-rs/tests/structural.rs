// Domain harness for hew-codegen-rs `structural` tests.
// Add new structural-behaviour tests under `tests/structural/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "structural/actor_send_status_check.rs"]
mod actor_send_status_check;
#[path = "structural/bytes_extern_fail_closed.rs"]
mod bytes_extern_fail_closed;
#[path = "structural/closure_env_drop_manifest.rs"]
mod closure_env_drop_manifest;
#[path = "structural/composite_return.rs"]
mod composite_return;
#[path = "structural/d65_vec_release_truth_table.rs"]
mod d65_vec_release_truth_table;
#[path = "structural/emit_duplex_pair.rs"]
mod emit_duplex_pair;
#[path = "structural/error_enum_cross_crate_parity.rs"]
mod error_enum_cross_crate_parity;
#[path = "structural/extern_malloc_string_adoption.rs"]
mod extern_malloc_string_adoption;
#[path = "structural/fork_spawn_env_drop.rs"]
mod fork_spawn_env_drop;
#[path = "structural/hashmap_layout_ops.rs"]
mod hashmap_layout_ops;
#[path = "structural/machine_layout.rs"]
mod machine_layout;
#[path = "structural/periodic_spawn_arming.rs"]
mod periodic_spawn_arming;
#[path = "structural/resolved_impl_call_hashmap_layout_descriptor_materialisation.rs"]
mod resolved_impl_call_hashmap_layout_descriptor_materialisation;
#[path = "structural/scalar_discard_guard.rs"]
mod scalar_discard_guard;
#[path = "structural/state_clone_generic_enum_pipeline.rs"]
mod state_clone_generic_enum_pipeline;
#[path = "structural/state_clone_synthesis.rs"]
mod state_clone_synthesis;
#[path = "structural/supervised_handler_name_registration.rs"]
mod supervised_handler_name_registration;
#[path = "structural/wasm_duplex_classification.rs"]
mod wasm_duplex_classification;
#[path = "structural/wasm_task_scope_classification.rs"]
mod wasm_task_scope_classification;
