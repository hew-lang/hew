// Domain harness for hew-hir `lowering_core` tests.
// Add new lowering_core-behaviour tests under `tests/lowering_core/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

// Declared once here (not per-leaf) because two or more leaves in this
// harness use `tests/support/`; declaring it in each leaf would load the
// same file multiple times in one binary (clippy::duplicate_mod). Leaves
// reference it via `use crate::support;`.
#[path = "support/mod.rs"]
mod support;

#[path = "lowering_core/bitcopy_inference.rs"]
mod bitcopy_inference;
#[path = "lowering_core/cross_module_machine_binding.rs"]
mod cross_module_machine_binding;
#[path = "lowering_core/duplex_constructor_verify.rs"]
mod duplex_constructor_verify;
#[path = "lowering_core/gen_block_lowering.rs"]
mod gen_block_lowering;
#[path = "lowering_core/impl_block_lowering.rs"]
mod impl_block_lowering;
#[path = "lowering_core/index_trait_lowering.rs"]
mod index_trait_lowering;
#[path = "lowering_core/invalid_constructor_diagnostics.rs"]
mod invalid_constructor_diagnostics;
#[path = "lowering_core/is_type_pattern_lowering.rs"]
mod is_type_pattern_lowering;
#[path = "lowering_core/let_wildcard_discard.rs"]
mod let_wildcard_discard;
#[path = "lowering_core/loop_break_continue_lower.rs"]
mod loop_break_continue_lower;
#[path = "lowering_core/m3_surface_pid_lower.rs"]
mod m3_surface_pid_lower;
#[path = "lowering_core/machine_hir.rs"]
mod machine_hir;
#[path = "lowering_core/machine_walker_coverage.rs"]
mod machine_walker_coverage;
#[path = "lowering_core/nested_match_payload_lower.rs"]
mod nested_match_payload_lower;
#[path = "lowering_core/numeric_cast_lowering.rs"]
mod numeric_cast_lowering;
#[path = "lowering_core/raw_pointer_no_lowering.rs"]
mod raw_pointer_no_lowering;
#[path = "lowering_core/record_registry_collision_guard.rs"]
mod record_registry_collision_guard;
#[path = "lowering_core/regex_match_arm_lower.rs"]
mod regex_match_arm_lower;
#[path = "lowering_core/stdlib_catalog_layout_descriptor_coverage.rs"]
mod stdlib_catalog_layout_descriptor_coverage;
#[path = "lowering_core/stdlib_catalog_layout_descriptor_non_callable.rs"]
mod stdlib_catalog_layout_descriptor_non_callable;
#[path = "lowering_core/substitute_ty_composites.rs"]
mod substitute_ty_composites;
#[path = "lowering_core/tuple_let_destructure.rs"]
mod tuple_let_destructure;
#[path = "lowering_core/tuple_literal_lowering.rs"]
mod tuple_literal_lowering;
#[path = "lowering_core/type_identity_carrier.rs"]
mod type_identity_carrier;
#[path = "lowering_core/unary_lowering.rs"]
mod unary_lowering;
#[path = "lowering_core/var_self_writeback.rs"]
mod var_self_writeback;
#[path = "lowering_core/vec_index_lowering.rs"]
mod vec_index_lowering;
#[path = "lowering_core/vec_slice_lowering.rs"]
mod vec_slice_lowering;
#[path = "lowering_core/vertical.rs"]
mod vertical;
