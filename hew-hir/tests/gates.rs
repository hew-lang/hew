// Domain harness for hew-hir `gates` tests.
// Add new gates-behaviour tests under `tests/gates/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

// Declared once here (not per-leaf) because two or more leaves in this
// harness use `tests/support/`; declaring it in each leaf would load the
// same file multiple times in one binary (clippy::duplicate_mod). Leaves
// reference it via `use crate::support;`.
#[path = "support/mod.rs"]
mod support;

#[path = "gates/binary_operator_gates.rs"]
mod binary_operator_gates;
#[path = "gates/catalog_inventory_gate.rs"]
mod catalog_inventory_gate;
#[path = "gates/checker_boundary_violation.rs"]
mod checker_boundary_violation;
#[path = "gates/clone_not_yet_supported.rs"]
mod clone_not_yet_supported;
#[path = "gates/record_layout_missing.rs"]
mod record_layout_missing;
#[path = "gates/root_item_ids_excludes_builtins.rs"]
mod root_item_ids_excludes_builtins;
#[path = "gates/target_gates.rs"]
mod target_gates;
#[path = "gates/vec_index_element_type_gates.rs"]
mod vec_index_element_type_gates;
