// Domain harness for hew-types `resolved_call` tests.
// Add new resolved_call-behaviour tests under `tests/resolved_call/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

// Declared once here (not per-leaf) because two or more leaves in this
// harness use `tests/common/`; declaring it in each leaf would load the
// same file multiple times in one binary (clippy::duplicate_mod). Leaves
// reference it via `use crate::common;`.
#[path = "common/mod.rs"]
mod common;

#[path = "resolved_call/resolved_call_bounds_form_unit.rs"]
mod resolved_call_bounds_form_unit;
#[path = "resolved_call/resolved_call_hashmap_coverage.rs"]
mod resolved_call_hashmap_coverage;
#[path = "resolved_call/resolved_call_hashmap_scalar_k_unit.rs"]
mod resolved_call_hashmap_scalar_k_unit;
#[path = "resolved_call/resolved_call_hashset_coverage.rs"]
mod resolved_call_hashset_coverage;
#[path = "resolved_call/resolved_call_kernel_symbols.rs"]
mod resolved_call_kernel_symbols;
#[path = "resolved_call/resolved_call_registry_lookup.rs"]
mod resolved_call_registry_lookup;
