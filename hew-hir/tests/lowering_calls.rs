// Domain harness for hew-hir `lowering_calls` tests.
// Add new lowering_calls-behaviour tests under `tests/lowering_calls/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

// Declared once here (not per-leaf) because two or more leaves in this
// harness use `tests/support/`; declaring it in each leaf would load the
// same file multiple times in one binary (clippy::duplicate_mod). Leaves
// reference it via `use crate::support;`.
#[path = "support/mod.rs"]
mod support;

#[path = "lowering_calls/builtin_return_type_lower.rs"]
mod builtin_return_type_lower;
#[path = "lowering_calls/call_trait_method_static_creation_allowlist.rs"]
mod call_trait_method_static_creation_allowlist;
#[path = "lowering_calls/closure_capture_lower.rs"]
mod closure_capture_lower;
#[path = "lowering_calls/display_dispatch_lower.rs"]
mod display_dispatch_lower;
#[path = "lowering_calls/dyn_trait_type_annotation_lower.rs"]
mod dyn_trait_type_annotation_lower;
#[path = "lowering_calls/imported_free_fn_lower.rs"]
mod imported_free_fn_lower;
#[path = "lowering_calls/imported_impl_lower.rs"]
mod imported_impl_lower;
#[path = "lowering_calls/loop_call_site_walker.rs"]
mod loop_call_site_walker;
#[path = "lowering_calls/math_builtin_lower.rs"]
mod math_builtin_lower;
#[path = "lowering_calls/mem_intrinsic_floor_lower.rs"]
mod mem_intrinsic_floor_lower;
#[path = "lowering_calls/method_call_bridge.rs"]
mod method_call_bridge;
#[path = "lowering_calls/static_trait_dispatch.rs"]
mod static_trait_dispatch;
