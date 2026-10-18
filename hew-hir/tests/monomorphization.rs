// Domain harness for hew-hir `monomorphization` tests.
// Add new monomorphization-behaviour tests under `tests/monomorphization/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "monomorphization/mangle_resolved_ty_test.rs"]
mod mangle_resolved_ty_test;
#[path = "monomorphization/mono_foundation_byte_compat.rs"]
mod mono_foundation_byte_compat;
#[path = "monomorphization/monomorph_call_type_args_boundary.rs"]
mod monomorph_call_type_args_boundary;
#[path = "monomorphization/monomorph_closure_test.rs"]
mod monomorph_closure_test;
