// Domain harness for hew-hir `monomorphization` tests.
// Add new monomorphization-behaviour tests under `tests/monomorphization/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "monomorphization/layout_mono_pass.rs"]
mod layout_mono_pass;
#[path = "monomorphization/machine_mono_pass.rs"]
mod machine_mono_pass;
#[path = "monomorphization/mangle_resolved_ty_segment_parity.rs"]
mod mangle_resolved_ty_segment_parity;
#[path = "monomorphization/mangle_resolved_ty_test.rs"]
mod mangle_resolved_ty_test;
#[path = "monomorphization/mono_foundation_byte_compat.rs"]
mod mono_foundation_byte_compat;
#[path = "monomorphization/monomorph_call_type_args_boundary.rs"]
mod monomorph_call_type_args_boundary;
#[path = "monomorphization/monomorph_closure_test.rs"]
mod monomorph_closure_test;
#[path = "monomorphization/monomorph_inner_call_probe.rs"]
mod monomorph_inner_call_probe;
#[path = "monomorphization/monomorph_registry_test.rs"]
mod monomorph_registry_test;
#[path = "monomorphization/monomorph_trait_rewrite_probe.rs"]
mod monomorph_trait_rewrite_probe;
#[path = "monomorphization/type_mono_registry_test.rs"]
mod type_mono_registry_test;
