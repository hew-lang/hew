// Domain harness for hew-types `ffi_differential` tests.
// Add new ffi_differential-behaviour tests under `tests/ffi_differential/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

// Declared once here (not per-leaf) because two or more leaves in this
// harness use `tests/common/`; declaring it in each leaf would load the
// same file multiple times in one binary (clippy::duplicate_mod). Leaves
// reference it via `use crate::common;`.
#[path = "common/mod.rs"]
mod common;

#[path = "ffi_differential/declarative_monomorphic_ffi_differential.rs"]
mod declarative_monomorphic_ffi_differential;
#[path = "ffi_differential/declarative_option_result_ffi_differential.rs"]
mod declarative_option_result_ffi_differential;
#[path = "ffi_differential/declarative_string_bytes_ffi_differential.rs"]
mod declarative_string_bytes_ffi_differential;
