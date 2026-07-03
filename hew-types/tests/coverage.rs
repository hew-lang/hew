// Domain harness for hew-types `coverage` tests.
// Add new coverage-behaviour tests under `tests/coverage/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

// Declared once here (not per-leaf) because two or more leaves in this
// harness use `tests/common/`; declaring it in each leaf would load the
// same file multiple times in one binary (clippy::duplicate_mod). Leaves
// reference it via `use crate::common;`.
#[path = "common/mod.rs"]
mod common;

#[path = "coverage/active_mode_handler_coercion.rs"]
mod active_mode_handler_coercion;
#[path = "coverage/builtin_type_discriminator.rs"]
mod builtin_type_discriminator;
#[path = "coverage/char_cast.rs"]
mod char_cast;
#[path = "coverage/checked_mir_corpus_coverage.rs"]
mod checked_mir_corpus_coverage;
#[path = "coverage/clone_expression.rs"]
mod clone_expression;
#[path = "coverage/generic_enum_variants.rs"]
mod generic_enum_variants;
#[path = "coverage/generic_lambda_multi_instantiation.rs"]
mod generic_lambda_multi_instantiation;
#[path = "coverage/hash_eq_derivation_audit.rs"]
mod hash_eq_derivation_audit;
#[path = "coverage/is_allowance.rs"]
mod is_allowance;
#[path = "coverage/numeric_method_lowering.rs"]
mod numeric_method_lowering;
#[path = "coverage/numerics.rs"]
mod numerics;
#[path = "coverage/tail_ok_coercion.rs"]
mod tail_ok_coercion;
#[path = "coverage/ty_coverage.rs"]
mod ty_coverage;
#[path = "coverage/type_error_coverage.rs"]
mod type_error_coverage;
#[path = "coverage/type_system_check.rs"]
mod type_system_check;
#[path = "coverage/type_system_negative.rs"]
mod type_system_negative;
#[path = "coverage/unify_coverage.rs"]
mod unify_coverage;
#[path = "coverage/unsafe_enforcement_test.rs"]
mod unsafe_enforcement_test;
#[path = "coverage/vec_range_slice.rs"]
mod vec_range_slice;
