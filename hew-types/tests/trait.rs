// Domain harness for hew-types `trait` tests.
// Add new trait-behaviour tests under `tests/trait/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

// Declared once here (not per-leaf) because two or more leaves in this
// harness use `tests/common/`; declaring it in each leaf would load the
// same file multiple times in one binary (clippy::duplicate_mod). Leaves
// reference it via `use crate::common;`.
#[path = "common/mod.rs"]
mod common;

#[path = "trait/associated_types.rs"]
mod associated_types;
#[path = "trait/builtin_trait_method_projection.rs"]
mod builtin_trait_method_projection;
#[path = "trait/closable_trait.rs"]
mod closable_trait;
#[path = "trait/dyn_trait_coercion.rs"]
mod dyn_trait_coercion;
#[path = "trait/iterator_trait_surface.rs"]
mod iterator_trait_surface;
#[path = "trait/trait_bounds.rs"]
mod trait_bounds;
#[path = "trait/trait_coverage.rs"]
mod trait_coverage;
#[path = "trait/trait_default_method_self.rs"]
mod trait_default_method_self;
#[path = "trait/trait_impl_signature_equivalence.rs"]
mod trait_impl_signature_equivalence;
#[path = "trait/trait_object_order.rs"]
mod trait_object_order;
#[path = "trait/trait_self_primitive_impl.rs"]
mod trait_self_primitive_impl;
