// Domain harness for hew-types `registry_core` tests.
// Add new registry_core-behaviour tests under `tests/registry_core/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

// Declared once here (not per-leaf) because two or more leaves in this
// harness use `tests/common/`; declaring it in each leaf would load the
// same file multiple times in one binary (clippy::duplicate_mod). Leaves
// reference it via `use crate::common;`.
#[path = "common/mod.rs"]
mod common;

#[path = "registry_core/builtins_duplex_resolution.rs"]
mod builtins_duplex_resolution;
#[path = "registry_core/check_regex_pattern.rs"]
mod check_regex_pattern;
#[path = "registry_core/closure_escape_advisory.rs"]
mod closure_escape_advisory;
#[path = "registry_core/consumes_receiver_metadata.rs"]
mod consumes_receiver_metadata;
#[path = "registry_core/descriptor_authority.rs"]
mod descriptor_authority;
#[path = "registry_core/extern_symbol_runtime_descriptor_boundary.rs"]
mod extern_symbol_runtime_descriptor_boundary;
#[path = "registry_core/iterator_lazy_wrappers.rs"]
mod iterator_lazy_wrappers;
#[path = "registry_core/link_monitor_sigs.rs"]
mod link_monitor_sigs;
#[path = "registry_core/m3_surface_pid_trait.rs"]
mod m3_surface_pid_trait;
#[path = "registry_core/m3_surface_pid_types.rs"]
mod m3_surface_pid_types;
#[path = "registry_core/machine_typecheck.rs"]
mod machine_typecheck;
#[path = "registry_core/methods_duplex.rs"]
mod methods_duplex;
#[path = "registry_core/module_qualified_constructor_test.rs"]
mod module_qualified_constructor_test;
#[path = "registry_core/module_system_test.rs"]
mod module_system_test;
#[path = "registry_core/no_legacy_vec_rewrite.rs"]
mod no_legacy_vec_rewrite;
#[path = "registry_core/node_register.rs"]
mod node_register;
#[path = "registry_core/owned_handle_accessors.rs"]
mod owned_handle_accessors;
#[path = "registry_core/q004_self_item_projection.rs"]
mod q004_self_item_projection;
#[path = "registry_core/record_marker_derive.rs"]
mod record_marker_derive;
#[path = "registry_core/resource_close_consume.rs"]
mod resource_close_consume;
#[path = "registry_core/supervisor_check.rs"]
mod supervisor_check;
#[path = "registry_core/user_generic_type_decl_checker_test.rs"]
mod user_generic_type_decl_checker_test;
#[path = "registry_core/veciter_intoiterator_surface.rs"]
mod veciter_intoiterator_surface;
#[path = "registry_core/width_conversion.rs"]
mod width_conversion;
