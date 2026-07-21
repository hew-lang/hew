// Domain harness for hew-types `actor` tests.
// Add new actor-behaviour tests under `tests/actor/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

// Declared once here (not per-leaf) because two or more leaves in this
// harness use `tests/common/`; declaring it in each leaf would load the
// same file multiple times in one binary (clippy::duplicate_mod). Leaves
// reference it via `use crate::common;`.
#[path = "common/mod.rs"]
mod common;

#[path = "actor/actor_blocking_warn.rs"]
mod actor_blocking_warn;
#[path = "actor/actor_identity_qualified.rs"]
mod actor_identity_qualified;
#[path = "actor/actor_init_typecheck.rs"]
mod actor_init_typecheck;
#[path = "actor/actor_lifecycle_hooks.rs"]
mod actor_lifecycle_hooks;
#[path = "actor/actor_method_dispatch.rs"]
mod actor_method_dispatch;
#[path = "actor/actor_protocol_descriptor.rs"]
mod actor_protocol_descriptor;
#[path = "actor/actor_state_lock_contract.rs"]
mod actor_state_lock_contract;
#[path = "actor/generic_actor_spawn_checker.rs"]
mod generic_actor_spawn_checker;
