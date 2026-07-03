// Domain harness for hew-hir `concurrency` tests.
// Add new concurrency-behaviour tests under `tests/concurrency/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

// Declared once here (not per-leaf) because two or more leaves in this
// harness use `tests/support/`; declaring it in each leaf would load the
// same file multiple times in one binary (clippy::duplicate_mod). Leaves
// reference it via `use crate::support;`.
#[path = "support/mod.rs"]
mod support;

#[path = "concurrency/actor_body.rs"]
mod actor_body;
#[path = "concurrency/actor_call_surface.rs"]
mod actor_call_surface;
#[path = "concurrency/actor_hir.rs"]
mod actor_hir;
#[path = "concurrency/actor_identity_carrier.rs"]
mod actor_identity_carrier;
#[path = "concurrency/actor_send_gates.rs"]
mod actor_send_gates;
#[path = "concurrency/actor_state_lock_lower.rs"]
mod actor_state_lock_lower;
#[path = "concurrency/cancellation_token_lower.rs"]
mod cancellation_token_lower;
#[path = "concurrency/spawn_qualified_identity.rs"]
mod spawn_qualified_identity;
#[path = "concurrency/supervisor_child_accessor_gates.rs"]
mod supervisor_child_accessor_gates;
#[path = "concurrency/supervisor_hir.rs"]
mod supervisor_hir;
#[path = "concurrency/supervisor_spawn_gates.rs"]
mod supervisor_spawn_gates;
#[path = "concurrency/task_gates.rs"]
mod task_gates;
#[path = "concurrency/task_gates_type_dependent.rs"]
mod task_gates_type_dependent;
