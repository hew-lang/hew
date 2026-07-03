// Domain harness for hew-mir `actor` tests.
// Add new actor-behaviour tests under `tests/actor/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "actor/actor_field_defaults.rs"]
mod actor_field_defaults;
#[path = "actor/actor_handler_lowering.rs"]
mod actor_handler_lowering;
#[path = "actor/actor_send_ask.rs"]
mod actor_send_ask;
#[path = "actor/actor_state_lock_lowering.rs"]
mod actor_state_lock_lowering;
#[path = "actor/cancellation_scope.rs"]
mod cancellation_scope;
#[path = "actor/cancellation_token_lower.rs"]
mod cancellation_token_lower;
#[path = "actor/context_carrier.rs"]
mod context_carrier;
#[path = "actor/supervisor_child_accessor.rs"]
mod supervisor_child_accessor;
#[path = "actor/supervisor_lifecycle_symbol.rs"]
mod supervisor_lifecycle_symbol;
#[path = "actor/supervisor_lowering.rs"]
mod supervisor_lowering;
#[path = "actor/task_entry_adapter_lowering.rs"]
mod task_entry_adapter_lowering;
