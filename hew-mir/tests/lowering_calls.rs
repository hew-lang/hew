// Domain harness for hew-mir `lowering_calls` tests.
// Add new lowering_calls-behaviour tests under `tests/lowering_calls/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "lowering_calls/borrow_param_lowering.rs"]
mod borrow_param_lowering;
#[path = "lowering_calls/bytes_runtime_call_producer.rs"]
mod bytes_runtime_call_producer;
#[path = "lowering_calls/dyn_trait_dispatch.rs"]
mod dyn_trait_dispatch;
#[path = "lowering_calls/extern_string_ownership.rs"]
mod extern_string_ownership;
#[path = "lowering_calls/link_monitor_spawn_binding.rs"]
mod link_monitor_spawn_binding;
#[path = "lowering_calls/producer_call_runtime_abi.rs"]
mod producer_call_runtime_abi;
#[path = "lowering_calls/producer_drop_elaboration.rs"]
mod producer_drop_elaboration;
#[path = "lowering_calls/producer_method_send.rs"]
mod producer_method_send;
#[path = "lowering_calls/record_producer.rs"]
mod record_producer;
#[path = "lowering_calls/resolved_impl_call_family_dispatch.rs"]
mod resolved_impl_call_family_dispatch;
#[path = "lowering_calls/select_terminator_producer.rs"]
mod select_terminator_producer;
#[path = "lowering_calls/static_trait_dispatch_mir.rs"]
mod static_trait_dispatch_mir;
#[path = "lowering_calls/trait_default_method_mir.rs"]
mod trait_default_method_mir;
#[path = "lowering_calls/user_fn_call.rs"]
mod user_fn_call;
#[path = "lowering_calls/vec_resolved_impl_call.rs"]
mod vec_resolved_impl_call;
