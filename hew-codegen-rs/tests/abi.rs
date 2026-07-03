// Domain harness for hew-codegen-rs `abi` tests.
// Add new abi-behaviour tests under `tests/abi/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "abi/abi_offset_parity.rs"]
mod abi_offset_parity;
#[path = "abi/actor_dispatch_borrow_abi.rs"]
mod actor_dispatch_borrow_abi;
#[path = "abi/cycle_capable_spawn_abi.rs"]
mod cycle_capable_spawn_abi;
#[path = "abi/dyn_trait_method_dispatch.rs"]
mod dyn_trait_method_dispatch;
#[path = "abi/machine_dispatch_codegen.rs"]
mod machine_dispatch_codegen;
#[path = "abi/runtime_abi_fail_closed.rs"]
mod runtime_abi_fail_closed;
