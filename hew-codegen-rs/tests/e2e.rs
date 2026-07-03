// Domain harness for hew-codegen-rs `e2e` tests.
// Add new e2e-behaviour tests under `tests/e2e/`, wire them into this
// file via #[path] — do not create a new top-level tests/*.rs file.

#[path = "e2e/actor_e2e.rs"]
mod actor_e2e;
#[path = "e2e/select_stream_e2e.rs"]
mod select_stream_e2e;
