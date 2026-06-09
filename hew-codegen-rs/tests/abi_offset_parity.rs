//! Codegen↔runtime ABI offset parity.
//!
//! Codegen emits raw GEPs into the runtime's `HewActor` and
//! `HewExecutionContext` structs using hand-copied byte-offset literals
//! (`hew-codegen-rs/src/llvm.rs`) so the compiler backend does not link the
//! runtime crate. Those literals can silently desync from the real struct
//! layout: removing `HewActor.pid` shifted `state` from offset 24→16, but the
//! codegen literal stayed at 24, so generated handlers loaded `state_size`
//! (now at 24) as the state pointer and dereferenced a small integer →
//! SIGSEGV / actor-crash deadlock.
//!
//! The runtime derives the canonical offsets with `offset_of!`; this test
//! asserts every codegen literal equals its runtime-derived counterpart so the
//! drift fails closed at test time instead of corrupting actor memory at
//! runtime. Mirrors the `HEW_CTX_OFFSET_*` self-check discipline in
//! `hew-runtime/src/execution_context.rs` and the WASM↔native layout assert in
//! `scheduler_wasm.rs`.

use hew_codegen_rs::llvm;

/// `HewActor` field offsets codegen mirrors. The runtime values are
/// `offset_of!`-derived, so this guards against any field reorder in
/// `hew-runtime/src/actor.rs`.
#[test]
fn hew_actor_offset_parity() {
    assert_eq!(
        llvm::HEW_ACTOR_OFFSET_ID,
        hew_runtime::actor::HEW_ACTOR_OFFSET_ID,
        "codegen HEW_ACTOR_OFFSET_ID drifted from runtime offset_of!(HewActor, id)"
    );
    assert_eq!(
        llvm::HEW_ACTOR_OFFSET_STATE,
        hew_runtime::actor::HEW_ACTOR_OFFSET_STATE,
        "codegen HEW_ACTOR_OFFSET_STATE drifted from runtime offset_of!(HewActor, state)"
    );
}

/// `HewExecutionContext` field offsets codegen mirrors. Same drift class as the
/// actor offsets; guarded against any field reorder in
/// `hew-runtime/src/execution_context.rs`.
#[test]
fn hew_ctx_offset_parity() {
    assert_eq!(
        llvm::HEW_CTX_OFFSET_ACTOR,
        hew_runtime::HEW_CTX_OFFSET_ACTOR,
        "codegen HEW_CTX_OFFSET_ACTOR drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_ACTOR_ID,
        hew_runtime::HEW_CTX_OFFSET_ACTOR_ID,
        "codegen HEW_CTX_OFFSET_ACTOR_ID drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_PARENT_SUPERVISOR,
        hew_runtime::HEW_CTX_OFFSET_PARENT_SUPERVISOR,
        "codegen HEW_CTX_OFFSET_PARENT_SUPERVISOR drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX,
        hew_runtime::HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX,
        "codegen HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_FLAGS,
        hew_runtime::HEW_CTX_OFFSET_FLAGS,
        "codegen HEW_CTX_OFFSET_FLAGS drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_CANCEL_TOKEN,
        hew_runtime::HEW_CTX_OFFSET_CANCEL_TOKEN,
        "codegen HEW_CTX_OFFSET_CANCEL_TOKEN drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_TASK_SCOPE,
        hew_runtime::HEW_CTX_OFFSET_TASK_SCOPE,
        "codegen HEW_CTX_OFFSET_TASK_SCOPE drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_ARENA,
        hew_runtime::HEW_CTX_OFFSET_ARENA,
        "codegen HEW_CTX_OFFSET_ARENA drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_TRACE,
        hew_runtime::HEW_CTX_OFFSET_TRACE,
        "codegen HEW_CTX_OFFSET_TRACE drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_TRACE_SPAN,
        hew_runtime::HEW_CTX_OFFSET_TRACE_SPAN,
        "codegen HEW_CTX_OFFSET_TRACE_SPAN drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_PARTITION_POLICY,
        hew_runtime::HEW_CTX_OFFSET_PARTITION_POLICY,
        "codegen HEW_CTX_OFFSET_PARTITION_POLICY drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_PREV_CONTEXT,
        hew_runtime::HEW_CTX_OFFSET_PREV_CONTEXT,
        "codegen HEW_CTX_OFFSET_PREV_CONTEXT drifted from runtime"
    );
    assert_eq!(
        llvm::HEW_CTX_OFFSET_LOCK_SEAT,
        hew_runtime::HEW_CTX_OFFSET_LOCK_SEAT,
        "codegen HEW_CTX_OFFSET_LOCK_SEAT drifted from runtime"
    );
}
