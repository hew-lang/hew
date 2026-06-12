#![cfg(not(target_arch = "wasm32"))]
#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI diagnostics property suite exercises raw C ABI reader functions"
)]

use std::ffi::CStr;
use std::ptr;

use hew_runtime::execution_context::{
    current_context, set_current_context, HewExecutionContext, EXECUTION_CONTEXT_NOT_INSTALLED,
    EXECUTION_CONTEXT_NOT_INSTALLED_AT_SPAWN,
};
use hew_runtime::{hew_clear_error, hew_last_error};
use proptest::prelude::*;

struct ContextResetGuard {
    previous: *mut HewExecutionContext,
}

impl ContextResetGuard {
    fn clear_current() -> Self {
        Self {
            previous: set_current_context(ptr::null_mut()),
        }
    }
}

impl Drop for ContextResetGuard {
    fn drop(&mut self) {
        let _ = set_current_context(self.previous);
        hew_clear_error();
    }
}

fn last_error_string() -> String {
    let ptr = hew_last_error();
    assert!(
        !ptr.is_null(),
        "ctx-null reader returned without a diagnostic"
    );
    unsafe { CStr::from_ptr(ptr) }
        .to_str()
        .expect("hew_last_error must be valid UTF-8")
        .to_owned()
}

fn assert_context_not_installed_error() {
    assert_eq!(last_error_string(), EXECUTION_CONTEXT_NOT_INSTALLED);
}

#[expect(
    clippy::too_many_lines,
    reason = "property catalog intentionally keeps every ctx-null reader case in one audited match"
)]
fn run_ctx_null_reader_case(case: u8) {
    let _guard = ContextResetGuard::clear_current();
    assert!(
        current_context().is_null(),
        "property case must start outside an execution context"
    );
    hew_clear_error();

    match case {
        0 => {
            assert_eq!(hew_runtime::actor::hew_actor_current_id(), -1);
            assert_context_not_installed_error();
        }
        1 => {
            assert!(hew_runtime::actor::hew_actor_self().is_null());
            assert_context_not_installed_error();
        }
        2 => {
            assert_eq!(hew_runtime::actor::hew_actor_self_pid(), 0);
            assert_context_not_installed_error();
        }
        3 => {
            hew_runtime::actor::hew_actor_self_stop();
            assert_context_not_installed_error();
        }
        4 => {
            assert_eq!(hew_runtime::scheduler::hew_actor_cooperate(), 0);
            assert_context_not_installed_error();
        }
        5 => {
            assert!(hew_runtime::arena::set_current_arena(ptr::null_mut()).is_null());
            assert_context_not_installed_error();
        }
        6 => {
            assert!(unsafe { hew_runtime::arena::hew_arena_malloc(16) }.is_null());
            assert_context_not_installed_error();
        }
        7 => {
            assert!(unsafe {
                hew_runtime::task_scope::hew_task_scope_set_current(ptr::null_mut())
            }
            .is_null());
            assert_context_not_installed_error();
        }
        8 => {
            let mut out = hew_runtime::tracing::HewTraceContext {
                trace_id_hi: u64::MAX,
                trace_id_lo: u64::MAX,
                span_id: u64::MAX,
                parent_span_id: u64::MAX,
                flags: u8::MAX,
            };
            unsafe { hew_runtime::tracing::hew_trace_get_context(&raw mut out) };
            assert_eq!(out.trace_id_hi, 0);
            assert_eq!(out.trace_id_lo, 0);
            assert_eq!(out.span_id, 0);
            assert_eq!(out.parent_span_id, 0);
            assert_eq!(out.flags, 0);
            assert_context_not_installed_error();
        }
        9 => {
            let trace = hew_runtime::tracing::HewTraceContext {
                trace_id_hi: 1,
                trace_id_lo: 2,
                span_id: 3,
                parent_span_id: 4,
                flags: 1,
            };
            unsafe { hew_runtime::tracing::hew_trace_set_context(&raw const trace) };
            assert_context_not_installed_error();
        }
        10 => {
            hew_runtime::tracing::hew_trace_enable(1);
            hew_runtime::tracing::hew_trace_begin(11, 12);
            hew_runtime::tracing::hew_trace_enable(0);
            assert_context_not_installed_error();
        }
        11 => {
            hew_runtime::tracing::hew_trace_enable(1);
            hew_runtime::tracing::hew_trace_end(11, 12);
            hew_runtime::tracing::hew_trace_enable(0);
            assert_context_not_installed_error();
        }
        12 => {
            hew_runtime::tracing::hew_trace_enable(1);
            hew_runtime::tracing::hew_trace_lifecycle(11, hew_runtime::tracing::SPAN_STOP);
            hew_runtime::tracing::hew_trace_enable(0);
            assert_context_not_installed_error();
        }
        13 => {
            unsafe extern "C" fn noop_context_task(
                _ctx: *mut HewExecutionContext,
                _task: *mut hew_runtime::task_scope::HewTask,
            ) {
            }

            let rc = unsafe {
                hew_runtime::task_scope::hew_task_spawn_thread_with_inherited_context(
                    ptr::null_mut(),
                    ptr::null_mut(),
                    noop_context_task,
                )
            };
            assert_eq!(rc, -1);
            assert_eq!(
                last_error_string(),
                EXECUTION_CONTEXT_NOT_INSTALLED_AT_SPAWN
            );
        }
        _ => unreachable!("case generator is bounded"),
    }
}

proptest! {
    #[test]
    fn ctx_null_readers_emit_typed_error(case in 0u8..14) {
        run_ctx_null_reader_case(case);
    }
}

#[test]
fn ctx_null_reader_catalog_covers_every_case() {
    for case in 0..14 {
        run_ctx_null_reader_case(case);
    }
}
