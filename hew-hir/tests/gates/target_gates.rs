//! Tests for P0 target architecture gates (slepp A222 fail-closed).
//!
//! P0.1/P0.2: Coroutine gates reject actors/supervisors on unsupported targets.

use hew_hir::{HirDiagnosticKind, TargetArch};

use crate::support;

fn lower(source: &str, target: TargetArch) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker_for_target(source, target)
}

#[test]
fn target_coroutine_unsupported_rejects_actor_decl() {
    let source = r"
        actor Counter {
            var count: int = 0;
            receive fn increment() { self.count += 1; }
        }
    ";
    let output = lower(source, TargetArch::Other);

    assert!(
        !output.diagnostics.is_empty(),
        "Expected diagnostic for actor on unsupported target, got: {:?}",
        output.diagnostics
    );
    let has_coroutine_unsupported = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::TargetCoroutineUnsupported { .. }));
    assert!(
        has_coroutine_unsupported,
        "Expected TargetCoroutineUnsupported diagnostic, got: {:?}",
        output.diagnostics
    );

    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() should fail with unsupported target gate"
    );
}

#[test]
fn target_coroutine_unsupported_rejects_supervisor_decl() {
    let source = r"
        supervisor Root {
            strategy: one_for_one;
            child worker: WorkerActor();
        }
        actor WorkerActor {
            receive fn work() {}
        }
    ";
    let output = lower(source, TargetArch::Wasm32);

    let coroutine_diagnostics: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, HirDiagnosticKind::TargetCoroutineUnsupported { .. }))
        .collect();

    assert!(
        !coroutine_diagnostics.is_empty(),
        "Expected at least one coroutine gate diagnostic"
    );

    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() should fail with unsupported target gate"
    );
}

#[test]
fn target_coroutine_unsupported_accepts_pure_data_program() {
    let source = r"
        pub type Point { x: int; y: int }
        pub fn distance(p1: Point, p2: Point) -> int {
            let dx = p2.x - p1.x;
            let dy = p2.y - p1.y;
            dx * dx + dy * dy
        }
    ";
    let output = lower(source, TargetArch::Other);

    let has_coroutine_unsupported = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::TargetCoroutineUnsupported { .. }));
    assert!(
        !has_coroutine_unsupported,
        "Pure data program should not trigger coroutine gate"
    );

    assert!(
        !output
            .diagnostics
            .iter()
            .any(|d| { matches!(d.kind, HirDiagnosticKind::TargetCoroutineUnsupported { .. }) }),
        "Pure data program should not have target-gate errors"
    );
    let result = output.into_result();
    assert!(
        result.is_ok()
            || result.as_ref().err().is_some_and(|errs| {
                !errs
                    .iter()
                    .any(|d| matches!(d.kind, HirDiagnosticKind::TargetCoroutineUnsupported { .. }))
            }),
        "Pure data program should pass or fail for non-target reasons"
    );
}

#[test]
fn native_target_accepts_actors() {
    let source = r"
        actor Counter {
            var count: int = 0;
            receive fn increment() { self.count += 1; }
        }
    ";
    let output = lower(source, TargetArch::X86_64);

    let has_coroutine_unsupported = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::TargetCoroutineUnsupported { .. }));
    assert!(!has_coroutine_unsupported, "x86_64 should support actors");
}

#[test]
fn aarch64_target_accepts_actors() {
    let source = r"
        actor Counter {
            var count: int = 0;
            receive fn increment() { self.count += 1; }
        }
    ";
    let output = lower(source, TargetArch::Aarch64);

    let has_coroutine_unsupported = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::TargetCoroutineUnsupported { .. }));
    assert!(!has_coroutine_unsupported, "aarch64 should support actors");
}

// ── P0.3/P0.4: wasm32 blocking channel recv gate ────────────────────────────

#[test]
fn wasm_blocking_channel_recv_rejected() {
    // A program that calls `.recv()` on a channel-like receiver must be
    // rejected at HIR-lower time when compiling for wasm32, because the
    // runtime blocking-recv path is `unreachable!()` on that target.
    let source = r"
        fn main() {
            let v = ch.recv();
        }
    ";
    let output = lower(source, TargetArch::Wasm32);

    let has_blocking_recv = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::BlockingChannelRecvUnsupportedOnWasm { .. }
        )
    });
    assert!(
        has_blocking_recv,
        "wasm32 + .recv() must emit BlockingChannelRecvUnsupportedOnWasm; \
         got: {:#?}",
        output.diagnostics
    );

    // Fail-closed boundary: into_result() must return Err.
    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err when BlockingChannelRecvUnsupportedOnWasm \
         is present"
    );
}

#[test]
fn wasm_try_recv_accepted() {
    // `.try_recv()` is the non-blocking variant and must NOT be rejected on
    // wasm32 — it does not suspend and the runtime path is available.
    let source = r"
        fn main() {
            let v = ch.try_recv();
        }
    ";
    let output = lower(source, TargetArch::Wasm32);

    let has_blocking_recv = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::BlockingChannelRecvUnsupportedOnWasm { .. }
        )
    });
    assert!(
        !has_blocking_recv,
        ".try_recv() on wasm32 must NOT emit BlockingChannelRecvUnsupportedOnWasm; \
         got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn native_blocking_channel_recv_accepted() {
    // The same `.recv()` call must compile cleanly on native (x86_64) — the
    // gate is wasm32-only.
    let source = r"
        fn main() {
            let v = ch.recv();
        }
    ";
    let output = lower(source, TargetArch::X86_64);

    let has_blocking_recv = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::BlockingChannelRecvUnsupportedOnWasm { .. }
        )
    });
    assert!(
        !has_blocking_recv,
        ".recv() on x86_64 must NOT emit BlockingChannelRecvUnsupportedOnWasm; \
         got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn wasm_blocking_recv_nested_in_block_rejected() {
    // Ensure the walker descends into nested expressions, not just top-level.
    let source = r"
        fn main() {
            if true {
                for x in items {
                    let v = ch.recv();
                }
            }
        }
    ";
    let output = lower(source, TargetArch::Wasm32);

    let has_blocking_recv = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::BlockingChannelRecvUnsupportedOnWasm { .. }
        )
    });
    assert!(
        has_blocking_recv,
        "nested .recv() inside if/for must be detected; got: {:#?}",
        output.diagnostics
    );
}
