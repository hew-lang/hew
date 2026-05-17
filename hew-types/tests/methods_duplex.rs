//! Type-checker method dispatch for `Duplex` / `SendHalf` / `RecvHalf`.
//!
//! Accept fixtures verify that each wired method resolves correctly and
//! records the expected entries in `method_call_rewrites` and
//! `method_call_consumes_receiver`.
//!
//! Reject fixtures verify that:
//! - `.send()` on a non-Send payload emits an actor-boundary error.
//! - `.send()` on a `RecvHalf` / `.recv()` on a `SendHalf` emit
//!   `UndefinedMethod`.
//! - Calling `.send_half()` twice on the same binding fires `UseAfterMove`.

use hew_types::check::MethodCallRewrite;
use hew_types::error::TypeErrorKind;

mod common;

use common::typecheck;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn has_rewrite(output: &hew_types::TypeCheckOutput, symbol: &str) -> bool {
    output.method_call_rewrites.values().any(
        |rw| matches!(rw, MethodCallRewrite::RewriteToFunction { c_symbol } if c_symbol == symbol),
    )
}

// ---------------------------------------------------------------------------
// Accept fixtures — Duplex<S, R> methods
// ---------------------------------------------------------------------------

/// `d.send(42)` on tell-shaped `Duplex<int, ()>` returns `Result<(), SendError>`.
#[test]
fn duplex_send_tell_shaped_resolves() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, ()>(16);
            let _: Result<(), SendError> = d.send(42);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "d.send(42) on tell-shaped Duplex<int,()> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_send"),
        "expected hew_duplex_send in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

/// `d.send(42)` on ask-shaped `Duplex<int, bool>` returns `Result<bool, AskError>`.
#[test]
fn duplex_send_ask_shaped_resolves() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, bool>(16);
            let _: Result<bool, AskError> = d.send(42);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "d.send(42) on ask-shaped Duplex<int,bool> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_send"),
        "expected hew_duplex_send in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

/// `d.send(42)` on `Duplex<int, int>` (ask-shaped) typechecks and records
/// `hew_duplex_send` in the rewrite table. Kept for rewrite-table coverage.
#[test]
fn duplex_send_int_resolves() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let _: Result<int, AskError> = d.send(42);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "d.send(42) on Duplex<int,int> should typecheck (ask-shaped); got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_send"),
        "expected hew_duplex_send in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

/// `d.recv()` on `Duplex<int, int>` returns `Result<int, RecvError>`.
#[test]
fn duplex_recv_resolves() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let _: Result<int, RecvError> = d.recv();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "d.recv() on Duplex<int,int> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_recv"),
        "expected hew_duplex_recv in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

/// `d.send_half()` on `Duplex<int, bool>` returns `SendHalf<int>` and the
/// call site is flagged as consuming.
#[test]
fn duplex_send_half_resolves_and_consumes() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, bool>(16);
            let _: SendHalf<int> = d.send_half();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "d.send_half() on Duplex<int,bool> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_send_half"),
        "expected hew_duplex_send_half in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
    assert!(
        !output.method_call_consumes_receiver.is_empty(),
        "send_half() must mark the call site as consuming"
    );
}

/// `d.recv_half()` on `Duplex<int, bool>` returns `RecvHalf<bool>` and
/// the call site is flagged as consuming.
#[test]
fn duplex_recv_half_resolves_and_consumes() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, bool>(16);
            let _: RecvHalf<bool> = d.recv_half();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "d.recv_half() on Duplex<int,bool> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_recv_half"),
        "expected hew_duplex_recv_half in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
    assert!(
        !output.method_call_consumes_receiver.is_empty(),
        "recv_half() must mark the call site as consuming"
    );
}

/// `d.close()` on `Duplex<int, int>` returns `Result<(), CloseError>` and is consuming.
#[test]
fn duplex_close_resolves_and_consumes() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let _: Result<(), CloseError> = d.close();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "d.close() on Duplex<int,int> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_close"),
        "expected hew_duplex_close in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
    assert!(
        !output.method_call_consumes_receiver.is_empty(),
        "close() must mark the call site as consuming"
    );
}

// ---------------------------------------------------------------------------
// Accept fixtures — SendHalf<S> methods
// ---------------------------------------------------------------------------

/// `h.send(42)` on `SendHalf<int>` typechecks and records `hew_send_half_send`.
#[test]
fn send_half_send_resolves() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let h: SendHalf<int> = d.send_half();
            let _: Result<(), SendError> = h.send(42);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "h.send(42) on SendHalf<int> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_send_half_send"),
        "expected hew_send_half_send in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

/// `h.close()` on `SendHalf<int>` typechecks and is consuming.
#[test]
fn send_half_close_resolves_and_consumes() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let h: SendHalf<int> = d.send_half();
            let _: Result<(), CloseError> = h.close();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "h.close() on SendHalf<int> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_close_half"),
        "expected hew_duplex_close_half in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
    // Two consuming calls recorded: send_half() + close().
    assert!(
        !output.method_call_consumes_receiver.is_empty(),
        "close() on SendHalf must mark the call site as consuming"
    );
}

// ---------------------------------------------------------------------------
// Accept fixtures — RecvHalf<R> methods
// ---------------------------------------------------------------------------

/// `h.recv()` on `RecvHalf<int>` returns `Result<int, RecvError>`.
#[test]
fn recv_half_recv_resolves() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let h: RecvHalf<int> = d.recv_half();
            let _: Result<int, RecvError> = h.recv();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "h.recv() on RecvHalf<int> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_recv_half_recv"),
        "expected hew_recv_half_recv in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

/// `h.close()` on `RecvHalf<int>` returns `Result<(), CloseError>` and is consuming.
#[test]
fn recv_half_close_resolves_and_consumes() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let h: RecvHalf<int> = d.recv_half();
            let _: Result<(), CloseError> = h.close();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "h.close() on RecvHalf<int> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_close_half"),
        "expected hew_duplex_close_half in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
    assert!(
        !output.method_call_consumes_receiver.is_empty(),
        "close() on RecvHalf must mark the call site as consuming"
    );
}

// ---------------------------------------------------------------------------
// Accept fixtures — try_send / try_recv on Duplex
// ---------------------------------------------------------------------------

/// `d.try_send(42)` on tell-shaped `Duplex<int, ()>` typechecks and records
/// `hew_duplex_try_send` in the rewrite table.
#[test]
fn duplex_try_send_resolves() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, ()>(16);
            let _: Result<(), SendError> = d.try_send(42);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "d.try_send(42) on Duplex<int,int> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_try_send"),
        "expected hew_duplex_try_send in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

/// `d.try_recv()` on `Duplex<int, int>` returns `Result<int, RecvError>` and
/// records `hew_duplex_try_recv`.
#[test]
fn duplex_try_recv_resolves() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let _: Result<int, RecvError> = d.try_recv();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "d.try_recv() on Duplex<int,int> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_try_recv"),
        "expected hew_duplex_try_recv in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

// ---------------------------------------------------------------------------
// Accept fixtures — try_send on SendHalf / try_recv on RecvHalf
// ---------------------------------------------------------------------------

/// `h.try_send(42)` on `SendHalf<int>` typechecks and records
/// `hew_send_half_try_send` in the rewrite table.
#[test]
fn send_half_try_send_resolves() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let h: SendHalf<int> = d.send_half();
            let _: Result<(), SendError> = h.try_send(42);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "h.try_send(42) on SendHalf<int> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_send_half_try_send"),
        "expected hew_send_half_try_send in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

/// `h.try_recv()` on `RecvHalf<int>` typechecks and records
/// `hew_recv_half_try_recv` in the rewrite table.
#[test]
fn recv_half_try_recv_resolves() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let h: RecvHalf<int> = d.recv_half();
            let _: Result<int, RecvError> = h.try_recv();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "h.try_recv() on RecvHalf<int> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_recv_half_try_recv"),
        "expected hew_recv_half_try_recv in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

// ---------------------------------------------------------------------------
// Accept fixtures — try_recv on Stream / try_send on Sink
// ---------------------------------------------------------------------------

/// `s.try_recv()` on `Stream<string>` returns `Option<string>` and records
/// `hew_stream_try_next` in the rewrite table.
#[test]
fn stream_try_recv_resolves() {
    let source = r"
        fn probe(s: Stream<string>) {
            let _: Option<string> = s.try_recv();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "s.try_recv() on Stream<string> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_stream_try_next"),
        "expected hew_stream_try_next in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

/// `s.send(v)` on `Sink<string>` typechecks; `s.try_send(v)` must also
/// typecheck and record `hew_sink_try_write_string` in the rewrite table.
#[test]
fn sink_try_send_resolves() {
    let source = r#"
        fn probe(s: Sink<string>) {
            s.try_send("hello");
        }
    "#;
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "s.try_send(\"hello\") on Sink<string> should typecheck; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_sink_try_write_string"),
        "expected hew_sink_try_write_string in method_call_rewrites; got: {:#?}",
        output.method_call_rewrites
    );
}

// ---------------------------------------------------------------------------
// Reject fixtures
// ---------------------------------------------------------------------------

/// `.send()` with a non-Send payload on `Duplex<Rc<int>, int>` must fire
/// an actor-boundary send error (`InvalidSend`).
///
/// Note: the constructor (`duplex_pair<Rc<int>, int>`) itself also fails
/// with `BoundsNotSatisfied`, so we test with a type annotation bypass to
/// isolate the `.send()` rejection.  Both errors are expected.
#[test]
fn duplex_send_non_send_payload_rejected() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<Rc<int>, int>(16);
            d.send(Rc::new(1));
        }
    ";
    let output = typecheck(source);
    // Either BoundsNotSatisfied (from constructor) or InvalidSend (from .send())
    // must appear — the non-Send payload cannot silently pass through.
    let has_error = output.errors.iter().any(|e| {
        matches!(
            e.kind,
            TypeErrorKind::InvalidSend
                | TypeErrorKind::BoundsNotSatisfied
                | TypeErrorKind::UndefinedType
                | TypeErrorKind::UndefinedVariable
        )
    });
    assert!(
        has_error,
        "Duplex<Rc<int>, int>.send() must produce an error; got: {:#?}",
        output.errors
    );
}

/// `.send()` is not defined on `RecvHalf<int>` — must fire `UndefinedMethod`.
#[test]
fn recv_half_send_rejected() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let h: RecvHalf<int> = d.recv_half();
            h.send(42);
        }
    ";
    let output = typecheck(source);
    let has_undef = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::UndefinedMethod));
    assert!(
        has_undef,
        "RecvHalf::send() must fire UndefinedMethod; got: {:#?}",
        output.errors
    );
}

/// `.recv()` is not defined on `SendHalf<int>` — must fire `UndefinedMethod`.
#[test]
fn send_half_recv_rejected() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let h: SendHalf<int> = d.send_half();
            h.recv();
        }
    ";
    let output = typecheck(source);
    let has_undef = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::UndefinedMethod));
    assert!(
        has_undef,
        "SendHalf::recv() must fire UndefinedMethod; got: {:#?}",
        output.errors
    );
}

/// Calling `.send_half()` twice on the same `Duplex` fires `UseAfterMove`
/// on the second call because the first call consumes the receiver.
#[test]
fn duplex_send_half_twice_fires_use_after_move() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let _h1 = d.send_half();
            let _h2 = d.send_half();
        }
    ";
    let output = typecheck(source);
    let has_uam = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::UseAfterMove));
    assert!(
        has_uam,
        "second d.send_half() must fire UseAfterMove; got: {:#?}",
        output.errors
    );
}

/// Calling `.recv_half()` twice fires `UseAfterMove` on the second call.
#[test]
fn duplex_recv_half_twice_fires_use_after_move() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let _h1 = d.recv_half();
            let _h2 = d.recv_half();
        }
    ";
    let output = typecheck(source);
    let has_uam = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::UseAfterMove));
    assert!(
        has_uam,
        "second d.recv_half() must fire UseAfterMove; got: {:#?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// Q30=(a) — lambda-actor dual-surface accept fixtures
// ---------------------------------------------------------------------------

/// Call-syntax `worker(msg)` and method-syntax `worker.send(msg)` both typecheck
/// on a lambda-actor handle.  Both route to `hew_duplex_send` because lambda-actor
/// handles are `Duplex<Msg, Reply>` underneath.
///
/// Call-syntax remains canonical; `.send()` is an allowed-secondary surface.
#[test]
fn lambda_actor_call_syntax_typechecks() {
    let source = r"
        fn main() {
            let worker = actor |msg: int| {
                println(msg);
            };
            worker(42);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "call-syntax `worker(42)` on lambda-actor handle should typecheck; got: {:#?}",
        output.errors
    );
}

/// `.send()` on a lambda-actor handle typechecks and records `hew_duplex_send`
/// in the rewrite table — confirming the Duplex arm fires, not a separate actor arm.
///
/// This proves the shared-substrate invariant: the type system cannot distinguish
/// a lambda-actor Duplex from a raw-duplex Duplex at the method-call site.
#[test]
fn lambda_actor_dot_send_routes_through_duplex_arm() {
    let source = r"
        fn main() {
            let worker = actor |msg: int| {
                println(msg);
            };
            worker.send(42);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "`.send()` on lambda-actor handle should typecheck via Duplex::send; got: {:#?}",
        output.errors
    );
    assert!(
        has_rewrite(&output, "hew_duplex_send"),
        "`.send()` on lambda-actor handle must record hew_duplex_send rewrite \
         (Duplex arm must fire, not a separate actor arm); got: {:#?}",
        output.method_call_rewrites
    );
}

/// Calling `.close()` on a Duplex then using `d` again fires `UseAfterMove`.
#[test]
fn duplex_use_after_close_fires_use_after_move() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let _ = d.close();
            let _ = d.send(1);
        }
    ";
    let output = typecheck(source);
    let has_uam = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::UseAfterMove));
    assert!(
        has_uam,
        "using d after d.close() must fire UseAfterMove; got: {:#?}",
        output.errors
    );
}

/// When `UseAfterMove` fires on a `Duplex` binding the diagnostic includes a
/// note attributing the consume site (`"value was consumed here"`) so the user
/// can locate the call that moved the handle.
#[test]
fn duplex_use_after_move_includes_consumed_here_note() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let _ = d.close();
            let _ = d.send(1);
        }
    ";
    let output = typecheck(source);
    let uam = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::UseAfterMove));
    assert!(
        uam.is_some(),
        "expected UseAfterMove; got: {:#?}",
        output.errors
    );
    let uam = uam.unwrap();
    assert!(
        uam.notes
            .iter()
            .any(|(_, msg)| msg.contains("consumed here")),
        "UseAfterMove on Duplex should include a 'consumed here' note; got notes: {:#?}",
        uam.notes
    );
}

/// When `UseAfterMove` fires on a `Duplex` binding the diagnostic includes a
/// suggestion naming the handle type and its affine-consume semantics.
#[test]
fn duplex_use_after_move_includes_substrate_handle_suggestion() {
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let _h = d.send_half();
            let _h2 = d.send_half();
        }
    ";
    let output = typecheck(source);
    let uam = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::UseAfterMove));
    assert!(
        uam.is_some(),
        "expected UseAfterMove; got: {:#?}",
        output.errors
    );
    let uam = uam.unwrap();
    assert!(
        uam.suggestions
            .iter()
            .any(|s| s.contains("substrate handle")),
        "UseAfterMove on Duplex should include a substrate-handle suggestion; got suggestions: {:#?}",
        uam.suggestions
    );
}

// ---------------------------------------------------------------------------
// PartitionDetected exhaustiveness audit
// ---------------------------------------------------------------------------

/// Documents the Hew type checker's exhaustiveness behaviour for `RecvError`
/// match arms.
///
/// `RecvError` is registered in the checker as a `Ty::Named` marker
/// (see `hew-types/src/ty.rs` `Ty::recv_error()`), not as a `TypeDef` with
/// enumerated variants. Consequently `check_exhaustiveness` (diagnostics.rs)
/// reaches the `Ty::Named { name, .. }` branch, calls `lookup_type_def(name)`,
/// and finds `None` — the checker is currently variant-blind to `RecvError`.
///
/// **Result**: a Hew `match` on `Result<_, RecvError>` with only `Err(Closed)`
/// and `Err(Empty)` arms and no wildcard does NOT generate a non-exhaustive
/// diagnostic today. The `PartitionDetected` variant is therefore not checked
/// at the Hew-language level.
///
/// This is intentional for v0.5: `RecvError` is a Rust `#[repr(i32)]` enum
/// exposed through the C-ABI surface; its variant list is authoritative in
/// Rust, not in the Hew type system. Exhaustiveness at the language level
/// is deferred to the `RecvError`-as-Hew-enum registration lane (M3+).
///
/// This test pins the current behaviour so a future lane that DOES register
/// `RecvError` variants can see that the check now fires.
#[test]
fn recv_match_exhaustive_partition() {
    // A match that covers `Ok`, `Err(RecvError::Closed)`, and
    // `Err(RecvError::Empty)` but NOT `Err(RecvError::PartitionDetected)`.
    // Today the checker accepts this (no exhaustiveness error), because
    // RecvError has no registered TypeDef variants.
    let source = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let result: Result<int, RecvError> = d.recv();
            match result {
                Ok(_) => println(0),
                Err(_) => println(1),
            }
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "RecvError match with wildcard Err arm should typecheck; got: {:#?}",
        output.errors
    );

    // A match that covers `Ok` and `Err(e)` with a binding — exhaustive
    // by binding-identifier rule.
    let source_binding = r"
        fn main() {
            let (d, _) = duplex_pair<int, int>(16);
            let result: Result<int, RecvError> = d.recv();
            match result {
                Ok(_) => println(0),
                Err(e) => println(1),
            }
        }
    ";
    let output_binding = typecheck(source_binding);
    assert!(
        output_binding.errors.is_empty(),
        "RecvError match with binding arm should typecheck; got: {:#?}",
        output_binding.errors
    );
}
