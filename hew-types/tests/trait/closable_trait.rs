//! Checker-level tests for the `Closable` trait and `CloseError` enum.
//!
//! These tests verify the end-to-end seam introduced by PR 2.0 of the RAII
//! rollout: `std/io/closable.hew` defines `trait Closable`, and loading it
//! via the normal stdlib path wires `Closable::close` into the
//! consume-receiver set so the move-checker marks the receiver moved after
//! every call.  Tests here use the stdlib-aware `checker()` helper so the
//! registration fires from trait-load, not from a direct
//! `register_consume_receiver_method` call.

use crate::common;

use common::typecheck as typecheck_inline;
use hew_types::error::TypeErrorKind;

// ---------------------------------------------------------------------------
// Commit 3 tests — consume marker fires via stdlib registration
// ---------------------------------------------------------------------------

/// Importing `std::io::closable` and implementing `Closable` for a user type
/// causes `UseAfterMove` on the second `close()` call.
///
/// This test does NOT call `register_consume_receiver_method` directly: it
/// relies on `register_stdlib_hew_items` wiring the flag when the `Closable`
/// trait is processed.  If the registration call in `registration.rs` is
/// removed, this test fails — that's the discriminator.
#[test]
fn closable_close_triggers_use_after_move_on_second_call() {
    let output = typecheck_inline(
        r#"
        import std::io::closable;

        type Widget { name: string; }

        impl Closable for Widget {
            fn close(w: Widget) -> Result<(), closable.CloseError> {
                Ok(())
            }
        }

        fn main() {
            let w = Widget { name: "x" };
            let _ = w.close();
            let _ = w.close();
        }
        "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UseAfterMove),
        "expected UseAfterMove on second close() after Closable is imported via stdlib; \
         got errors: {:?}",
        output.errors,
    );
}

/// A single `close()` call on a `Closable` implementor type-checks cleanly —
/// the move-checker marks the receiver moved but there is no second use.
#[test]
fn closable_single_close_typechecks_cleanly() {
    let output = typecheck_inline(
        r#"
        import std::io::closable;

        type Widget { name: string; }

        impl Closable for Widget {
            fn close(w: Widget) -> Result<(), closable.CloseError> {
                Ok(())
            }
        }

        fn main() {
            let w = Widget { name: "x" };
            let _ = w.close();
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck for single close() call; got: {:?}",
        output.errors,
    );
}

/// The per-call-site flag in `method_call_consumes_receiver` is populated for
/// a `Closable::close` call when the trait is loaded via the stdlib path.
#[test]
fn closable_close_records_per_call_site_flag_via_stdlib() {
    let output = typecheck_inline(
        r#"
        import std::io::closable;

        type Pipe { tag: string; }

        impl Closable for Pipe {
            fn close(p: Pipe) -> Result<(), closable.CloseError> {
                Ok(())
            }
        }

        fn main() {
            let p = Pipe { tag: "io" };
            let _ = p.close();
        }
        "#,
    );
    assert!(
        !output.method_call_consumes_receiver.is_empty(),
        "expected at least one call site flagged in method_call_consumes_receiver \
         after loading Closable via stdlib; got {:?}",
        output.method_call_consumes_receiver,
    );
}

// ---------------------------------------------------------------------------
// Commit 4 tests — process.Child Drop is scope-exit only
// ---------------------------------------------------------------------------

/// `process.Child` has `impl Drop` but the trait is NOT in the
/// consume-receiver set.  Calling `child.drop()` directly does not fire the
/// `UseAfterMove` diagnostic on a second use of `child`, confirming that
/// `Drop::drop` is scope-exit-only and not treated as a consuming method.
///
/// This is a regression guard: if `Drop::drop` were accidentally added to
/// `consume_receiver_methods`, all `impl Drop` types would become
/// mistakenly non-copyable after a `drop()` call expression, which is not
/// a user-callable surface.
///
/// Note: the checker may surface an "unknown method" error for `child.drop()`
/// if `drop` is not exposed as a user-callable method.  The test checks that
/// there is NO `UseAfterMove` on the second reference to `child` — regardless
/// of any other diagnostics produced.
#[test]
fn child_drop_not_in_consume_receiver_set() {
    let output = typecheck_inline(
        r#"
        import std::process;

        fn main() {
            let child = process.start("sleep 1");
            child.drop();
            let _ = child.kill();
        }
        "#,
    );
    // The critical assertion: `Drop::drop` must NOT trigger UseAfterMove on
    // a subsequent use of `child`.  The second `child.kill()` must remain
    // visible to the checker — if UseAfterMove fires here, the Drop trait was
    // incorrectly added to the consume set.
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UseAfterMove),
        "Drop::drop must not be in the consume-receiver set; \
         UseAfterMove must not fire on a second use after child.drop(); \
         got errors: {:?}",
        output.errors,
    );
}
