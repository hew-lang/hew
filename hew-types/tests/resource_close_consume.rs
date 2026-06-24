//! Checker-level tests for `#[resource]` inherent-`close` consume semantics
//! (issue #1295).
//!
//! A `#[resource]` type's inherent `fn close(self)` is BOTH the implicit-drop
//! dispatch target (W3.030) AND a terminal consuming method: calling it moves
//! the receiver, so a subsequent use is a compile error and the scope-exit
//! implicit drop is suppressed on the consumed path. These tests verify the
//! checker wires the consume marker (the move-checker mark + the per-call-site
//! `method_call_consumes_receiver` flag HIR reads to lower the receiver with
//! `IntentKind::Consume`).

mod common;

use common::typecheck as typecheck_inline;

// NOTE on the rejection layer: a `#[resource]` struct whose fields are all
// `Copy` (e.g. `Conn { id: i64 }`) is structurally `Copy` from the checker's
// marker view, so the checker's `mark_expr_moved_if_non_copy` is a no-op and no
// checker-level `UseAfterMove` fires. The ownership discipline for `#[resource]`
// types is enforced by the MIR value-class (`AffineResource`): the consume flag
// recorded here makes HIR lower the receiver with `IntentKind::Consume`, and the
// MIR move-checker rejects the use-after-close with `UseAfterConsume`. The
// end-to-end rejection + drop-suppression is proved in
// `hew-mir/tests/vertical.rs`; these checker tests pin the side-table fact HIR
// reads.

/// The per-call-site flag in `method_call_consumes_receiver` is recorded for a
/// `#[resource]` inherent `close()` call. HIR reads this side-table fact to
/// lower the receiver with `IntentKind::Consume`, which suppresses the
/// duplicate scope-exit implicit drop.
#[test]
fn resource_inherent_close_records_per_call_site_flag() {
    let output = typecheck_inline(
        r"
        #[resource]
        pub type Conn { id: i64; }

        impl Conn {
            fn close(self) {}
        }

        fn main() {
            let c = Conn { id: 1 };
            c.close();
        }
        ",
    );
    assert!(
        !output.method_call_consumes_receiver.is_empty(),
        "expected the `#[resource]` close() call site flagged in \
         method_call_consumes_receiver; got {:?}",
        output.method_call_consumes_receiver,
    );
}

/// A single `close()` call on a `#[resource]` type type-checks cleanly — the
/// move-checker marks the receiver moved but there is no second use.
#[test]
fn resource_inherent_single_close_typechecks_cleanly() {
    let output = typecheck_inline(
        r"
        #[resource]
        pub type Conn { id: i64; }

        impl Conn {
            fn close(self) {}
        }

        fn main() {
            let c = Conn { id: 1 };
            c.close();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected a clean typecheck for a single `#[resource]` close(); got: {:?}",
        output.errors,
    );
}

/// A non-`#[resource]` type's inherent `close(self)` does NOT register into the
/// consume set keyed on the resource marker. The double-free this fix targets
/// only manifests for the `#[resource]` implicit-drop path; an ordinary type's
/// `close` is a plain by-value method (its receiver still moves as a call
/// argument, but no per-call-site consume flag is recorded for the resource
/// drop-suppression path).
#[test]
fn non_resource_inherent_close_not_in_resource_consume_set() {
    let output = typecheck_inline(
        r"
        pub type Plain { id: i64; }

        impl Plain {
            fn close(self) {}
        }

        fn main() {
            let p = Plain { id: 1 };
            p.close();
        }
        ",
    );
    assert!(
        output.method_call_consumes_receiver.is_empty(),
        "a non-`#[resource]` inherent close() must not record a resource \
         consume-receiver flag; got {:?}",
        output.method_call_consumes_receiver,
    );
}
